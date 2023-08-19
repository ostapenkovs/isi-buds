library(tidyverse)
library(tidymodels)
library(data.table)
library(corrplot)
library(lme4)
library(umap)

train_test_split = function(df) {
  # visit numbers to loop through 
  visit_numbers = unique(df$visit)
  
  # creating two empty data frames to populate with loop
  train = data.frame(id=character(0),    
                     day_after_visit=double(0),
                     visit=character(0))
  test = data.frame(id=character(0),    
                    day_after_visit=double(0),
                    visit=character(0))
  
  # outer loop loops through visit numbers 
  for (x in visit_numbers) {
    visit = df %>% filter(visit == x)
    
    # ids to loop through 
    ids = unique(visit$id)
    
    # creating two empty data frames to populate with loop
    train_temp = data.frame(id=character(0),    
                            day_after_visit=double(0),
                            visit=character(0))
    test_temp = data.frame(id=character(0),    
                           day_after_visit=double(0),
                           visit=character(0))
    
    # inner loop loops through id numbers within a visit number
    for (i in ids) {
      # filtering to just that persons data
      temp = visit  %>% filter(id == i)
      
      # cut out participants who only have one day of data 
      if( max(temp$day_after_visit) == min(temp$day_after_visit )) {
        next
      }
      
      # take out last day as test and all others as train and append rows to empty data frames
      last_day = max(temp$day_after_visit)
      test_temp = rbind(test_temp,temp %>% filter(day_after_visit == last_day))
      train_temp = rbind(train_temp,temp %>% filter(day_after_visit < last_day))
    }
    
    # adding the current train and test data frames into final ones 
    train = rbind(train, train_temp)
    test = rbind(test,test_temp)
  }
  
  return(list(train=train, test=test))
}

transform_x_into_w = function(df, q, d) {
  # distributed lag paper transformation of Xs into Ws
  # q is maximum lag time period
  # d is maximum degree of polynomial we are fitting
  container = list()
  for(i in 0:d) {
    container[[ paste0('w', i) ]] = as.matrix(df) %*% (0:q)^i
  }
  return(as.data.frame(container))
}

forward_selection = function(train_df, to_search) {
  train_copy = train_df %>% copy()
  scaled_preds = scale( train_copy[, ..to_search] )
  train_copy[, (to_search) := as.data.table(scaled_preds)]
  
  best = c(to_search[1])
  temp_best = best
  
  init_formula = as.formula(paste0('pss_binary ~ -1 + ',
                                   paste0(best, collapse=' + '), ' + ',
                                   '(1 | id)'))
  init_fit = glmer(init_formula, 
                   data=train_copy, family=binomial(link='logit'),
                   control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
  min_aic = AIC(init_fit)
  
  done = FALSE
  while(!done) {
    for(i in setdiff(to_search, best)) { 
      formula_str = as.formula(paste0('pss_binary ~ -1 + ',
                                      paste0(c(best, i), collapse=' + '), ' + ',
                                      '(1 | id)'))
      this_fit = glmer(formula_str, 
                       data=train_copy, family=binomial(link='logit'),
                       control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
      
      this_aic = AIC(this_fit)
      
      if(this_aic < min_aic) {
        min_aic = this_aic
        temp_best = c(best, i)
        print(paste0('New min. AIC found: ', min_aic))
        print(paste0('With predictors: ', paste(temp_best, collapse=', ' )))
      }
    }
    if((length(setdiff(temp_best, best)) == 0)) {
      done = TRUE
    } else {
      best = temp_best
    }
  }
  
  return( list(best_preds=best, best_aic=min_aic) )
}

train_val_one_model = function(fold_train, fold_val, fixed, measure='rmssd', poly_deg=NA, n_lags=NA) {
  train_w = data.frame()
  val_w = data.frame()
  if(!(is.na(poly_deg) & is.na(n_lags))) {
    train_w = transform_x_into_w(df=select(fold_train, paste0(measure, '_ibi_lag_', 0:n_lags)), q=n_lags, d=poly_deg)
    val_w = transform_x_into_w(df=select(fold_val, paste0(measure, '_ibi_lag_', 0:n_lags)), q=n_lags, d=poly_deg)
  }
  fold_train = cbind(fold_train, train_w)
  fold_val = cbind(fold_val, val_w)
  
  vars_to_scale = c(paste0('w', 0:poly_deg), fixed)
  
  scaled_train = scale( fold_train[, ..vars_to_scale] )
  scaled_val = scale(fold_val[, ..vars_to_scale], center=attr(scaled_train, 'scaled:center'), 
                     scale=attr(scaled_train, 'scaled:scale'))
  
  fold_train[, (vars_to_scale) := as.data.table(scaled_train)]
  fold_val[, (vars_to_scale) := as.data.table(scaled_val)]
  
  return( list(train=fold_train, val=fold_val, center=attr(scaled_train, 'scaled:center'),
               scale=attr(scaled_train, 'scaled:scale')) )
}

evaluate_one_model = function(folds, fixed, random, measure='rmssd', poly_deg=NA, n_lags=NA) {
  metrics = list()
  
  poly_str = ''
  if(!(is.na(poly_deg) & is.na(n_lags))) {
    poly_str = paste0(paste0('w', 0:poly_deg, collapse=' + '), ' + ')
  }
  formula_str = as.formula(paste0('pss_binary ~ -1 + ',
                                  poly_str,
                                  paste0(fixed, collapse=' + '), ' + ', 
                                  paste0('(1 | ', random, ')')))
  
  for(i in 1:nrow(folds)) {
    print(paste0('# FOLD: ', i, ' #'))
    fold_train = analysis(folds$splits[[i]])
    fold_val = assessment(folds$splits[[i]])
    
    fold_container = train_val_one_model(fold_train=fold_train, fold_val=fold_val, 
                                         fixed=fixed, measure=measure, poly_deg=poly_deg, n_lags=n_lags)
    
    fold_train = fold_container$train
    fold_val = fold_container$val
    
    # train
    fold_train_fit = glmer(formula_str, 
                           data=fold_train, family=binomial(link='logit'),
                           control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))
    
    # validate
    fold_predictions = unname(predict(fold_train_fit, fold_val, type='response'))
    fold_binary_predictions = as.factor(ifelse(fold_predictions <= 0.5, 0, 1))
    fold_truth = fold_val$pss_binary
    
    # collect metrics (accuracy, roc/auc, aic, bic)
    fold_accuracy = accuracy_vec(truth=fold_truth, 
                                 estimate=fold_binary_predictions)
    fold_roc_auc = roc_auc_vec(truth=fold_truth, 
                               estimate=fold_predictions, 
                               event_level='second')
    fold_aic = AIC(fold_train_fit)
    fold_bic = BIC(fold_train_fit)
    
    metrics[[i]] = list(fold=i, accuracy=fold_accuracy, roc_auc=fold_roc_auc, 
                        aic=fold_aic, bic=fold_bic)
  }
  
  return( rbindlist(metrics) )
}

interval = '30 seconds' #'30 seconds' #'1 minute'
df = data.table::fread(here::here(paste0('processed_data/', 
                                         str_replace(interval, ' ', '_'), 
                                         '_merged.csv')))
df$pss_binary = as.factor(df$pss_binary)

container = train_test_split(df=df)
train_df = container$train
test_df = container$test

to_search = c("mean_activity", "mean_bpm", "var_ibi_lag_0", "alone", "caffeinated_drink", 
              "social_interaction", "gestational_age", "work", "weekend", 
              "maternal_age", "race_mom", "ethnicity_mom", "OBrisk_cat", 
              "bmi_prepreg_MaternalDemographics", "SES_2", "prior_children")

best_covs = forward_selection(train_df=train_df, to_search=to_search)

print(best_covs$best_preds)
print(best_covs$best_aic)

n_folds = 3
target_col = 'pss_binary'
folds = vfold_cv(train_df, v=n_folds, strata=all_of(target_col))

res_df = data.frame()
n_lags = c(5, 10 , 20)
poly_deg = c(2, 3)
measures = c('rmssd', 'mean')
for(n in n_lags) {
  for(d in poly_deg) {
    for(m in measures) {
      print(paste0('Models of degree: ', d, '; Measure: ', m, '; NLags: ', n))
      this_res = evaluate_one_model(folds=folds, fixed=best_covs$best_preds, 
                                    random=c('id'), measure=m, poly_deg=d, n_lags=c(n))
      this_res$deg = d
      this_res$measure = m
      this_res$lag = n
      res_df = rbind(res_df, this_res)
    }
  }
}

print(res_df)

final_container = train_val_one_model(fold_train=train_df, fold_val=test_df, 
                                      fixed=best_covs$best_preds, measure='rmssd', poly_deg=5, n_lags=5)

formula_str = as.formula(paste0('pss_binary ~ -1 + ',
                                paste0(paste0('w', 0:5, collapse=' + '), ' + '),
                                paste0(best_covs$best_preds, collapse=' + '), ' + ', 
                                paste0('(1 | id)')))

final_fit = glmer(formula_str, 
                  data=final_container$train, family=binomial(link='logit'),
                  control=glmerControl(optimizer='bobyqa', optCtrl=list(maxfun=2e5)))

final_predictions = unname(predict(final_fit, final_container$val, type='response'))
final_binary_predictions = as.factor(ifelse(final_predictions <= 0.5, 0, 1))
final_truth = final_container$val$pss_binary

final_accuracy = accuracy_vec(truth=final_truth, 
                              estimate=final_binary_predictions)
final_roc_auc = roc_auc_vec(truth=final_truth, 
                            estimate=final_predictions, 
                            event_level='second')

print(final_accuracy)
print(final_roc_auc)

pred_data = as.data.frame(list(pred=final_predictions, 
                               pred_binary=final_binary_predictions, 
                               true=final_truth))

pred_curve = roc_curve(data=pred_data, 
                       truth=true, 
                       estimate=pred, event_level='second')
autoplot(pred_curve)

conf_mtx = conf_mat(data=pred_data, truth=true, estimate=pred_binary)
print(conf_mtx)
heatmap(conf_mtx$table)

summary(final_fit)
