# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(stringr)
library(anytime)
library(timetk)

# Helpers -----------------------------------------------------------------
RMSSD = function(x) {
  # compute the RMSSD for a vector of values
  the_sum = 0
  n = length(x)
  for(i in 1:(n-1)) {
    the_sum = the_sum + (x[i+1] - x[i])^2
  }
  return(sqrt(the_sum/(n-1)))
}

ibi_metrics = function(x) {
  # work around way to compute mean, var, RMSSD for a string of values
  z = as.numeric(strsplit(x, ' ')[[1]])
  return(paste(mean(z), var(z), RMSSD(z)))
}


lagged = function(x, lag=1) {
  # return time-lagged vector
  if (lag == 0) {
    return(x)
  }
  n = length(x)
  if(n <= lag) {
    return( rep(NA, n) )
  } else {
    return( c(rep(NA, lag), x[-( (n-lag+1):n)]) )
  }
}

# Process -----------------------------------------------------------------
process_actiheart = function(fname_list, time_interval='1 minute', n_lags=3) {
  container = list()
  i = 1
  
  for(fname in fname_list) {
    split_name = strsplit(fname, '_', fixed=TRUE)
    id = as.numeric(split_name[[1]][[2]])
    
    if(id>=1001 & id<=1253) {
      visit = strsplit(split_name[[1]][[3]], '.', fixed=TRUE)
      visit = visit[[1]][[1]]
      
      data = data.table::fread(fname) %>%
        column_to_rownames('V1')
      
      data$date_time = anytime(data$date_time)
    
      data = unite(data, 'ibi_str', ibi_1:ibi_80, remove=TRUE, na.rm=TRUE,
                  sep=' ')
      
      data = select(data, date_time, activity, bpm, ibi_str)
  
      data = data %>%
        summarise_by_time(
          .date_var=date_time,
          .by=time_interval,
          mean_activity=mean(activity),
          mean_bpm=mean(bpm),
          all_ibi=ibi_metrics(ibi_str)
        )
    
      data[c('mean_ibi_lag_0', 'var_ibi_lag_0', 'rmssd_ibi_lag_0')] = 
        str_split_fixed(data$all_ibi, ' ', 3)
    
      data = subset(data, select=-c(all_ibi))
    
      data$id = id
      #data$visit = visit
      
      for(n in 1:lags) {
        data[paste0('rmssd_ibi_lag_', n)] = lagged(
          data$rmssd_ibi_lag_0, lag=n)
        
        data[paste0('mean_ibi_lag_', n)] = lagged(
          data$mean_ibi_lag_0, lag=n)
      }
    
      container[[i]] = data
      i = i + 1
    }
    
    if(i %% 5 == 0) {
      print(paste0(i, ' / ', length(fname_list)))
    }
  }
  
  return(rbindlist(container))
}

# Process and export ------------------------------------------------------
fnames = list.files(path=here::here('data/actiheart'), full.names=TRUE)
interval = '30 seconds'
lags = 20

df = process_actiheart(fname_list=fnames, time_interval=interval, n_lags=lags)

data.table::fwrite(df, here::here(
  paste0('processed_data/processed_actiheart/', 
         str_replace(interval, ' ', '_'), '_actiheart.csv')))
