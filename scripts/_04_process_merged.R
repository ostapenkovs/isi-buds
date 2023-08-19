# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(anytime)

# Data load ---------------------------------------------------------------
ema = data.table::fread(here::here('processed_data/ema_done.csv'))

interval = '30 seconds' #'30 seconds' #'1 minute'
act = data.table::fread(here::here(paste0('processed_data/processed_actiheart/', 
                                          str_replace(interval, ' ', '_'), 
                                          '_actiheart.csv')))

# Process -----------------------------------------------------------------
# approximate date time merge
setkey(act, id, date_time)
setkey(ema, id, date_time)

df = act[ema, roll=T]

# remove people that do not have two classes of entries for pss
people = df %>% group_by(id) %>% summarise(mean_pss_binary=mean(pss_binary))
people = people[people$mean_pss_binary %in% c(0, 1), 'id']
df = df[!(df$id %in% people$id), ]

# marge time invariant data (covariates)
cov = data.table::fread(here::here('processed_data/covariates_done.csv'))
df = merge(df, cov, by='id')

# drop dupes
df = na.omit(df)

# Export ------------------------------------------------------------------
data.table::fwrite(df, here::here(paste0('processed_data/', 
                                         str_replace(interval, ' ', '_'), 
                                         '_merged.csv')))
