# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)
library(anytime)
library(timetk)
library(janitor)

# Process -----------------------------------------------------------------
fname = here::here('data/ema.csv')

ema = data.table::fread(fname)

# select columns we need
ema = ema %>% 
  select(Idmaternal, Date, Trimester, 
         Day, finish.time, Location, 
         DayofWeek, Alone, CaffeinatedDrink, 
         StressedNumeric, PercievedStressScore, 
         SocialInteraction, QuestionType, GA)

# combine data and time into datetime object
ema$Date = format(as.Date(ema$Date, format='%m/%d/%Y'), '%Y-%m-%d')
ema$Date = gsub(pattern='00', replacement='20', x=ema$Date, fixed=TRUE)
ema$date_time = paste(ema$Date, ema$finish.time, sep=' ')
ema$date_time = as.POSIXct(ema$date_time, tz='UTC')
ema = select(ema, -c(Date, finish.time))

# get rid of irrelevant questions
ema  = ema %>% subset(QuestionType == 'Day')

# drop duplicate rows
ema = ema %>% 
  group_by(Idmaternal, date_time) %>% 
  filter(n() < 2)

# recode trimester to string code and call it visit
ema$visit = recode(ema$Trimester, `0`='G1', `1`='G2', `2`='G3')

# recode stress numeric into binary with cutoff at 1
ema$stressed_binary = ifelse(ema$StressedNumeric <= 1, 0, 1)
ema$pss_binary = ifelse(ema$PercievedStressScore < 2, 0, 1)

# code work binary
ema$work = ifelse(ema$Location == 'Work', 1, 0)

# code binary weekend or not
ema$weekend = ifelse(ema$DayofWeek %in% c('Sat', 'Sun'), 1, 0)

# get rid of garbage
ema = select(ema, -c(DayofWeek, Location, Trimester))

# clean names
ema = clean_names(ema)
names(ema)[names(ema) == 'idmaternal'] = 'id'
names(ema)[names(ema) == 'ga'] = 'gestational_age'
names(ema)[names(ema) == 'day'] = 'day_after_visit'

# Export ------------------------------------------------------------------
data.table::fwrite(ema, here::here(
  'processed_data/ema_done.csv'))
