# Libraries ---------------------------------------------------------------
library(tidyverse)
library(data.table)

# Process -----------------------------------------------------------------
fname = here::here('data/pstress_covariates.csv')

data = data.table::fread(fname)

# rename id column
names(data)[length(names(data))] = 'id'

# convert t0_parity to binary
data$prior_children = ifelse(data$t0_parity == 0, 0, 1)

# Export ------------------------------------------------------------------
data.table::fwrite(data, here::here(
  'processed_data/covariates_done.csv'))
