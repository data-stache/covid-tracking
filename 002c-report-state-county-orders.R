# Load Libraries ---------------------------------------------------------------
{
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(tidylog)
}


load(file = 'rda/case_order_county.rda')
load(file = 'rda/death_order_county.rda')
load(file = 'rda/case_order_state.rda')
load(file = 'rda/death_order_state.rda')

head(case_order_county, 10)
head(case_order_state, 5)

STATE <- 'Delaware'

case_order_county %>%
  filter(state == STATE)

death_order_county %>%
  filter(state == STATE)
