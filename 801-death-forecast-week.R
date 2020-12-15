load("rda/covid_us_sum.rda")
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(digits = 3)

library(tidyverse)
library(zoo)
library(lubridate)
library(tidylog)

deaths <- covid_us_sum %>%
  filter(date >= ymd(20201001)) %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(change = new_death / last(new_death)) %>%
  filter(!is.na(change) & !is.infinite(change)) %>%
  select(week, day, new_death, change) 

##### MODEL GUTS #####
dat_mod <- deaths %>% 
  group_by(day) %>% 
  summarize(mu = mean(change),
            sd = sd(change),
            Ns = n(),
            se = qnorm(.975) * sd / sqrt(Ns))

##### DATES #####
day <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")

actual <- map_df(day, function(d){
  covid_us_sum %>%
    group_by(day) %>%
    filter(day == d & date == max(date)) %>%
    select(new_death, date)
})

actual <- actual %>%
  mutate(actual = new_death) %>%
  select(date, actual) %>%
  ungroup

this_week <- dat_mod %>% 
  mutate(model = actual$actual[actual$day == 'Sunday'] * mu,
         high = actual$actual[actual$day == 'Sunday'] * (mu + se),
         low = actual$actual[actual$day == 'Sunday'] * (mu - se)) %>%
  left_join(actual) %>%
  mutate(actual = ifelse(date < date[1], NA, actual),
         date = as.Date(ifelse(date < date[1], date+7, date)),
         hit = ifelse(actual >= low & actual <= high, TRUE, FALSE),
         lo_miss = ifelse(hit == FALSE, ifelse(actual > high, NA, (actual - low) / low), NA),
         miss = ifelse(hit == FALSE, (actual - model) / model, NA),
         hi_miss = ifelse(hit == FALSE, ifelse(actual < low, NA, (actual - high) / high), NA)) %>%
  filter(!day == 'Sunday')

##### MODEL OUTPUT #####
library(knitr)
this_week %>%
  kable()

this_week %>%
  summarise(low_avg = mean(low),
            model_avg = mean(model),
            high_avg = mean(high),
            low_sum = sum(low),
            model_sum = sum(model),
            high_sum = sum(high)) %>%
  kable()

day_no_sun <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
# Graph of Model
P_this_week <- this_week %>%
  ggplot(aes(x = factor(day, levels = day_no_sun))) +
  geom_ribbon(aes(ymin = low, ymax = high, group = 1), fill = 'red', alpha = .3) +
  geom_line(aes(y = model, group = 1), size = .4, col = 'red4') +
  geom_line(aes(y = low, group = 1), size = .2, col = 'red4') +
  geom_line(aes(y = high, group = 1), size = .2, col = 'red4') +
  geom_point(aes(y = model), col = 'red4', size = .5) +
  geom_text(aes(y = model, label = round(model)), nudge_y = 75, size = 1.7) +
  #  geom_point(aes(y = Actual), col = 'red4', size = .5) +
  #  ggtitle(paste('Forecasted vs Actual Deaths: Week of ', sun$date, sep = '')) +
  ggtitle(paste('Forecasted Deaths: Week of ', this_week$date[1], sep = '')) +
  scale_y_continuous(breaks = seq(0,7000,500)) +
  scale_x_discrete(expand = c(.02, .02)) +
  theme_DataStache()

P_this_week

p_width <- 6
p_height <- (9/16) * p_width  

ggsave(paste('figs/death-forecast-week-of-', this_week$date[1], '.png', sep = ''),
       P_this_week,
       width = p_width,
       height = p_height,
       dpi = "retina")
