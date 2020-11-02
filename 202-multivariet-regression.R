load("rda/mobility.rda")
load("rda/weather.rda")
load("rda/covid.rda")

# REGRESSION DF
dat <- covid %>%
  select(date, state, state_name, new_cases_percap) %>%
  inner_join(mobility) %>%
  select(1:10) %>%
  inner_join(weather_usa) %>%
  select(1:11) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         ret = rollapply(retail_recreation, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         groc = rollapply(grocery_pharmacy, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         parks = rollapply(parks, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         trans = rollapply(transit, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         work = rollapply(workplace, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         res = rollapply(residential, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

cor <- dat %>%
  filter(!is.na(parks) & !is.na(trans) & date >= ymd(20200401)) %>%
  summarise(cor_ret = cor(cases, ret),
            cor_groc = cor(cases, groc),
            cor_parks = cor(cases, parks),
            cor_trans = cor(cases, trans),
            cor_work = cor(cases, work),
            cor_res = cor(cases, res),
            cor_temp = cor(cases, temp)) %>%
  summarise(cor_ret = mean(cor_ret),
            cor_groc = mean(cor_groc),
            cor_parks = mean(cor_parks),
            cor_trans = mean(cor_trans),
            cor_work = mean(cor_work),
            cor_res = mean(cor_res),
            cor_temp = mean(cor_temp, na.rm = TRUE))
cor

dat %>%
  filter(date >= ymd(20200301)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(alpha = .5) +
  geom_smooth()

dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = trans, y = cases)) +
  geom_point(alpha = .5) +
  geom_smooth() +
  facet_wrap(. ~ state)
