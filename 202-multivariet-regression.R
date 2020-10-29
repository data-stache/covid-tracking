load("rda/mobility.rda")
load("rda/weather.rda")
load("rda/covid.rda")
tail(mobility)

# REGRESSION DF
dat <- covid %>%
  select(date, state, state_name, new_cases_percap) %>%
  inner_join(mobility) %>%
  select(1:10) %>%
  inner_join(weather_usa) %>%
  select(1:11) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         ret = rollapply(retail_recreation, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         groc = rollapply(grocery_pharmacy, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         parks = rollapply(parks, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         trans = rollapply(transit, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         work = rollapply(workplace, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         res = rollapply(residential, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         temp = rollapply(tavg, width = 14, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

cor <- dat %>%
  filter(!is.na(parks) & !is.na(trans) & date >= ymd(20200401)) %>%
  summarise(cor_ret = cor(cases, ret),
            cor_groc = cor(cases, groc),
            cor_parks = cor(cases, parks),
            cor_trans = cor(cases, trans),
            cor_work = cor(cases, work),
            cor_res = cor(cases, res),
            cor_temp = cor(cases, temp))
cor

dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(alpha = .5) +
  geom_smooth()
