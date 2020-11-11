library(tidyverse)
library(caret)
library(broom)

load("rda/covid.rda")
load("rda/weather.rda")
load("rda/cook_pvi.rda")
load("rda/mobility.rda")

# BUILD DATA SET
dat <- covid %>%
  filter(date >= ymd(20200601)) %>%
  select(date, day, state, state_name, new_cases_percap) %>%
  left_join(weather_usa) %>%
  select(1:6) %>%
  filter(!is.na(tavg)) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  select(date, day, state, state_name, cases, temp) %>%
  ungroup() %>%
  left_join(mobility) %>%
  group_by(state) %>%
  mutate(retail = rollapply(retail_recreation, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         grocery = rollapply(grocery_pharmacy, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         parks = rollapply(parks, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         transit = rollapply(transit, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         work = rollapply(workplace, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         residential = rollapply(residential, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  select(date, day, state, state_name, cases, temp, retail, grocery, parks, transit, work, residential) %>%
  ungroup() %>%
  left_join(cook_pvi)


##### EXPLATORY #####
head(dat)

# TEMP
dat %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(alpha = .5, color = "black")

# RETAIL
dat %>%
  ggplot(aes(x = retail, y = cases)) +
  geom_point(alpha = .5, color = "black")

# GROCERY
dat %>%
  ggplot(aes(x = grocery, y = cases)) +
  geom_point(alpha = .5, color = "black")

# PARKS
dat %>%
  ggplot(aes(x = parks, y = cases)) +
  geom_point(alpha = .5, color = "black")

# TRANSIT
dat %>%
  ggplot(aes(x = transit, y = cases)) +
  geom_point(alpha = .5, color = "black")

# WORK
dat %>%
  ggplot(aes(x = work, y = cases)) +
  geom_point(alpha = .5, color = "black")

# RES
dat %>%
  ggplot(aes(x = residential, y = cases)) +
  geom_point(alpha = .5, color = "black")

# DAY
dat %>%
  ggplot(aes(x = day, y = cases)) +
  geom_point(alpha = .5, color = "black")

# PVI
dat %>%
  ggplot(aes(x = pvi, y = cases)) +
  geom_point(alpha = .5, color = "black")


# BUILD TRAIN AND TEST SET
y <- dat$cases
set.seed(20201105, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)


# LOESS MODEL
temp_range <- diff(range(train_set$temp))

Ss <- seq(3:79)
acc <- map_df(Ss, function(s){
  span <- s / temp_range
  fit <- loess(cases ~ temp, degree = 1, span = span, data = train_set, control=loess.control(surface="direct"))
  y_hat <- predict(fit, test_set)
  error <- mean((y_hat - test_set$cases)^2)
  tibble(error = error, S = s)
})

acc %>%
  ggplot(aes(S, error)) +
  geom_line() +
  geom_point()

best_s <- Ss[which.min(acc$error)]
span <- best_s / temp_range

fit <- loess(cases ~ temp, degree = 1, span = span, data = train_set, control=loess.control(surface="direct"))
fit

train_set %>% 
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = 1, alpha = .5, color = "grey") +
  geom_line(aes(temp, smooth), color = "red")

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$cases)^2)












