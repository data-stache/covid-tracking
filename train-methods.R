library(tidyverse)
library(caret)
library(broom)
library(lubridate)
library(zoo)
library(tidylog)

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

dat <- dat %>%
  filter(!is.na(retail) & !is.na(grocery) & !is.na(parks) & !is.na(transit) & !is.na(work) & !is.na(residential) & !is.na(pvi))

any(is.na(dat$cases))

# retail, grocery, parks, transit, work, residential, pvi

##### BUILD TRAIN AND TEST SET #####
y <- dat$cases
set.seed(20201105, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)


##### TRAIN KNN #####
# Predict region using KNN
library(caret)
fit_knn <- train(cases ~ temp,  method = "knn", 
             tuneGrid = data.frame(k = seq(1, 81, 2)), 
             data = train_set)
ggplot(fit_knn, highlight = TRUE)

print(fit_knn)

knn_predictions <- predict(fit_knn, test_set)
knn_data <- as_tibble(knn_predictions) %>% 
  bind_cols(temps = test_set$temp)
ggplot(knn_data, aes(x = temps, y = value)) +
  geom_point(data = test_set, aes(x = temp, y = cases), col = "grey40", alpha = .5, size = .7) +
  geom_line(color = "blue", size = 1)



# Predict cases using decision tree RPART
fit_rpart <- train(cases ~ temp,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.015, len = 25)),
                     data = train_set)

ggplot(fit_rpart, highlight = TRUE)

rpart_predictions <- predict(fit_rpart, test_set)
rpart_data <- as_tibble(rpart_predictions) %>% 
  bind_cols(temps = test_set$temp)
ggplot(rpart_data, aes(x = temps, y = value)) +
  geom_point(data = test_set, aes(x = temp, y = cases), col = "grey40", alpha = .5, size = .7) +
  geom_line(color = "blue", size = 1)



# Predict cases loess
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

fit_loess <- train(cases ~ temp,
                   method = "gamLoess",
                   tuneGrid=grid,
                   data = train_set)

ggplot(fit_loess, highlight = TRUE)

loess_predictions <- predict(fit_loess, test_set)
loess_data <- as_tibble(loess_predictions) %>% 
  bind_cols(temps = test_set$temp)
ggplot(loess_data, aes(x = temps, y = value)) +
  geom_point(data = test_set, aes(x = temp, y = cases), col = "grey40", alpha = .5, size = 1) +
  geom_line(color = "blue", size = 1)









