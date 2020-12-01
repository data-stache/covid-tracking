library(tidyverse)
library(caret)
library(broom)
library(lubridate)
library(zoo)
library(tidylog)

options(scipen = 999)

load("rda/covid.rda")
load("rda/weather.rda")
load("rda/covid_pol.rda")
load("rda/mobility.rda")

head(covid_pol)

# BUILD DATA SET
dat <- covid_pol %>%
  filter(date >= ymd(20200601)) %>%
  select(date, day, state, state_name, new_cases, pop, lean_avg, mask_law) %>%
  group_by(date, state) %>%
  mutate(new_cases_percap = new_cases / pop * 100000) %>%
  ungroup() %>%
  left_join(weather_usa) %>%
  select(1:10) %>%
  filter(!is.na(tavg)) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         cases = round(cases, digits = 0),
         cases = case_when(cases <= 0 ~ 'Less Than 0',
                           cases %in% 1:10 ~ '1-10',
                           cases %in% 11:20 ~ '11-20',
                           cases %in% 21:30 ~ '21-30',
                           cases %in% 31:40 ~ '31-40',
                           cases %in% 41:50 ~ '41-50',
                           cases %in% 51:60 ~ '51-60',
                           cases %in% 61:70 ~ '61-70',
                           cases %in% 71:80 ~ '71-80',
                           cases %in% 81:90 ~ '81-90',
                           cases %in% 91:100 ~ '91-100',
                           cases > 100 ~ 'Greter than 100'),
         cases = factor(cases),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  select(date, day, state, state_name, cases, temp, lean_avg, mask_law) %>%
  ungroup() %>%
  left_join(mobility) %>%
  group_by(state) %>%
  mutate(retail = rollapply(retail_recreation, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         grocery = rollapply(grocery_pharmacy, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         parks = rollapply(parks, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         transit = rollapply(transit, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         work = rollapply(workplace, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"),
         residential = rollapply(residential, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ungroup() %>%
  select(cases, temp, lean_avg, mask_law, retail, grocery, parks, transit, work, residential)

dat <- dat %>%
  filter(!is.na(retail) & !is.na(grocery) & !is.na(parks) & !is.na(transit) & !is.na(work) & !is.na(residential))

any(is.na(dat$cases))

names(dat)

# retail, grocery, parks, transit, work, residential, pvi

##### BUILD TRAIN AND TEST SET #####
y <- dat$cases
set.seed(20201105, sample.kind = "Rounding")
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

# ENSEMBLE PREDICTION
models <- c("lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "rf")

fits <- lapply(models, function(model){ 
  print(model)
  train(cases ~ temp + lean_avg + mask_law,
        method = model,
        data = test_set)
}) 

names(fits) <- models

pred <- sapply(fits, function(object) 
  predict(object, newdata = test_set))
dim(pred)

# Now compute accuracy for each model on the test set.
# Report the mean accuracy across all models.

i <- seq(1,length(models))
pred_models <- sapply(i, function(i){
  mean(pred[,i] == test_set$cases)
})

# -- given answer --
acc <- colMeans(pred == test_set$cases)
acc
mean(acc)

fits[7]

varImp(fits[7])




train_rf <- train(cases ~ temp + lean_avg + mask_law,
                  method = 'rf',
                  data = test_set)
confusionMatrix(predict(train_rf, test_set), test_set$cases)$overall["Accuracy"]

varImp(train_rf)

plot(train_rf$finalModel, margin = 0.1)
text(train_rf$finalModel)

dat %>% 
  mutate(y_hat = predict(train_rf)) %>% 
  ggplot() +
  geom_point(aes(cases, temp)) +
  geom_step(aes(temp, y_hat), col = "red")
