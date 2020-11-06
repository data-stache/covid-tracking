library(tidyverse)
library(caret)
library(broom)

load("rda/covid.rda")
load("rda/weather.rda")

head(covid)
head(weather_usa)

# BUILD DATA SET
dat <- covid %>%
  filter(date >= ymd(20200601)) %>%
  select(date, state, state_name, new_cases_percap) %>%
  left_join(weather_usa) %>%
  select(1:5) %>%
  filter(!is.na(tavg)) %>%
  group_by(state) %>%
  mutate(cases = rollapply(new_cases_percap, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="left"),
         temp = rollapply(tavg, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  select(date, state, state_name, cases, temp) %>%
  ungroup()


# BUILD TRAIN AND TEST SET
y <- dat$cases
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)


# JUST GUESSING BASED ON MEAN
avg <- mean(train_set$temp)
avg
mean((avg - test_set$temp)^2) # <---- SQUARED LOSS!!!


# LINEAR MODEL
fit <- glm(cases ~ temp, data = train_set)
fit$coef

y_hat <- predict(fit, test_set)
mean((y_hat - test_set$cases)^2)

train_set %>% 
  mutate(smooth = fit$fitted) %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = 1, alpha = .5, color = "grey") +
  geom_line(aes(temp, smooth), color = "red")


# LOESS MODEL
temp_range <- diff(range(train_set$temp))
span <- 7/temp_range

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

predict(fit, 62)



#KNN MODEL
knn_fit <- knn3(cases ~ temp, data = train_set, k=5)
y_hat_knn <- predict(knn_fit, test_set, type = "class") # type = "class" gives us the actual outcomes
confusionMatrix(data = y_hat_knn, reference = test_set)$overall["Accuracy"]

confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(cases ~ temp, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  cm <- confusionMatrix(data = y_hat, reference = test_set)
  error <- cm$overall["Accuracy"]
  
  tibble(error = error)
})



# EXPERIMENTING WITH QUADRATIC
# Square X Axis
train_set <- train_set %>%
  mutate(temp2 = temp^2)

test_set <- test_set %>%
  mutate(temp2 = temp^2)

quad_fit <- lm(cases ~ temp + temp2, data = train_set)
confint(quad_fit)

y_hat <- predict(quad_fit, test_set)
mean((y_hat - test_set$cases)^2)


# LINES
temps <- seq(0, 90, .1)
predict <- predict(quad_fit, list(temp = temps, temp2 = temps^2))

dat <- train_set
dat$lm <- predict(quad_fit)

dat %>%
  filter(state == "AZ") %>%
  ggplot(aes(x = temp, y = cases)) +
  geom_point(size = .5, alpha = .3) +
  geom_line(aes(y = lm))












