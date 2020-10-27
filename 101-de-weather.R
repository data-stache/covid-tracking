library(broom)

load("rda/covid.rda")
load("rda/de_weather.rda")

dat <- covid %>%
  filter(state == "DE")

dat <- dat %>%
  select(state, date, new_cases, new_cases_07da) %>%
  inner_join(weather) 

dat %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()

dat %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  facet_wrap(. ~ month(date))



P2 <- dat %>%
  filter(date >= ymd(20200401)) %>%
  ggplot(aes(x = avg_T_07da, y = new_cases_07da)) +
  geom_point(size = 1, alpha = .5) +
  geom_smooth()
P2

dat %>%
  filter(date >= ymd(20200401)) %>%
  summarize(cor = cor(new_cases_07da, avg_T_07da))

model_jul <- dat %>%
  filter(date >= ymd(20200701)) %>%
  lm(new_cases_07da ~ avg_T_07da, data = .)
model_jul

coefs_jul <- tidy(model_jul, conf.int = TRUE)
coefs_jul

new_temps <- data.frame(avg_T_07da = seq(20,60,5))

predict(model_jul, newdata = new_temps)
