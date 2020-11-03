load("rda/covid_us_sum.rda")
load("rda/theme_DataStache.rda")

head(covid_us_sum)

ind_date <- ymd(20201101)

covid_us_sum %>%
  filter(date >= ymd(20200301) & date < ind_date) %>%
  mutate(month = month(date, label = TRUE),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(month, day) %>%
  summarize(cases = sum(new_cases),
            death = sum(new_death),
            tests = sum(new_tests)) %>%
  group_by(month) %>%
  mutate(cases_per = round(cases / sum(cases) * 100, 1),
         death_per = round(death / sum(death) * 100, 1),
         tests_per = round(tests / sum(tests) * 100, 1)) %>%
  ggplot(aes(x = month, y = death, fill = day, label = cases_per)) +
  geom_col(col = "black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5)) +
  scale_fill_viridis(discrete = TRUE)
