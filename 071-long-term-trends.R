load("rda/covid_us_sum.rda")
load("rda/theme_DataStache.rda")
options(scipen = 999)

library(tidyverse)
library(lubridate)
library(tidylog)

head(covid_us_sum)

dat <- covid_us_sum %>%
  filter(date >= ymd(20200401)) %>%
  mutate(month = month(date, label = TRUE),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>%
  group_by(month, day) %>%
  summarize(cases = sum(new_cases),
            death = sum(new_death),
            tests = sum(new_tests)) %>%
  group_by(month) %>%
  mutate(cases_per = round(cases / sum(cases) * 100, 1),
         death_per = round(death / sum(death) * 100, 1),
         tests_per = round(tests / sum(tests) * 100, 1))

# NEW CASeS
dat %>%
  mutate(cases_per = paste(cases_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = cases, fill = day, label = cases_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), hjust = -.2, position = position_dodge(.9), size = 1, fontface = 'bold', angle = 90) +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Cases Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/cases-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

 # NEW DEATHS
dat %>%
  mutate(death_per = paste(death_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = death, fill = day, label = death_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), hjust = -.2, position = position_dodge(.9), size = 1, fontface = 'bold', angle = 90) +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('Covid-19 Deaths Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

ggsave("figs/death-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")

# NEW TESTS
dat %>%
  mutate(tests_per = paste(tests_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = tests, fill = day, label = tests_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), hjust = -.2, position = position_dodge(.9), size = 1, fontface = 'bold', angle = 90) +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Tests Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

ggsave("figs/tests-day-of-the-week.png",
       width = p_width,
       height = p_height,
       dpi = "retina")
