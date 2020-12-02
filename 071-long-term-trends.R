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
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")),
         day = recode(day, 'Sunday' = 'Sn',
                      'Monday' = 'M',
                      'Tuesday' = 'Tu',
                      'Wednesday' = 'W',
                      'Thursday' = 'Th',
                      'Friday' = 'F',
                      'Saturday' = 'St')) %>%
  group_by(month, day) %>%
  summarize(cases = sum(new_cases),
            death = sum(new_death),
            tests = sum(new_tests)) %>%
  group_by(month) %>%
  mutate(cases_per = round(cases / sum(cases) * 100, 1),
         death_per = round(death / sum(death) * 100, 1),
         tests_per = round(tests / sum(tests) * 100, 1))

# NEW CASeS
P_cases <- dat %>%
  mutate(cases_per = paste(cases_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = cases, fill = day, label = cases_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), vjust = -.35, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Cases Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

#p_width <- 6
#p_height <- (9/16) * p_width  

#ggsave("figs/cases-day-of-the-week.png",
#       width = p_width,
#       height = p_height,
#       dpi = "retina")

 # NEW DEATHS
P_deaths <- dat %>%
  mutate(death_per = paste(death_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = death, fill = day, label = death_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), vjust = -.35, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('Covid-19 Deaths Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

#ggsave("figs/death-day-of-the-week.png",
#       width = p_width,
#       height = p_height,
#       dpi = "retina")

# NEW TESTS
P_tests <- dat %>%
  mutate(tests_per = paste(tests_per,"%", sep = "")) %>%
  ggplot(aes(x = month, y = tests, fill = day, label = tests_per)) +
  geom_hline(yintercept = 0, size = .3, col = 'grey40') +
  geom_col(position = 'dodge', col = "black", size = .2) +
  geom_text(size = 1, position = position_dodge(.9), hjust = 1.2, angle = 90) +
  geom_text(aes(y=0 , label = day), vjust = -.35, position = position_dodge(.9), size = 1, fontface = 'bold') +
  scale_fill_brewer(palette = 'Dark2') +
  coord_cartesian(expand = FALSE) +
  theme_DataStache() +
  theme(panel.grid.major.x = element_blank()) +
  ggtitle('New Covid-19 Tests Reported by Day of the Week',
          subtitle = 'Long Term Trends in US Covid-19 Reporting') +
  labs(caption = 'Created by Andrew F. Griffin, Data The Covid Tracking Project',
       fill = 'Day of the Week')

#ggsave("figs/tests-day-of-the-week.png",
#       width = p_width,
#       height = p_height,
#       dpi = "retina")

library(gridExtra)

grid.arrange(P_cases, P_deaths, P_tests)
G <- arrangeGrob(P_cases, P_deaths, P_tests)

p_width <- 12
p_height <- (9/16) * p_width  

ggsave("figs/long-term-trends.png",
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")
