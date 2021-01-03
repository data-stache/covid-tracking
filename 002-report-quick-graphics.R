library(tidyverse)
library(tidylog)
library(lubridate)
library(zoo)
library(gridExtra)

##### US FULL PANDEMIC GRAPH #####
# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/covid_us_growth.rda")
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(scipen = 999)

tdy_date <- covid_us_sum$date[1]

#### US DAILY GRAPHS ####
p_US_new_case_plot <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_cases)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .2, col="blue") +
  ggtitle("US Total New Cases") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_us_sum$new_cases_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_test_plot <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_tests)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark green", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .2, col="dark green") +
  ggtitle("US Total New Tests") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_us_sum$new_tests_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_deaths_plot <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_death)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark red", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_death_07da), size = .2, col="dark red") +
  ggtitle("US Total New Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_us_sum$new_death_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_percent_pos <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, percent_pos)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="deepskyblue4", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .2, col="deepskyblue4") +
  ggtitle("US Percent Positive") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_hosp <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, cur_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange4", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = cur_hosp_07da), size = .2, col="orange4") +
  ggtitle("US Hospitalization") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_us_sum$cur_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_hosp <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange4", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_07da), size = .2, col="orange4") +
  ggtitle("US New Hospitalization") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(min(covid_us_sum$new_hosp_07da, na.rm = TRUE) * 1.1, max(covid_us_sum$new_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

library(gridExtra)
library(ggthemes)
library(gghighlight)
grid.arrange(p_US_new_case_plot, p_US_new_test_plot, p_US_hosp, p_US_new_deaths_plot, p_US_percent_pos, p_US_new_hosp, nrow=2)

# GRID PRINT PLOTS
G <- arrangeGrob(p_US_new_case_plot, p_US_new_test_plot, p_US_hosp, p_US_new_deaths_plot, p_US_percent_pos, p_US_new_hosp, nrow=2)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave(paste("figs/united-states-graphs-", tdy_date, ".png", sep = ""),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")


##### US GRID CASES #####
# LOAD DATA
load("rda/covid.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

head(covid)

## MEAN HOSPITALIZATION

case_order <- covid %>%
  filter(date >= as.Date(date[1] - 6)) %>%
  group_by(state_name) %>%
  summarize(cases = sum(new_cases_percap)) %>%
  arrange(desc(cases)) %>%
  .$state_name

p_ALL_states_cases_plot <- covid %>%
  mutate(state_name = factor(state_name, levels = case_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkblue") +
  geom_line(aes(x = date, y = new_cases_percap_07da), color = "darkblue", size = .25) +
  ggtitle("New Cases of Covid 19 per 100k People",
          subtitle = "Ordered from most to least reported Cases in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_cases_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_cases_plot

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-cases-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")


##### US GRID HOSPITALIZATION #####
# LOAD DATA
load("rda/covid.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

head(covid)

## MEAN HOSPITALIZATION

hosp_order <- covid %>%
  filter(date >= as.Date(date[1] - 6)) %>%
  group_by(state_name) %>%
  summarize(hosp = mean(hosp_percap, na.rm = TRUE)) %>%
  arrange(desc(hosp)) %>%
  .$state_name

p_ALL_states_hosp_plot <- covid %>%
  mutate(state_name = factor(state_name, levels = hosp_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = hosp_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "orange4") +
  geom_line(aes(x = date, y = hosp_percap_07da), color = "orange4", size = .25) +
  ggtitle("Covid 19 Hospitalizations per 100k People",
          subtitle = "Ordered from most to least reported Hospitalization in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$hosp_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_hosp_plot

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-hosp-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")


##### US GRID TESTS #####
# LOAD DATA
load("rda/covid.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

head(covid)

## MEAN HOSPITALIZATION

test_order <- covid %>%
  filter(date >= as.Date(date[1] - 6)) %>%
  group_by(state_name) %>%
  summarize(tests = sum(new_tests_percap)) %>%
  arrange(desc(tests)) %>%
  .$state_name

p_ALL_states_test_plot <- covid %>%
  mutate(state_name = factor(state_name, levels = test_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_tests_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkgreen") +
  geom_line(aes(x = date, y = new_tests_percap_07da), color = "darkgreen", size = .25) +
  ggtitle("New Covid 19 Tests per 100k People",
          subtitle = "Ordered from most to least reported Tests in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_tests_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_test_plot

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-test-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")


##### US GRID DEATHS #####
# LOAD DATA
load("rda/covid.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

head(covid)

## MEAN HOSPITALIZATION

death_order <- covid %>%
  filter(date >= as.Date(date[1] - 6)) %>%
  group_by(state_name) %>%
  summarize(deaths = sum(new_death_percap)) %>%
  arrange(desc(deaths)) %>%
  .$state_name

p_ALL_states_death_plot <- covid %>%
  mutate(state_name = factor(state_name, levels = death_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_death_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkred") +
  geom_line(aes(x = date, y = new_death_percap_07da), color = "darkred", size = .25) +
  ggtitle("New Covid 19 Deaths per 100k People",
          subtitle = "Ordered from most to least reported Deaths in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_death_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_death_plot

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-death-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")


##### US GRID CASES #####
# LOAD DATA
load("rda/covid.rda")
load('rda/policy.rda')
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

head(covid)
head(policy)

policy <- covid %>%
  filter(!state == 'PR') %>%
  mutate(state = as.character(state)) %>%
  select(state, state_name, date, new_cases_percap, new_cases_percap_07da) %>%
  left_join(policy)

## CASES vs POLICY BY POLICY

policy_order <- policy %>%
  filter(date >= as.Date(date[1] - 6)) %>%
  group_by(state_name) %>%
  summarize(pol = mean(mean_index, na.rm = TRUE)) %>%
  arrange(desc(pol)) %>%
  .$state_name

p_ALL_states_cases_policy <- policy %>%
  mutate(state_name = factor(state_name, levels = policy_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkblue") +
  geom_line(aes(x = date, y = new_cases_percap_07da), color = "darkblue", size = .25) +
  geom_line(aes(y = mean_index * 2), color = "red4", size = .5) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stringency")) +
  ggtitle("New Cases of Covid 19 per 100k People vs State Mitigation Efforts",
          subtitle = "Ordered from Most Stringent to Least Stringent Covid Mitigation Policy") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_cases_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_cases_policy

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-cases-policy-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")



## CASES vs POLICY BY CASES

p_ALL_states_cases_policy_by_case <- policy %>%
  mutate(state_name = factor(state_name, levels = case_order)) %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(x = date, y = new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkblue") +
  geom_line(aes(x = date, y = new_cases_percap_07da), color = "darkblue", size = .25) +
  geom_line(aes(y = mean_index * 2), color = "red4", size = .5) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stringency")) +
  ggtitle("New Cases of Covid 19 per 100k People vs State Mitigation Efforts",
          subtitle = "Ordered from Most Stringent to Least Stringent Covid Mitigation Policy") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid$new_cases_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state_name,
             strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.5),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_cases_policy_by_case

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-cases-policy-by-case-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")
