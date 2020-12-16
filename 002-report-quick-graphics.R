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
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .35, col="blue") +
  xlab("Date") +
  ylab("New Total Cases") +
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
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .35, col="dark green") +
  xlab("Date") +
  ylab("New Total Tests") +
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
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_death_07da), size = .35, col="dark red") +
  xlab("Date") +
  ylab("New Total Deaths") +
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
  geom_bar(stat = "identity", fill="deepskyblue4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .35, col="deepskyblue4") +
  xlab("Date") +
  ylab("Percent Positive") +
  ggtitle("US Percent Positive") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_us_sum$percent_pos_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_hosp <- covid_us_sum %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, cur_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = cur_hosp_07da), size = .35, col="orange4") +
  xlab("Date") +
  ylab("Percent Positive") +
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
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_07da), size = .35, col="orange4") +
  xlab("Date") +
  ylab("Percent Positive") +
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
load("rda/ind_new_case_state.rda")
load("rda/covid_state_growth.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

## NEW CASES ALL
covid_grid_cases <- covid
new_case_order <- as.character(covid_state_growth$state[ind_new_case_state])
covid_grid_cases$state <- factor(covid_grid_cases$state, levels = new_case_order)

p_ALL_states_new_cases_plot <- covid_grid_cases %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .3, size = .1, fill = "darkblue") +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  geom_line(aes(date, y = new_cases_percap_07da, color = "darkblue"), size = .5) +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  ggtitle("New Cases Per Capita",
          subtitle = "Ordered from most to least new cases per capita in the last 7 days") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0, max(covid_grid_cases$new_cases_percap_07da) * 1.1)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_new_cases_plot

width <- 6
height <- 6 

ggsave(paste("figs/state-grid-cases-", tdy_date, ".png", sep = ""),
       width = width,
       dpi = "retina")


##### RED ZONE STATES #####
# LOAD DATA
load("rda/covid.rda")
load("rda/covid_state_growth.rda")
load("rda/ind_new_case_state.rda")
load("rda/ind_red_perpos.rda")
load("rda/ind_red_cases.rda")
load("rda/ind_tdy.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# EPIC LINEPLOT OF COVID - FULL PANDEMIC

new_case_order <- covid_state_growth$state_name[ind_new_case_state]

covid %>%
  filter(state %in% ind_red_cases & state %in% ind_red_perpos & date >= ymd(20200315)) %>%
  mutate(state_name = factor(state_name, levels = new_case_order)) %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, size = .3) +
  ggtitle("Where is Covid Hitting The Hardest?",
          subtitle = "States where both New Cases & Percent Positive are in the Red Zone") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 20), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text = element_text(size = rel(.4))) +
  facet_wrap(. ~ state_name) +
  theme(strip.text.x = element_blank()) +
  gghighlight(label_key = state_name, use_direct_label = TRUE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  x = ymd(20200501),
                                  y = 65,
                                  hjust = 1,
                                  size = 1.2,
                                  color = "dark blue",
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

width <- 6
height <- (9/16) * width

ggsave(paste("figs/double-red-zone-states-", tdy_date, ".png", sep = ""),
       width = width,
       height = height, 
       dpi = "retina")


covid %>%
  filter(state %in% ind_red_cases & date >= ymd(20200315)) %>%
  mutate(state_name = factor(state_name, levels = new_case_order)) %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, size = .3) +
  ggtitle("Where Are Covid Cases Rising the Most?",
          subtitle = "States With Greater than 100 New Cases per Week per 100k People") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 20), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(axis.text = element_text(size = rel(.4))) +
  facet_wrap(. ~ state_name) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  gghighlight(label_key = state_name, use_direct_label = TRUE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  x = ymd(20200501),
                                  y = 99,
                                  hjust = 1,
                                  size = 1.2,
                                  color = "dark blue",
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

width <- 6
height <- (9/16) * width

ggsave(paste("figs/cases-red-zone-states-", tdy_date, ".png", sep = ""),
       width = width,
       height = height, 
       dpi = "retina")
