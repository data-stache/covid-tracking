{
  library(tidyverse)
  library(lubridate)
  library(zoo)
  library(gridExtra)
  library(ggthemes)
  library(gghighlight)
  library(tidylog)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
  
}

# Time Series Graph ------------------------------------------------------------
# LOAD DATA
load("rda/covid_national.rda")
options(scipen = 999)

tdy_date <- tail(covid_national$date, 1)

#### US DAILY GRAPHS ####
p_US_new_case_plot <- covid_national %>%
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
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_national$new_cases_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_deaths_plot <- covid_national %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_deaths)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark red", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_07da), size = .2, col="dark red") +
  ggtitle("US Total New Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid_national$new_deaths_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

grid.arrange(p_US_new_case_plot, p_US_new_deaths_plot, nrow=2)


# Grid Print Plots
G <- arrangeGrob(p_US_new_case_plot, p_US_new_deaths_plot, nrow=2)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave(paste("figs/united-states-graphs-", tdy_date, ".png", sep = ""),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")



# US Grid Cases ----------------------------------------------------------------
# Load Data
load(file = 'rda/covid_state.rda')
load(file = 'rda/case_order_state.rda')
load(file = 'rda/death_order_state.rda')
load(file = 'rda/ind_xlim_3m.rda')
options(digits = 3)

omit <- c('Diamond Princess', 'Grand Princess', 'American Samoa', 'Northern Mariana Islands', 'Guam', 'Virgin Islands')

# Plot
case_order <- case_order_state %>%
  filter(!state %in% omit) %>%
  .$state

p_ALL_states_cases_plot <- covid_state %>%
  filter(date %in% c(ind_xlim_3m[1]:ind_xlim_3m[2]) & !state %in% omit) %>%
  mutate(state = factor(state, levels = case_order)) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_percap_07da), size = .2, col="blue") +
  ggtitle("New Cases of Covid 19 per 100k People",
          subtitle = "Ordered from most to least reported Cases in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from Johns Hopkins") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state,
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



# US Grid Deaths ---------------------------------------------------------------
# Plot
death_order <- death_order_state %>%
  filter(!state %in% omit) %>%
  .$state

p_ALL_states_death_plot <- covid_state %>%
  filter(date %in% c(ind_xlim_3m[1]:ind_xlim_3m[2]) & !state %in% omit) %>%
  mutate(state = factor(state, levels = case_order)) %>%
  ggplot(aes(date, new_deaths_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="darkred", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_percap_07da), size = .2, col="darkred") +
  ggtitle("New Deaths of Covid 19 per 100k People",
          subtitle = "Ordered from most to least reported Cases in the last week") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from Johns Hopkins") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state,
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


# US Grid Cases VS Policy ------------------------------------------------------
# Load Policy
load('rda/policy.rda')
options(digits = 3)

# Merge Policy
policy <- covid_state %>%
  mutate(state = as.character(state)) %>%
  select(state, date, new_cases_percap, new_cases_percap_07da) %>%
  left_join(policy)



## CASES vs POLICY BY CASES

p_ALL_states_cases_policy_by_case <- policy %>%
  filter(date %in% c(ind_xlim_3m[1]:ind_xlim_3m[2])  & !state %in% omit) %>%
  mutate(state = factor(state, levels = case_order)) %>%
  ggplot(aes(x = date, y = new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .2, size = .1, fill = "darkblue") +
  geom_line(aes(y = new_cases_percap_07da), color = "darkblue", size = .25) +
  geom_line(aes(y = mean_index * 2), color = "red4", size = .5) +
  scale_y_continuous(sec.axis = sec_axis(~./2, name = "Stringency")) +
  ggtitle("New Cases of Covid 19 per 100k People vs State Mitigation Efforts",
          subtitle = "Ordered from Most Stringent to Least Stringent Covid Mitigation Policy") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  coord_cartesian(ylim = c(0, NA)) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state,
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
