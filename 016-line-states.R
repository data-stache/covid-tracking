library(tidyverse)
library(zoo)
library(lubridate)
library(tidylog)

# LOAD DATA
load("rda/covid.rda")
load("rda/ind_tdy.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# EPIC LINEPLOT OF COVID - FULL PANDEMIC
avg <- covid %>%
  group_by(date) %>%
  summarize(us_rate = sum(new_cases) / sum(pop) * 100000) %>%
  ungroup()

avg <- avg %>%
  mutate(us_rate = rollapply(us_rate, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

covid %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, alpha = .5, size = .2) +
  geom_line(mapping = aes(date, us_rate), data = avg, size = .5, col = "red4") +
  ggtitle("New Cases Reports in States Compared to the National Average",
          subtitle = "Each Line Represents a US State New Reported Cases Per 100k") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200301), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache()

p_width <- 6
p_height <- (9/16) * p_width

ggsave("figs/ALL STATES CASES.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


# EPIC LINEPLOT OF COVID - FULL PANDEMIC - DEATH
avg_death <- covid %>%
  group_by(date) %>%
  summarize(us_death_rate = sum(new_death) / sum(pop) * 100000) %>%
  ungroup()

avg_death <- avg_death %>%
  mutate(us_death_rate = rollapply(us_death_rate, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

covid %>%
  ggplot() +
  geom_line(aes(x = date, y = new_death_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, alpha = .5, size = .2) +
  geom_line(mapping = aes(date, us_death_rate), data = avg_death, size = .5, col = "red4") +
  ggtitle("New Deaths Reports in States Compared to the National Average",
          subtitle = "Each Line Represents a US State New Reported Cases Per 100k") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200301), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache()

ggsave("figs/ALL STATES DEATHS.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")
