# Libraries and Options --------------------------------------------------------
{
  library(tidyverse, quietly = TRUE)
  library(lubridate, quietly = TRUE)
  library(gridExtra, quietly = TRUE)
  library(tidylog, quietly = TRUE)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
}
options(scipen = 999)



# Load Data --------------------------------------------------------------------
load('rda/vaccinations.rda')



# Variables --------------------------------------------------------------------
# Max Date
tdy_date <- vaccinations$date[1]

# US Vaccination Weekly Totals
vaccination_weekly <- vaccinations %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  summarize(new_vaccinations = sum(new_vaccinations, na.rm = TRUE),
            new_people_vaccinated = sum(new_people_vaccinated, na.rm = TRUE),
            new_fully_vaccinated = sum(new_fully_vaccinated, na.rm = TRUE),
            share_pop_vaccinated = sum(people_vaccinated, na.rm = TRUE) / sum(population),
            share_pop_fully_vaccinated = sum(people_fully_vaccinated, na.rm = TRUE) / sum(population))

# Week Start Dates
week_start <- vaccinations %>%
  mutate(week = epiweek(date)) %>%
  group_by(week) %>%
  arrange(date) %>%
  slice_head(., n = 1) %>%
  arrange(desc(date)) %>%
  select(week, date)

week_start$date[week_start$date == min(week_start$date)] <- ymd(20210110)

# Join - min and max dates
vaccination_weekly <- vaccination_weekly %>% inner_join(week_start) %>% arrange(desc(date))
min_date <- min(vaccination_weekly$date)
max_date <- max(vaccination_weekly$date)



# Run Visualizations -----------------------------------------------------------
# New Vaccinations
change <- round(((vaccination_weekly$new_vaccinations[1] - vaccination_weekly$new_vaccinations[2]) / vaccination_weekly$new_vaccinations[2]) * 100, 1)
p_NEW_VACCINATIONS <- vaccination_weekly %>%
  ggplot(aes(x = date, y = new_vaccinations)) +
  geom_col(fill = 'red4') +
  ggtitle(paste('US Weekly New Vaccinations\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "1 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week")) +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'red4'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_NEW_VACCINATIONS


# New People Vaccinated
change <- round(((vaccination_weekly$new_people_vaccinated[1] - vaccination_weekly$new_people_vaccinated[2]) / vaccination_weekly$new_people_vaccinated[2]) * 100, 1)
p_NEW_PEOPLE_VACCINATED <- vaccination_weekly %>%
  ggplot(aes(x = date, y = new_people_vaccinated)) +
  geom_col(fill = 'magenta4') +
  ggtitle(paste('US Weekly New People Vaccinated\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week")) +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'magenta4'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_NEW_PEOPLE_VACCINATED


# New People Fully Vaccinated
change <- round(((vaccination_weekly$new_fully_vaccinated[1] - vaccination_weekly$new_fully_vaccinated[2]) / vaccination_weekly$new_fully_vaccinated[2]) * 100, 1)
p_NEW_FULLY <- vaccination_weekly %>%
  ggplot(aes(x = date, y = new_fully_vaccinated)) +
  geom_col(fill = 'dark blue') +
  ggtitle(paste('US New People Fully Vaccinated\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week")) +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'dark blue'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))

p_NEW_FULLY


## DEATH
change <- round(((covid_weekly$new_death[1] - covid_weekly$new_death[2]) / covid_weekly$new_death[2]) * 100, 1)
p_Death <- covid_weekly %>%
  ggplot(aes(x = date, y = new_death)) +
  geom_col(fill = 'green4') +
  ggtitle(paste('US Weekly Death\nWeek Beginning', month(max_date, label = TRUE), day(max_date)),
          subtitle = paste(ifelse(change > 0, '+', ''), change, '% From Last Week', sep = '')) +
  scale_x_date(breaks = function(x) seq.Date(from = min_date, 
                                             to = max_date, 
                                             by = "2 weeks"),
               minor_breaks = function(x) seq.Date(from = min_date, 
                                                   to = max_date, 
                                                   by = "week")) +
  theme_DataStache() +
  coord_cartesian(expand = FALSE) +
  geom_hline(yintercept = 0, size = .25 , col = "grey40") +
  theme(plot.title = element_text(size = rel(.5), hjust = .5),
        plot.subtitle = element_text(size = rel(.5), face = 'bold',
                                     hjust = .5,
                                     vjust = 1.5, colour = 'green4'),
        axis.text.y = element_text(size = rel(1),
                                   face = "bold"),
        axis.text.x = element_text(size = rel(.8),
                                   angle = 90),
        panel.grid.major.x = element_blank(),
        plot.margin = unit(c(.1, .2, .1, .2), "cm"))


grid.arrange(p_Cases, p_Tests, p_Hosp, p_Death, nrow = 1)
P <- arrangeGrob(p_Cases, p_Tests, p_Hosp, p_Death, nrow = 1)

p_width <- 6
p_height <- (9/16) * p_width 

ggsave(paste("figs/weekly-percent-change-", tdy_date, ".png", sep = ''),
       P,
       width = p_width,
       height = p_height,
       dpi = 'retina')


