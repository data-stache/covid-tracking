# Load Libraries ---------------------------------------------------------------
{
  library(lubridate)
  library(knitr)
  library(tidyverse)
  library(tidylog)
  library(ggplot2)
  load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
}



# Load Data --------------------------------------------------------------------
load("rda/vaccinations.rda")



# Variables --------------------------------------------------------------------
RUN_DATE <- Sys.Date()
# STATE <- 'KY'

#dat_vac <- vaccinations %>%
#  filter(state == STATE)



# Single State ------------------------------------------------------------------
#dat_vac %>%
#  select(date, state, share_used_doses) %>%
#  kable()



# Make Grpahs ------------------------------------------------------------------
# New Vaccinations
# Currently Deactivated
if (FALSE) {
p_new_vac <- dat_vac %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = new_people_vaccinated), stat = "identity", fill="blue", alpha = .3, size = .1) +
  geom_line(aes(y = new_people_vaccinated_07da), size = .2, col="blue") +
  ggtitle("New People Vaccinated",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(dat_vac$new_people_vaccinated_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# New Fully Vaccinated     
p_new_full_vac <- dat_vac %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = new_fully_vaccinated), stat = "identity", fill="dark green", alpha = .3, size = .1) +
  geom_line(aes(y = new_fully_vaccinated_07da), size = .2, col="dark green") +
  ggtitle("New People Fully Vaccinated",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0, max(dat_vac$new_fully_vaccinated_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))

# Share Vaccinated
p_share_vaccinated <- dat_vac %>%
  ggplot(aes(x = date)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(y = share_pop_vaccinated * 100), size = .2, col="dark red") +
  geom_line(aes(y = share_pop_fully_vaccinated * 100), size = .2, col="dark orange") +
  ggtitle("Share Population Vaccinated",
          subtitle = 'State of Connecticut') +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.55)),
        axis.text = element_text(size = rel(.65)),
        plot.caption = element_text(size = rel(.5)))


library(gridExtra)
# GRID ARRANGE PLOTS
grid.arrange(p_new_vac, p_new_full_vac, p_share_vaccinated, nrow = 1)

p_width <- 6
p_height <- (9/16) * p_width 

P <- arrangeGrob(p_new_vac, p_new_full_vac, p_share_vaccinated, nrow = 1)
ggsave(paste("figs/state-daily-vac-metrics-", RUN_DATE, ".png", sep = ""),
       P,
       width = p_width,
       height = p_height,
       dpi = "retina")
}


# Percent Share Vaccinated State (BAR) -----------------------------------------
vac_ord <- vaccinations %>%
  filter(date == max(date)) %>%
  select(state_name, share_pop_vaccinated) %>%
  arrange(desc(share_pop_vaccinated)) %>%
  .$state_name

p_ALL_VACCINATIONS_BAR <- vaccinations %>%
  filter(date == max(date)) %>%
  mutate(state_name = factor(state_name, levels = rev(vac_ord))) %>%
  ggplot(aes(x = state_name)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = share_pop_vaccinated * 100), position = 'dodge', stat = 'identity', size = .35, fill = "dark red") +
  geom_text(aes(y = share_pop_vaccinated * 100, label = paste(round(share_pop_vaccinated * 100, 2), '%', sep = '')), nudge_y = .3, hjust = 0, size = 3, color = 'dark red') +
  ggtitle("Share of Population With At Least\nOne Dose of Covid-19 Vaccine",
          subtitle = paste('As of', date(max(vaccinations$date)))) +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 100, 10), limits = c(0, 110)) +
  coord_flip() +
  theme_DataStache() +
  theme(text = element_text(size = rel(2)))

p_ALL_VACCINATIONS_BAR

p_height <- 12
p_width <- (9/16) * p_height

ggsave(paste("figs/state-bar-all-vaccinations-", RUN_DATE, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")



vac_ord <- vaccinations %>%
  filter(date == max(date)) %>%
  select(state_name, share_pop_fully_vaccinated) %>%
  arrange(desc(share_pop_fully_vaccinated)) %>%
  .$state_name

p_FULLY_VACCINATIONED_BAR <- vaccinations %>%
  filter(date == max(date)) %>%
  mutate(state_name = factor(state_name, levels = rev(vac_ord))) %>%
  ggplot(aes(x = state_name)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = share_pop_fully_vaccinated * 100), position = 'dodge', stat = 'identity', size = .35, fill = "dark blue") +
  geom_text(aes(y = share_pop_fully_vaccinated * 100, label = paste(round(share_pop_fully_vaccinated * 100, 2), '%', sep = '')), nudge_y = .3, hjust = 0, size = 3, color = 'dark blue') +
  ggtitle("Share of Population Fully\nVaccinated Against Covid-19",
          subtitle = paste('As of', date(max(vaccinations$date)))) +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 100, 10), limits = c(0, 110)) +
  coord_flip() +
  theme_DataStache() +
  theme(text = element_text(size = rel(2)))

p_FULLY_VACCINATIONED_BAR

ggsave(paste("figs/state-bar-full-vaccinations-", RUN_DATE, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")



vac_ord <- vaccinations %>%
  filter(date == max(date)) %>%
  select(state_name, share_used_doses_07da) %>%
  arrange(desc(share_used_doses_07da)) %>%
  .$state_name

p_DOSES_USED_BAR_07DA <- vaccinations %>%
  filter(date == max(date)) %>%
  mutate(state_name = factor(state_name, levels = rev(vac_ord))) %>%
  ggplot(aes(x = state_name)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(aes(y = share_used_doses_07da * 100), position = 'dodge', stat = 'identity', size = .35, fill = "dark red") +
  geom_text(aes(y = share_used_doses_07da * 100, label = paste(round(share_used_doses_07da * 100, 2), '%', sep = '')), nudge_y = .3, hjust = 0, size = 3, color = 'dark red') +
  ggtitle("Share of Covid-19\nVaccine Used",
          subtitle = paste('As of', date(max(vaccinations$date)))) +
  labs(caption = "Created by Andrew F. Griffin\nVaccination Data from Our World in Data") +
  scale_y_continuous(expand = c(0,0), breaks = seq(0, 100, 10), limits = c(0, 110)) +
  coord_flip() +
  theme_DataStache() +
  theme(text = element_text(size = rel(2)))

p_DOSES_USED_BAR_07DA

ggsave(paste("figs/state-bar-doses-used-07da-", RUN_DATE, ".png", sep = ""),
       width = p_width,
       height = p_height,
       dpi = "retina")



# Raw Numbers ------------------------------------------------------------------
vaccinations %>%
  filter(date == max(date)) %>%
  select(state, state_name, share_pop_fully_vaccinated) %>%
  arrange(desc(share_pop_fully_vaccinated)) %>%
  head(5) %>%
  kable()

vaccinations %>%
  filter(date == max(date)) %>%
  select(state, state_name, share_pop_vaccinated) %>%
  arrange(desc(share_pop_vaccinated)) %>%
  head(5) %>%
  kable()

vaccinations %>%
  filter(date == max(date)) %>%
  select(state, state_name, share_used_doses_07da) %>%
  arrange(desc(share_used_doses_07da)) %>%
  head(5) %>%
  kable()













