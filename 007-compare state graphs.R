# LOAD DATA
load("rda/covid.rda")
load("rda/populations.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

## Pick State (ABB)
st <- c('DE', 'FL')
state <- populations %>% filter(state %in% st) %>% .$state_name

library(gghighlight)
##### PLOT CHARTS
# NEW CASES
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = new_cases_percap_07da)) +
  geom_hline(yintercept = 14, col = "red", alpha = .7, size = .25) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "New Cases Per 100k")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(c(0,0,0,0)),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW TESTS
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = new_tests_percap_07da)) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "New Tests Per 100k")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(c(0,0,0,0)),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW DEATHS
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = new_death_percap_07da)) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "New Deaths Per 100k")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(c(0,0,0,0)),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# PERCENT POS
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = percent_pos_07da)) +
  geom_hline(yintercept = .05, col = "red", alpha = .7, size = .25) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "Percent Positive")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(c(0,0,0,0)),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# HOSPITALIZATION
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = hosp_percap_07da)) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "Hospitalization per 100k")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(c(0,0,0,0)),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

# NEW HOSPITALIZATION
covid %>%
  filter(state %in% st) %>%
  ggplot(aes(x = date, y = new_hosp_percap_07da)) +
  geom_line(aes(col = state), size = .5) +
  ggtitle(paste(st[1], '&', st[2], "New Hospitalization per 100k")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(NA,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6))) +
  theme(legend.position = 'top',
        legend.text = element_text(size = rel(.5)),
        legend.margin = margin(t = -.4, r = -.4, b = -.4, l = -.4, unit = 'cm'),
        legend.key = element_blank()) +
  gghighlight(label_key = state,
              label_params = list(label.size = NA,
                                  size = 2.5,
                                  nudge_x = 4,
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))


