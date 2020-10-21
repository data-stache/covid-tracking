# LOAD DATA
load("rda/covid.rda")
load("rda/populations.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

## Pick State (ABB)
st <- "CT"
state <- populations %>% filter(state == st) %>% pull(state_name)

##### PLOT CHARTS
# NEW CASES
p_new_case <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_percap_07da), size = .25, col="blue") +
  ggtitle(paste(state, sep = " ", "New Cases Per 100k People")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  geom_hline(yintercept = 0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# NEW TESTS        
p_new_test <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_tests_percap)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_percap_07da), size = .25, col="dark green") +
  ggtitle(paste(state, sep = " ", "New Tests Per 100k People")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# NEW DEATHS
p_new_deaths <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_death_percap)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_death_percap_07da), size = .25, col="dark red") +
  ggtitle(paste(state, sep = " ", "New Deaths Per 100k People")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# PERCENT POSITIVE
p_percent_pos <- covid %>%
  filter(state == st) %>%
  mutate(percent_pos = percent_pos * 100,
         percent_pos_07da = percent_pos_07da * 100) %>%
  ggplot(aes(date, percent_pos)) +
  geom_bar(stat = "identity", fill="deepskyblue4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .25, col="deepskyblue4") +
  ggtitle(paste(state, sep = " ", "Percent Positive")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(breaks = seq(0,100,5), expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,30)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# HOSPITALIZATION
p_hosp <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, hosp_percap)) +
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = hosp_percap_07da), size = .25, col="orange4") +
  ggtitle(paste(state, sep = " ", "Hospitalization")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# NEW HOSPITALIZATION
p_new_hosp <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_hosp_percap)) +
  geom_bar(stat = "identity", fill="orange4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_hosp_percap_07da), size = .25, col="orange4") +
  ggtitle(paste(state, sep = " ", "New Hospitalization")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_new_test, p_hosp, p_new_deaths, p_percent_pos, p_new_hosp, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width  

# GRID PRINT PLOTS
G <- arrangeGrob(p_new_case, p_new_test, p_hosp, p_new_deaths, p_percent_pos, p_new_hosp, nrow = 2)
ggsave(paste("figs/", state, sep = " ", "Per Capita 90 days.png"),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")
