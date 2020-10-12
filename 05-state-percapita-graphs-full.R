# LOAD DATA
load("rda/covid.rda")
load("rda/populations.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

## Pick State (ABB)
st <- "FL"
state <- populations %>% filter(state == st) %>% pull(state_name)


##### PLOT CHARTS
# NEW CASES
p_new_case <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_percap_07da), size = .25, col="blue") +
  xlab("Date") +
  ylab("New Total Cases") +
  ggtitle(paste(state, sep = " ", "New Cases Total")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# NEW TESTS        
p_new_test <- covid %>%
  filter(state == st) %>%
  ggplot(aes(date, new_tests_percap)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_percap_07da), size = .25, col="dark green") +
  xlab("Date") +
  ylab("New Total Tests") +
  ggtitle(paste(state, sep = " ", "New Tests Total")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
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
  xlab("Date") +
  ylab("New Total Deaths") +
  ggtitle(paste(state, sep = " ", "New Deaths Total")) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_percent_pos <- covid %>%
  filter(state == st) %>%
  mutate(percent_pos = percent_pos * 100,
         percent_pos_07da = percent_pos_07da * 100) %>%
  ggplot(aes(date, percent_pos)) +
  geom_bar(stat = "identity", fill="deepskyblue4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .25, col="deepskyblue4") +
  xlab("Date") +
  ylab("New Deaths Per 100k") +
  ggtitle(paste(state, sep = " ", "Percent Positive"),
          subtitle = "Per 100k People, Past 90 Days") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(breaks = seq(0,100,5), expand = c(0,0)) +
  coord_cartesian(ylim = c(0,100)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

# GRID ARRANGE PLOTS
grid.arrange(p_new_case, p_percent_pos, p_new_test, p_new_deaths, nrow = 2)

# GRID PRINT PLOTS
G <- arrangeGrob(p_new_case, p_percent_pos, p_new_test, p_new_deaths, nrow = 2)

p_width <- 6
p_height <- (9/16) * p_width 

ggsave(paste("figs/", state, sep = " ", "Total Per Capita.png"),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")
