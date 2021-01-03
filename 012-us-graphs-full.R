# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/covid_us_growth.rda")
load("rda/theme_DataStache.rda")
options(scipen = 999)


#### US DAILY GRAPHS ####
p_US_new_case_plot <- covid_us_sum %>% 
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(0, max(covid_us_sum$new_cases_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_test_plot <- covid_us_sum %>%
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(0, max(covid_us_sum$new_tests_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_deaths_plot <- covid_us_sum %>%
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(0, max(covid_us_sum$new_death_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_percent_pos <- covid_us_sum %>%
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(0, .3)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_hosp <- covid_us_sum %>%
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(0, max(covid_us_sum$cur_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_hosp <- covid_us_sum %>%
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
  coord_cartesian(xlim = c(ymd(20200301), NA), ylim = c(min(covid_us_sum$new_hosp_07da, na.rm = TRUE) * 1.1, max(covid_us_sum$new_hosp_07da, na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

library(gridExtra)
grid.arrange(p_US_new_case_plot, p_US_new_test_plot, p_US_hosp, p_US_new_deaths_plot, p_US_percent_pos, p_US_new_hosp, nrow=2)

# GRID PRINT PLOTS
G <- arrangeGrob(p_US_new_case_plot, p_US_new_test_plot, p_US_hosp, p_US_new_deaths_plot, p_US_percent_pos, p_US_new_hosp, nrow=2)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/United States Totals.png",
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")