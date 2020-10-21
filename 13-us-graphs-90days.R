# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")

## US DAILY STATS
p_US_new_case_plot <- covid_us_sum %>% 
  ggplot(aes(date, new_cases)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .5, col="blue") +
  xlab("Date") +
  ylab("New Total Cases") +
  ggtitle("US Total New Cases",
          subtitle = "Past 90 Days") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_test_plot <- covid_us_sum %>%
  ggplot(aes(date, new_tests)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .5, col="dark green") +
  xlab("Date") +
  ylab("New Total Tests") +
  ggtitle("US Total New Tests",
          subtitle = "Past 90 Days") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_deaths_plot <- covid_us_sum %>%
  ggplot(aes(date, new_death)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_death_07da), size = .5, col="dark red") +
  xlab("Date") +
  ylab("New Total Deaths") +
  ggtitle("US Total New Deaths",
          subtitle = "Past 90 Days") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_hosp <- covid_us_sum %>%
  ggplot(aes(date, cur_hosp)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="orange3", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = cur_hosp_07da), size = .35, col="orange3") +
  xlab("Date") +
  ylab("Percent Positive") +
  ggtitle("US Hospitalization",
          subtitle = "Past 90 Days") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  coord_cartesian(xlim = ind_xlim_3m, ylim = c(0,NA)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

grid.arrange(p_US_new_case_plot, p_US_new_test_plot, p_US_new_deaths_plot, p_US_hosp, nrow=1)

# GRID PRINT PLOTS
G <- arrangeGrob(p_US_new_case_plot, p_US_new_test_plot, p_US_new_deaths_plot, p_US_hosp, nrow=1)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/United States Totals 90 Days.png",
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")
