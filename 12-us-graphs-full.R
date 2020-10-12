# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/covid_us_growth.rda")
load("rda/theme_DataStache.rda")


#### US DAILY GRAPHS ####
p_US_new_case_plot <- covid_us_sum %>% 
  ggplot(aes(date, new_cases)) +
  geom_bar(stat = "identity", fill="blue", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_07da), size = .35, col="blue") +
  xlab("Date") +
  ylab("New Total Cases") +
  ggtitle("US Total New Cases") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_test_plot <- covid_us_sum %>%
  ggplot(aes(date, new_tests)) +
  geom_bar(stat = "identity", fill="dark green", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_tests_07da), size = .35, col="dark green") +
  xlab("Date") +
  ylab("New Total Tests") +
  ggtitle("US Total New Tests") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_new_deaths_plot <- covid_us_sum %>%
  ggplot(aes(date, new_death)) +
  geom_bar(stat = "identity", fill="dark red", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_death_07da), size = .35, col="dark red") +
  xlab("Date") +
  ylab("New Total Deaths") +
  ggtitle("US Total New Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_US_percent_pos <- covid_us_sum %>%
  mutate(percent_pos = percent_pos * 100,
         percent_pos_07da = percent_pos_07da * 100) %>%
  ggplot(aes(date, percent_pos)) +
  geom_bar(stat = "identity", fill="deepskyblue4", alpha = .3, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = percent_pos_07da), size = .35, col="deepskyblue4") +
  xlab("Date") +
  ylab("Percent Positive") +
  ggtitle("US Percent Positive") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0,50,5), expand = c(0,0)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

grid.arrange(p_US_new_case_plot, p_US_new_test_plot, p_US_new_deaths_plot, p_US_percent_pos, nrow=2)

# GRID PRINT PLOTS
G <- arrangeGrob(p_US_new_case_plot, p_US_new_test_plot, p_US_new_deaths_plot, p_US_percent_pos, nrow=2)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave("figs/United States Totals.png",
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")