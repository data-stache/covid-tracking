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
load("rda/covid_county.rda")
options(scipen = 999)

tdy_date <- tail(covid_county$date, 1)



# Custom Variable --------------------------------------------------------------
STATE <- 'Florida'
COUNTY <- 'Manatee'



# County Graphs ----------------------------------------------------------------
p_new_case_plot <- covid_county %>%
  filter(date >= ymd(20200315) & state == STATE & county == COUNTY) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_cases_percap_07da), size = .2, col="blue") +
  ggtitle(paste('New Cases in', COUNTY, 'County,', STATE, sep = ' ')) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from Johns Hopkins University") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

p_new_deaths_plot <- covid_county %>%
  filter(date >= ymd(20200315) & state == STATE & county == COUNTY) %>%
  ggplot(aes(date, new_deaths_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="dark red", alpha = .2, size = .1) +
  scale_color_manual(values="light grey") +
  geom_line(aes(y = new_deaths_percap_07da), size = .2, col="dark red") +
  ggtitle(paste('New Deaths in', COUNTY, 'County,', STATE, sep = ' ')) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from Johns Hopkins University") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

grid.arrange(p_new_case_plot, p_new_deaths_plot, nrow=2)


# Grid Print Plots
G <- arrangeGrob(p_new_case_plot, p_new_deaths_plot, nrow=2)

p_width <- 6
p_height <- (9/16) * p_width  

ggsave(paste("figs/", COUNTY, "-", STATE, "-", tdy_date, ".png", sep = ""),
       G,
       width = p_width,
       height = p_height,
       dpi = "retina")


# County Ranks -----------------------------------------------------------------
load(file = 'rda/case_order_county.rda')
load(file = 'rda/death_order_county.rda')

county_count <- length(case_order_county$rank)

case_order_county %>%
  filter(state == STATE & county == COUNTY)

death_order_county %>%
  filter(state == STATE & county == COUNTY)




