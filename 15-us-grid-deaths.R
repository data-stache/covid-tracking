# LOAD DATA
load("rda/covid.rda")
load("rda/ind_new_death_state.rda")
load("rda/covid_state_growth.rda")
load("rda/ind_xlim_3m.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

## NEW CASES ALL
covid_grid_death <- covid
new_death_order <- as.character(covid_state_growth$state[ind_new_death_state])
covid_grid_death$state <- factor(covid_grid_death$state, levels = new_death_order)

mask_colors <- c("orangered3", "forestgreen")

p_ALL_states_new_deaths_plot <- covid_grid_death %>%
  mutate(masks = ifelse(mask_law == "NO", "No Mask", "Yes Mask"),
         masks = factor(masks, levels = c("Yes Mask", "No Mask"))) %>%
  ggplot(aes(date, new_death_percap, fill = masks)) +
  geom_hline(yintercept=0, col = "grey40", size = .25) +
  geom_bar(stat = "identity", alpha = .3, size = .1) +
  scale_color_fivethirtyeight() +
  scale_fill_fivethirtyeight() +
  geom_line(aes(date, y = new_death_percap_07da, color = masks), size = .5) +
  xlab("Date") +
  ylab("New Deaths Per 100k") +
  ggtitle("New Deaths Per Capita",
          subtitle = "Ordered from most to least new deaths per capita in the last 7 days") +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  ylim(0,2) +
  coord_cartesian(xlim = ind_xlim_3m) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle=90, hjust = 1)) +
  facet_wrap(. ~ state) +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))))

p_ALL_states_new_deaths_plot

width <- 6
height <- 6 

ggsave("figs/US New Deaths Tile.png",
       width = width,
       dpi = "retina")
