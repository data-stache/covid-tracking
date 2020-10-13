# LOAD DATA
load("rda/covid.rda")
load("rda/ind_tdy.rda")
load("rda/covid_state_growth.rda")
load("rda/ind_new_case_state.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# FILTER STATES > 40 CASES PER 100K PER DAY
worst <- covid %>%
  filter(new_cases_percap_07da > 40) %>%
  mutate(state = as.character(state)) %>%
  distinct(state) %>%
  pull(state)

new_case_order <- covid_state_growth$state_name[ind_new_case_state]

# EPIC LINEPLOT OF COVID - FULL PANDEMIC
avg <- covid %>%
  group_by(date) %>%
  summarize(us_rate = sum(new_cases) / sum(pop) * 100000) %>%
  ungroup()

avg <- avg %>%
  mutate(us_rate = rollapply(us_rate, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

p_line_covid <- covid %>%
  filter(state %in% worst) %>%
  mutate(state_name = factor(state_name, levels = new_case_order)) %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, size = .3) +
  geom_line(mapping = aes(date, us_rate), data = avg, size = .2, col = "dark red") +
  ggtitle("States Hit Hardest by the Pandemic",
          subtitle = "States That Have Had a 7 Day Average of Greater than 40 New Cases per Day Per 100k") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200301), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(axis.text = element_text(size = rel(.4))) +
  facet_wrap(. ~ state_name) +
  theme(strip.text.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 90)) +
  gghighlight(label_key = state_name, use_direct_label = TRUE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  x = ymd(20200501),
                                  y = 35,
                                  hjust = 1,
                                  size = 1.2,
                                  color = "dark blue",
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

p_width <- 6
p_height <- (9/16) * p_width

ggsave("figs/LINE 40 per Cap.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")
