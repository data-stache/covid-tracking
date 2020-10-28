# LOAD DATA
load("rda/covid.rda")
load("rda/covid_state_growth.rda")
load("rda/ind_new_case_state.rda")
load("rda/ind_red_perpos.rda")
load("rda/ind_red_cases.rda")
load("rda/ind_tdy.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# EPIC LINEPLOT OF COVID - FULL PANDEMIC
avg <- covid %>%
  group_by(date) %>%
  summarize(us_rate = sum(new_cases) / sum(pop) * 100000) %>%
  ungroup()

avg <- avg %>%
  mutate(us_rate = rollapply(us_rate, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

new_case_order <- covid_state_growth$state_name[ind_new_case_state]

covid %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, size = .3) +
  ggtitle("Where is Covid Hitting The Hardest?",
          subtitle = "States where both New Cases & Percent Positive are in the Red Zone") +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0, NA), expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200301), ind_tdy)) +
  geom_hline(yintercept=0, col = "grey40", size = .4) +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90)) +
  theme(axis.text = element_text(size = rel(.4))) +
  facet_wrap(. ~ state_name) +
  theme(strip.text.x = element_blank()) +
  gghighlight(label_key = state_name, use_direct_label = TRUE,
              unhighlighted_params = list(size = .25, colour = alpha("dark grey", 0.3)),
              label_params = list(label.size = NA,
                                  x = ymd(20200501),
                                  y = 65,
                                  hjust = 1,
                                  size = 1.2,
                                  color = "dark blue",
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

width <- 6
height <- (9/16) * width

ggsave("figs/STATES FULL PANDEMIC LINE.png",
       width = width,
       height = height, 
       dpi = "retina")


covid %>%
  filter(state %in% ind_red_cases) %>%
  mutate(state_name = factor(state_name, levels = new_case_order)) %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, size = .3) +
  ggtitle("Where Are Covid Cases Rising the Most?",
          subtitle = "States With Greater than 100 New Cases per Week per 100k People") +
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
                                  y = 99,
                                  hjust = 1,
                                  size = 1.2,
                                  color = "dark blue",
                                  fontface = 2,
                                  fill = alpha("white", .3),
                                  segment.size = 0))

width <- 6
height <- (9/16) * width

ggsave("figs/CASES RED ZONE STATES.png",
       width = width,
       height = height, 
       dpi = "retina")
