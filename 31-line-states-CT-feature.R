# LOAD DATA
load("rda/covid.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# EPIC LINEPLOT OF COVID - FULL PANDEMIC
st <- c("CT")

avg_st <- covid %>%
  filter(state %in% st) %>%
  select(date, new_cases_percap_07da, state)

avg <- covid %>%
  group_by(date) %>%
  summarize(us_rate = sum(new_cases) / sum(pop) * 100000) %>%
  ungroup()

avg <- avg %>%
  mutate(us_rate = rollapply(us_rate, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right"))

covid %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_percap_07da, group = state), color = "dark blue", 
            show.legend = FALSE, alpha = 0.15, size = .5) +
  geom_line(mapping = aes(date, us_rate), data = avg, size = 1, col = "dark blue") +
  geom_line(mapping = aes(date, new_cases_percap_07da, group = state), data = avg_st, size = 1, col = "dark red") +
  ggtitle("New Cases per 100,000 by state",
          subtitle = paste(st, sep = " ", "Featured in red")) +
  labs(caption = "Created by Andrew F. Griffin, Covid Data from The Covid Tracking Project") +
  xlab("Date") +
  ylab("New Cases Per 100k") +
  scale_x_date(date_labels = "%b", breaks= "1 month") +
  scale_y_continuous(breaks = seq(0, 200, 10), limits = c(0, NA)) +
  theme_DataStache() 
