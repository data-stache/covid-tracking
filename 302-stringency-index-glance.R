load('rda/policy.rda')
load('rda/covid.rda')
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(scipen = 999)

dat <- covid %>%
  filter(!state == 'PR') %>%
  left_join(policy) %>%
  select(state, date, new_cases_percap, new_cases_percap_07da, stringency_index, government_index, containment_index, economic_index, mean_index)

st <- 'KY'

dat %>%
  filter(date >= ymd(20200315) & state == st) %>%
  ggplot(aes(date, new_cases_percap)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  geom_line(aes(y = new_cases_percap_07da), size = .35, col="blue") +
  geom_line(aes(y = mean_index), size = .7, col="red4") +
  ggtitle(paste(covid$state_name[covid$state == st], 'New Cases Per 100k')) +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA), ylim = c(0, max(covid$new_cases_percap_07da[covid$state == st], na.rm = TRUE) * 1.1)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

load('rda/covid_us_sum.rda')
head(covid_us_sum)

dat <- policy %>%
  group_by(date) %>%
  summarize(mean_stringency_ind = mean(stringency_index, na.rm = TRUE),
            mean_government_ind = mean(government_index, na.rm = TRUE),
            mean_containment_ind = mean(containment_index, na.rm = TRUE),
            mean_economic_ind = mean(economic_index, na.rm = TRUE)) %>%
  arrange(desc(date)) %>%
  mutate(mean_index = (mean_stringency_ind + mean_government_ind + mean_containment_ind) / 3) %>%
  left_join(covid_us_sum) %>%
  filter(!is.na(new_cases))

P1 <- dat %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_cases)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_bar(stat = "identity", fill="blue", alpha = .2, size = .1) +
  geom_line(aes(y = new_cases_07da), size = .35, col="blue") +
  ggtitle('US New Cases') +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

P2 <- dat %>%
  filter(date >= ymd(20200315)) %>%
  ggplot(aes(date, new_cases)) +
  geom_hline(yintercept=0, col = "grey40", size = .2) +
  geom_line(aes(y = mean_index), size = .7, col="red4") +
  ggtitle('US Mean State Stringnecy Index') +
  labs(caption = "Created by Andrew F. Griffin\nCovid Data from The Covid Tracking Project") +
  scale_x_date(date_labels = "%b", breaks= "1 month", expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  coord_cartesian(xlim = c(ymd(20200315), NA)) +
  theme_DataStache() +
  theme(text = element_text(size = rel(.6)))

grid.arrange(P1, P2, nrow = 2)

