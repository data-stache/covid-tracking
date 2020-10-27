# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/theme_DataStache.rda")

##### CASES DAY OF THE WEEK #####
# Box Plot of New Cases Day of Week over Time
covid_us_sum %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_cases)) +
  geom_boxplot() +
  geom_point() +
  theme_DataStache()

##### DEATHS DAY OF THE WEEK #####
# Box Plot of New Deaths Day of Week over Time
covid_us_sum %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_death)) +
  geom_boxplot() +
  geom_point() +
  theme_DataStache()


##### TESTS DAY OF THE WEEK #####
# Box Plot of New Tests Day of Week over Time
covid_us_sum %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_tests)) +
  geom_boxplot() +
  geom_point() +
  theme_DataStache()


##### POSITIVE DAY OF THE WEEK #####
# Box Plot of New Deaths Day of Week over Time
covid_us_sum %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = percent_pos)) +
  geom_boxplot() +
  geom_point() +
  theme_DataStache()

##### NEW HOSP DAY OF THE WEEK #####
# Box Plot of New Hospitalizations Day of Week over Time
covid_us_sum %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(weeks = min(date)) %>%
  filter(date >= ymd(20200901)) %>%
  ggplot(aes(x = day, y = new_hosp)) +
  geom_boxplot() +
  geom_point() +
  theme_DataStache()



##### LINE GRAPHS FOR DAY OF THE WEEK #####
# New Cases Compared to this Day of the week
Day <- covid_us_sum$day[1]

# CASES
covid_us_sum %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = new_cases)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  coord_cartesian(ylim = c(0, NA)) +
  ggtitle(paste("United States New Cases Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nData from the Covid Tracking Project") +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90))

# DEATHS
covid_us_sum %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = new_death)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  coord_cartesian(ylim = c(0, NA)) +
  ggtitle(paste("United States New Deaths Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nData from the Covid Tracking Project") +
  theme_DataStache()+
  theme(axis.text.x = element_text(angle = 90))

# TESTS
covid_us_sum %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = new_tests)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  coord_cartesian(ylim = c(0, NA)) +
  ggtitle(paste("United States New Tests Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nData from the Covid Tracking Project") +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90))

# POSITIVE
covid_us_sum %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = percent_pos)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  coord_cartesian(ylim = c(0, NA)) +
  ggtitle(paste("United States Percent Positive Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nData from the Covid Tracking Project") +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90))

# HOSP
covid_us_sum %>%
  filter(day == day[1]) %>%
  filter(date >= ymd(20200801)) %>%
  ggplot(aes(x = as.factor(date), y = new_hosp)) +
  geom_hline(yintercept = 0, size = .2, color = "grey40") +
  geom_line(group=1, color = "dark blue", size = .3) +
  geom_point(color = "dark blue") +
  ggtitle(paste("United States New Hospitalization Compared to other", Day)) +
  labs(caption = "Created by Andrew F. Griffin\nData from the Covid Tracking Project") +
  theme_DataStache() +
  theme(axis.text.x = element_text(angle = 90))

