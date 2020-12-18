# LOAD DATA
load("rda/covid.rda")
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(digits = 3)

death_total <- covid %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000) %>%
  arrange(desc(deaths)) %>%
  .$state

covid %>%
  mutate(state = factor(state, levels = death_total)) %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000) %>%
  ggplot(aes(x = state, y = deaths)) +
  geom_bar(stat = 'identity')

death_total <- covid %>%
  filter(date >= ymd(20200701)) %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000) %>%
  arrange(desc(deaths)) %>%
  .$state

covid %>%
  filter(date >= ymd(20200701)) %>%
  mutate(state = factor(state, levels = death_total)) %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000) %>%
  ggplot(aes(x = state, y = deaths)) +
  geom_bar(stat = 'identity')
