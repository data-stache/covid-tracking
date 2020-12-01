load("rda/covid.rda")
load('rda/policy.rda')
load("rda/populations.rda")
load("rda/theme_DataStache.rda")
options(digits = 3)

# MERGE DATA
policy <- policy %>%
  select(-state_name)

covid %>%
  filter(!state == 'PR') %>%
  left_join(policy) %>%
  select(state_name, state, date, day, pop, new_cases_percap, new_tests_percap, new_death_percap, hosp_percap, stringency_index, government_index, containment_index, economic_index, mean_index) %>%
  filter(state == 'CT') %>%
  ggplot(aes(x = date)) +
  geom_line(aes(y = stringency_index), col = 'blue4') +
  geom_line(aes(y = government_index), col = 'green4') +
  geom_line(aes(y = containment_index), col = 'orange4') +
  geom_line(aes(y = economic_index), col = 'cyan4') +
  geom_line(aes(y = new_cases_percap), col = 'red4', size = 2)

covid %>%
  filter(!state == 'PR' & date >= ymd(20201101)) %>%
  left_join(policy) %>%
  select(state, date, pop, new_cases, stringency_index, government_index, containment_index, economic_index, mean_index) %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases, na.rm = TRUE) / unique(pop) * 100000,
            s_index = mean(stringency_index, na.rm = TRUE),
            g_index = mean(government_index, na.rm = TRUE),
            c_index = mean(containment_index, na.rm = TRUE),
            e_index = mean(economic_index, na.rm = TRUE),
            m_index = mean(mean_index, na.rm = TRUE)) %>%
  ggplot(aes(x = m_index, y = cases, label = state)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  geom_text_repel()
  
  
