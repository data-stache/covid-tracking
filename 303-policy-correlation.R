load('rda/policy.rda')
load('rda/covid.rda')
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")
options(scipen = 999)

covid_policy <- covid %>%
  filter(!state == 'PR') %>%
  left_join(policy) %>%
  # THIN COLUMNS
  select(date, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, stringency_index, government_index, containment_index, economic_index, mean_index)

head(covid_policy)

##### CASES PER CAPITA VS POLITICAL LEANINGS (FULL PANDEMIC) #####
library(ggrepel)
# TOTAL CASES OF COVID
covid_policy %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = cases, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Stringency of Covid Policy Impact New Cases") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "State Policy from the Oxford Covid-19 Government Response Tracker")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-policy-cases-entire-pandemic.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL CASES OF COVID SINCE JULY
covid_policy %>%
  filter(date >= ymd(20200701)) %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = cases, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Stringency of Covid Policy Impact New Cases") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "State Policy from the Oxford Covid-19 Government Response Tracker")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-policy-cases-ejuly.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL DEATHS OF COVID
covid_policy %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = deaths, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Stringency of Covid Policy Impact Covid Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "State Policy from the Oxford Covid-19 Government Response Tracker")

ggsave("figs/state-lean-deaths_entrie_pandemic.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL DEATHS OF COVID SINCE JULY
covid_policy %>%
  filter(date >= ymd(20200701)) %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = deaths, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Stringency of Covid Policy Impact Covid Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "State Policy from the Oxford Covid-19 Government Response Tracker")

ggsave("figs/state-lean-deaths_july.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


# AVG COVID HOSPITALIZATION
covid_policy %>%
  filter(date >= ymd(20200701)) %>%
  group_by(state) %>%
  summarize(hosp = mean(hosp, na.rm = TRUE) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = hosp, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Stringency of Covid Policy Impact Covid Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "State Policy from the Oxford Covid-19 Government Response Tracker")

ggsave("figs/state-lean-hospitalization-july.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


##### STATE LEAN -- GRAPHS PICK A MONTH #####
mth <- 'Dec'

# CASES
covid_policy %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = cases, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

# DEATHS
covid_policy %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = deaths, label = state)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")


##### STATE LEAN -- GRAPHS PER CAPITA VS POLITICAL LEANINGS STRATIFIED BY MONTH #####
# CASES
covid_policy %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) /pop[1] * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = cases)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-policy-cases-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# DEATHS
covid_policy %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            policy = mean(mean_index)) %>%
  ggplot(aes(x = policy, y = death)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
  ggtitle("Does Political Leaning Impact Death Toll") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-death-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")
