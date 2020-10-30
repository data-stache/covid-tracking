##### DATA LOAD #####
library(lubridate)
library(broom)
library(ggrepel)
library(tidyverse)
load("rda/covid_pol.rda")
load("rda/theme_DataStache.rda")

pol_party <- c("blue", "dark red")

##### PROCESSING #####
# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- covid_pol$state == "DC"
max_dem <- covid_pol %>%
  filter(SLPLI == "Democrat") %>%
  .$party_by
max_dem <- max(max_dem, na.rm = TRUE)
covid_pol$party_by[dc_ind] <- max_dem

covid_pol <- covid_pol %>%
  # CHANGE SLPLI TO - DEM and + REP
  mutate(party_by = ifelse(SLPLI == "Democrat", party_by * (-1), party_by)) %>%
  # THIN COLUMNS
  select(date, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, party_by, pvi, mask_law, mask_date) %>%
  # MAKE PVI A DECIMAL and Z SCORES
  mutate(pvi = pvi / 1000,
         pvi_z = (pvi - mean(pvi)) / sd(pvi),
         slpli_z = (party_by - mean(party_by, na.rm = TRUE)) / sd(party_by, na.rm = TRUE),
         lean_z = (pvi_z + slpli_z),
         state_lean = case_when(lean_z < 0 ~ "D",
                                lean_z == 0 ~"I",
                                lean_z > 0 ~ "R"))


##### CASES PER CAPITA VS POLITICAL LEANINGS (FULL PANDEMIC) #####
covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm")

covid_pol_M <- covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            lean_z = unique(lean_z))

covid_pol_M %>%
  summarise(cor = cor(cases, lean_z))

model <- lm(cases ~ lean_z, data = covid_pol_M)
model

coefs <- tidy(model, conf.int = TRUE)
coefs


##### STATE LEAN -- GRAPHS PER CAPITA VS POLITICAL LEANINGS STRATIFIED BY MONTH #####
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = cases)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_z) * 1.1, max(covid_pol$lean_z) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-lean-cases-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = death)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_z) * 1.1, max(covid_pol$lean_z) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Death Toll - BASED ON COOK PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/state-lean-death-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(pos = sum(new_cases) / sum(new_tests) * 100,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = pos)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_z) * 1.1, max(covid_pol$lean_z) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Percent Share Positive Tests") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/state-lean-percent-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = tests)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_z) * 1.1, max(covid_pol$lean_z) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/state-lean-testing-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina") 

##### BOX PLOTS#####
# CASES PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("3D", "3R", "4D", "4R","5D", '5R', '6D', '6R', '7D', '7R', '8D', '8R', '9D', '9R', '10D', '10R'))) %>%
  ggplot(aes(x = bx_month, y = cases, col = lean)) +
  geom_boxplot(aes(group = bx_month)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .25) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme_DataStache()

# DEATHS PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("3D", "3R", "4D", "4R","5D", '5R', '6D', '6R', '7D', '7R', '8D', '8R', '9D', '9R', '10D', '10R'))) %>%
  ggplot(aes(x = bx_month, y = death, col = lean)) +
  geom_boxplot(aes(group = bx_month)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .25) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme_DataStache()

# TESTS PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("3D", "3R", "4D", "4R","5D", '5R', '6D', '6R', '7D', '7R', '8D', '8R', '9D', '9R', '10D', '10R'))) %>%
  ggplot(aes(x = bx_month, y = tests, col = lean)) +
  geom_boxplot(aes(group = bx_month)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .25) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme_DataStache()

# POSITIVES PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(pos = sum(percent_pos) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("3D", "3R", "4D", "4R","5D", '5R', '6D', '6R', '7D', '7R', '8D', '8R', '9D', '9R', '10D', '10R'))) %>%
  ggplot(aes(x = bx_month, y = pos, col = lean)) +
  geom_boxplot(aes(group = bx_month)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .25) +
  coord_cartesian(ylim = c(0,.5)) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme_DataStache()



