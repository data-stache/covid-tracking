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
  mutate(cases = new_cases / pop * 100000,
         pvi = pvi / 1000,
         pvi_z = (pvi - mean(pvi)) / sd(pvi),
         slpli_z = (party_by - mean(party_by, na.rm = TRUE)) / sd(party_by, na.rm = TRUE),
         lean_z = (pvi_z + slpli_z),
         state_lean = case_when(lean_z < 0 ~ "D",
                                lean_z == 0 ~"I",
                                lean_z > 0 ~ "R"))


##### CASES PER CAPITA VS POLITICAL LEANINGS (FULL PANDEMIC) #####
# TOTAL CASES OF COVID
covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_z)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm")

# CASES OF COVID PER MONTH
covid_pol_M <- covid_pol %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            lean_z = unique(lean_z)) %>%
  ungroup()

covid_pol_M %>%
  filter(month >= 5) %>%
  ggplot(aes(x = lean_z, y = cases, label = state)) +
  geom_point(alpha = .5) +
  geom_smooth(method = "lm")

covid_pol_M %>%
  group_by(month) %>%
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
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-lean-cases-correl.png",
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
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Death Toll") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-death-correl.png",
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
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Percent Share Positive Tests") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-percent-correl.png",
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
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-testing-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina") 

##### BOX PLOTS#####
# CASES PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR', 'SepD', 'SepR', 'OctD', 'OctR'))) %>%
  ggplot(aes(x = bx_month, y = cases, fill = lean)) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank()) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold")) +
  ggtitle("Impact of State Political Lean on New Cases",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-cases-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# DEATHS PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR', 'SepD', 'SepR', 'OctD', 'OctR'))) %>%
  ggplot(aes(x = bx_month, y = death, fill = lean)) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank()) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold")) +
  ggtitle("Impact of State Political Lean on New Deaths",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-deaths-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TESTS PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR', 'SepD', 'SepR', 'OctD', 'OctR'))) %>%
  ggplot(aes(x = bx_month, y = tests, fill = lean)) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank()) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold")) +
  ggtitle("Impact of State Political Lean on New Testing",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-testing-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# POSITIVES PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(pos = mean(percent_pos),
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR', 'SepD', 'SepR', 'OctD', 'OctR'))) %>%
  ggplot(aes(x = bx_month, y = pos, fill = lean)) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank()) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold")) +
  ggtitle("Impact of State Political Lean on Average Positive Tests",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-positives-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")



