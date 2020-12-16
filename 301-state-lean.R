##### DATA LOAD #####
library(lubridate)
library(broom)
library(ggrepel)
library(tidyverse)
load("rda/covid_pol.rda")
load("/Users/andrewgriffin/projects/zConstants/rda/theme_DataStache.rda")

options(scipen = 999)

pol_party <- c("blue", "dark red")

##### PROCESSING #####
covid_pol <- covid_pol %>%
  # FILTER ONLY COMPLETE MONTHS
#  filter(date < ymd(20201101)) %>%
  # CHANGE SLPLI TO - DEM and + REP
  # THIN COLUMNS
  select(date, state, state_name, pop, new_cases, new_tests, new_death, hosp, percent_pos, lean_avg, state_lean, party_by, pvi) %>%
  # MAKE PVI A DECIMAL and Z SCORES
  mutate(cases = new_cases / pop * 100000)
head(covid_pol)

##### CASES PER CAPITA VS POLITICAL LEANINGS (FULL PANDEMIC) #####
# TOTAL CASES OF COVID
covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/state-lean-cases_entire_pandemic.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL CASES OF COVID SINCE JULY
covid_pol %>%
  group_by(state) %>%
  filter(date >= ymd(20200701)) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Case Load (Since July 2020)") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-cases_july.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL DEATHS OF COVID
covid_pol %>%
  group_by(state) %>%
  summarize(deaths = sum(new_death) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = deaths, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-deaths_entrie_pandemic.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL DEATHS OF COVID SINCE JULY
covid_pol %>%
  group_by(state) %>%
  filter(date >= ymd(20200701)) %>%
  summarize(deaths = sum(new_death) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = deaths, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Deaths (Since July 2020)") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-deaths_july.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TOTAL TESTS OF COVID
covid_pol %>%
  group_by(state) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = tests, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-testing.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# AVG COVID HOSPITALIZATION
covid_pol %>%
  group_by(state) %>%
  summarize(hosp = mean(hosp, na.rm = TRUE) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = hosp, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm") +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Hospitalization") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-hospitalization.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


##### STATE LEAN -- GRAPHS PICK A MONTH #####
mth <- 'Dec'

# CASES
covid_pol %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Case Load") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

# DEATHS
covid_pol %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = death, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Death Toll") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

# PERCENT POSITIVE
covid_pol %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(pos = sum(new_cases) / sum(new_tests) * 100,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = pos, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Percent Share Positive Tests") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

# TESTING
covid_pol %>%
  mutate(month = month(date, label = TRUE)) %>%
  filter(month == mth) %>%
  group_by(state) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = tests, label = state)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  geom_text_repel() +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact New Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")


##### STATE LEAN -- GRAPHS PER CAPITA VS POLITICAL LEANINGS STRATIFIED BY MONTH #####
# CASES
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = cases)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
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

ggsave("figs/state-lean-cases-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# DEATHS
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = death)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
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

# HOSPITALIZATION
covid_pol %>%
  filter(date >= ymd(20200401)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(hosp = mean(hosp) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = hosp)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
  ggtitle("Does Political Leaning Impact Hospitalization Rates") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")

ggsave("figs/state-lean-hospitalization-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# TESTS
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(lean_avg)) %>%
  ggplot(aes(x = partisan, y = tests)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$lean_avg) * 1.1, max(covid_pol$lean_avg) * 1.1), ylim = c(0,NA)) +
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
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR',
                                                'SepD', 'SepR', 'OctD', 'OctR', 'NovD', 'NovR', 'DecD', 'DecR'))) %>%
  ggplot(aes(x = bx_month, y = cases, fill = lean)) +
  geom_hline(yintercept = 0, col = "grey 60", size = .5) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
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
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR',
                                                'SepD', 'SepR', 'OctD', 'OctR', 'NovD', 'NovR', 'DecD', 'DecR'))) %>%
  ggplot(aes(x = bx_month, y = death, fill = lean)) +
  geom_hline(yintercept = 0, col = "grey 60", size = .5) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
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
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR',
                                                'SepD', 'SepR', 'OctD', 'OctR', 'NovD', 'NovR', 'DecD', 'DecR'))) %>%
  ggplot(aes(x = bx_month, y = tests, fill = lean)) +
  geom_hline(yintercept = 0, col = "grey 60", size = .5) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
  ggtitle("Impact of State Political Lean on New Testing",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-testing-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

# HOSPiTALIZATION PER CAPITA BY MONTH AND LEAN
covid_pol %>%
  filter(date >= ymd(20200401)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(hosp = mean(hosp),
            lean = unique(state_lean)) %>%
  ungroup() %>%
  mutate(bx_month = paste(month, lean, sep = ""),
         bx_month = factor(bx_month, levels = c("MarD", "MarR", "AprD", "AprR","MayD", 'MayR', 'JunD', 'JunR', 'JulD', 'JulR', 'AugD', 'AugR',
                                                'SepD', 'SepR', 'OctD', 'OctR', 'NovD', 'NovR', 'DecD', 'DecR'))) %>%
  ggplot(aes(x = bx_month, y = hosp, fill = lean)) +
  geom_hline(yintercept = 0, col = "grey 60", size = .5) +
  geom_boxplot(aes(group = bx_month), alpha = .5) +
  scale_color_manual(values = pol_party) +
  scale_fill_manual(values = pol_party) +
  geom_jitter(aes(col = lean), alpha = .5, width = .2) +
  theme_DataStache() +
  theme(axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.border = element_rect(colour = "grey 80", fill=NA, size=.7)) +
  facet_grid(. ~ month, scales = "free_x", switch = 'x') +
  theme(strip.text.x = element_text(face = "bold"),
        strip.background = element_rect(colour = "grey 80", size=.7)) +
  ggtitle("Impact of State Political Lean on Average Hospitalization",
          subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData From The Covid Tracking Project")

ggsave("figs/state-lean-hospitalization-box-plot.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

##### TIME SERIES PLOTS #####
# CASES
library(zoo)
dat_ends <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(cases = sum(new_cases) / sum(unique(pop)) * 100000) %>%
  mutate(cases = rollapply(cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  filter(date == max(date))

P_cases <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(cases = sum(new_cases) / sum(unique(pop)) * 100000) %>%
  mutate(cases = rollapply(cases, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ggplot(aes(x = date, y = cases, col = state_lean)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_line() +
  geom_point(aes(y = cases), data = dat_ends) +
  geom_text(aes(label = round(cases, 1)), data = dat_ends, nudge_x = 3, hjust = 0) +
  scale_color_manual(values = pol_party) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Covid Cases") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Cases Per 100k People in Democratic vs Republican Leaning States\nLean based on FiveThirtyEight SLPLI and Cook PVI")
P_cases

# DEATHS
dat_ends <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(deaths = sum(new_death) / sum(unique(pop)) * 100000) %>%
  mutate(deaths = rollapply(deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  filter(date == max(date))

P_deaths <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(deaths = sum(new_death) / sum(unique(pop)) * 100000) %>%
  mutate(deaths = rollapply(deaths, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ggplot(aes(x = date, y = deaths, col = state_lean)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_line() +
  geom_point(aes(y = deaths), data = dat_ends) +
  geom_text(aes(label = round(deaths, 1)), data = dat_ends, nudge_x = 3, hjust = 0) +
  scale_color_manual(values = pol_party) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Covid Deaths") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Deaths Per 100k People in Democratic vs Republican Leaning States\nLean based on FiveThirtyEight SLPLI and Cook PVI")
P_deaths

# TESTING
dat_ends <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(tests = sum(new_tests) / sum(unique(pop)) * 100000) %>%
  mutate(tests = rollapply(tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  filter(date == max(date))

P_tests <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(tests = sum(new_tests) / sum(unique(pop)) * 100000) %>%
  mutate(tests = rollapply(tests, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ggplot(aes(x = date, y = tests, col = state_lean)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_line() +
  geom_point(aes(y = tests), data = dat_ends) +
  geom_text(aes(label = round(tests, 1)), data = dat_ends, nudge_x = 3, hjust = 0) +
  scale_color_manual(values = pol_party) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Covid Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Tests Per 100k People in Democratic vs Republican Leaning States\nLean based on FiveThirtyEight SLPLI and Cook PVI")
P_tests

# POSITIVE
dat_ends <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(pos = sum(new_cases, na.rm = TRUE) / sum(new_tests, na.rm = TRUE)) %>%
  mutate(pos = rollapply(pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  filter(date == max(date))

P_pos <- covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  group_by(state_lean, date) %>%
  summarize(pos = sum(new_cases, na.rm = TRUE) / sum(new_tests, na.rm = TRUE)) %>%
  mutate(pos = rollapply(pos, width = 7, FUN=function(x) mean(x, na.rm=TRUE), by=1, by.column=TRUE, partial=TRUE, fill=NA, align="right")) %>%
  ggplot(aes(x = date, y = pos, col = state_lean)) +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
#  geom_ribbon(aes(ymin = 0, ymax = .05), fill = 'green', alpha = .3) +
#  geom_ribbon(aes(ymin = .05, ymax = .1), fill = 'yellow', alpha = .3) +
#  geom_ribbon(aes(ymin = .1, ymax = 1), fill = 'red4', alpha = .3) +
  geom_line() +
  geom_point(aes(y = pos), data = dat_ends) +
  geom_text(aes(label = round(pos, 2)), data = dat_ends, nudge_x = 3, hjust = 0) +
  scale_color_manual(values = pol_party) +
  coord_cartesian(ylim = c(0, .3)) +
  theme_DataStache() +
  ggtitle("Does Political Leaning Impact Percent Positive") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project",
       subtitle = "Lean based on FiveThirtyEight SLPLI and Cook PVI")
P_pos

library(gridExtra)
grid.arrange(P_cases, P_tests, P_deaths, P_pos, nrow = 2)
G <- arrangeGrob(P_cases, P_tests, P_deaths, P_pos, nrow = 2)

ggsave("figs/state-lean-time-series.png",
       G,
       width = p_width,
       height = p_height, 
       dpi = "retina")

