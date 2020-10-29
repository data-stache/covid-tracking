##### DATA LOAD #####
library(lubridate)
library(broom)
load("rda/covid_pol.rda")
load("rda/slpli.rda")
load("rda/theme_DataStache.rda")

pol_party <- c("blue", "dark red")

# APPLY CA SLPLI RATING TO DC TO POL DF
dc_ind <- covid_pol$state == "DC"
max_dem <- covid_pol %>%
  filter(SLPLI == "Democrat") %>%
  .$party_by
max_dem <- max(max_dem, na.rm = TRUE)
covid_pol$party_by[dc_ind] <- max_dem



##### PROCESSING #####
covid_pol <- covid_pol %>%
  # REMOVE PUERTO RICO
  filter(!state == "PR") %>%
  # ALTER PARTY BY TO (-) DEM and (+) REP
  mutate(party_by = ifelse(SLPLI == "Democrat", party_by * (-1), party_by),
         party_by = party_by * 100)

##### CASES PER CAPITA VS POLITICAL LEANINGS (FULL PANDEMIC) #####
covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(party_by)) %>%
  ggplot(aes(x = partisan, y = cases, label = state)) +
  geom_point(alpha = .5) +
  geom_text_repel() +
  geom_smooth(method = "lm")

covid_pol_M <- covid_pol %>%
  group_by(state) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(party_by))

covid_pol_M %>%
  summarise(cor = cor(cases, partisan))

model <- lm(cases ~ partisan, data = covid_pol_M)
model

coefs <- tidy(model, conf.int = TRUE)
coefs


##### SLPLI -- GRAPHS PER CAPITA VS POLITICAL LEANINGS STRATIFIED BY MONTH #####
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(party_by)) %>%
  ggplot(aes(x = partisan, y = cases)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(-50,50), ylim = c(0,NA)) +
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

ggsave("figs/cases-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            partisan = unique(party_by)) %>%
  ggplot(aes(x = partisan, y = death)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(-50,50)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Death Toll") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/death-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(pos = sum(new_cases) / sum(new_tests) * 100,
            partisan = unique(party_by)) %>%
  ggplot(aes(x = partisan, y = pos)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(-50,50), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Percent Share Positive Tests") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/percent-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(party_by)) %>%
  ggplot(aes(x = partisan, y = tests)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(-50,50), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Testing") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/testing-correl.png",
       width = p_width,
       height = p_height, 
       dpi = "retina") 

##### COOK PVI -- GRAPHS PER CAPITA VS POLITICAL LEANINGS STRATIFIED BY MONTH #####
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            partisan = unique(pvi)) %>%
  ggplot(aes(x = partisan, y = cases)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$pvi) * 1.1, max(covid_pol$pvi) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Case Load - BASED ON COOK PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

p_width <- 12
p_height <- (9/16) * p_width

ggsave("figs/cases-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(death = sum(new_death) / unique(pop) * 100000,
            partisan = unique(pvi)) %>%
  ggplot(aes(x = partisan, y = death)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$pvi) * 1.1, max(covid_pol$pvi) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Death Toll - BASED ON COOK PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/death-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")

covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(pos = sum(new_cases) / sum(new_tests) * 100,
            partisan = unique(pvi)) %>%
  ggplot(aes(x = partisan, y = pos)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$pvi) * 1.1, max(covid_pol$pvi) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact Percent Share Positive Tests - BASED ON COOK PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/percent-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina")


covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date, label = TRUE)) %>%
  group_by(state, month) %>%
  summarize(tests = sum(new_tests) / unique(pop) * 100000,
            partisan = unique(pvi)) %>%
  ggplot(aes(x = partisan, y = tests)) +
  geom_vline(xintercept = 0, size = .5, col = "40grey") +
  geom_hline(yintercept = 0, size = .5, col = "40grey") +
  geom_point(color = "dark blue", alpha = .5) +
  geom_smooth(method = "lm") +
  coord_cartesian(xlim = c(min(covid_pol$pvi) * 1.1, max(covid_pol$pvi) * 1.1), ylim = c(0,NA)) +
  theme_DataStache() +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=.7)) +
  facet_wrap(. ~ month, strip.position="bottom") +
  theme(strip.text.x = element_text(size = rel(.8),
                                    face = "bold",
                                    margin = margin(rel(.5), rel(1), rel(.5), rel(1))),
        strip.background = element_rect(colour = "black", size=.7)) +
  ggtitle("Does Political Leaning Impact New Testing - BASED ON COOK PVI") +
  labs(caption = "Created by Andrew F. Griffin\nData The Covid Tracking Project")

ggsave("figs/testing-correl-pvi.png",
       width = p_width,
       height = p_height, 
       dpi = "retina") 


# CASES PER CAPITA BY MONTH AND GOVERNOR PARTY
covid_pol %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            gov = unique(governor_party)) %>%
  ggplot(aes(x = month, y = cases, col = gov)) +
  geom_boxplot(aes(group = month)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .1) +
  facet_wrap(. ~ gov)

# CASES PER CAPITA BY MONTH AND SLPLI
covid_pol %>%
  filter(date >= ymd(20200301)) %>%
  mutate(month = month(date)) %>%
  group_by(state, month) %>%
  summarize(cases = sum(new_cases) / unique(pop) * 100000,
            slpli = unique(slpli_party)) %>%
  ggplot(aes(x = month, y = cases, col = slpli)) +
  geom_boxplot(aes(group = month)) +
  scale_x_continuous(breaks = seq(1, 12, 1)) +
  scale_color_manual(values = pol_party) +
  geom_jitter(alpha = .5, width = .15) +
  facet_wrap(. ~ slpli)


