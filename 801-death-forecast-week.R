load("rda/covid_us_sum.rda")
options(digits = 3)

deaths <- covid_us_sum %>%
  filter(date >= ymd(20201001)) %>%
  mutate(week = epiweek(date),
         day = factor(day, levels = c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))) %>% 
  group_by(week) %>%
  mutate(change = new_death / last(new_death)) %>%
  filter(!is.na(change) & !is.infinite(change)) %>%
  select(week, day, new_death, change) 

##### MODEL GUTS #####
dat <- deaths %>% 
  group_by(day) %>% 
  summarize(mu = mean(change),
            sd = sd(change),
            Ns = n(),
            se = qnorm(.975) * sd / sqrt(Ns))

##### DATES #####
sun <- covid_us_sum %>%
  group_by(day) %>%
  filter(day == "Sunday" & date == max(date)) %>%
  select(new_death, date) %>%
  ungroup()

mon <- covid_us_sum %>%
  filter(day == "Monday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

tues <- covid_us_sum %>%
  filter(day == "Tuesday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

wed <- covid_us_sum %>%
  filter(day == "Wednesday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

thurs <- covid_us_sum %>%
  filter(day == "Thursday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

fri <- covid_us_sum %>%
  filter(day == "Friday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

sat <- covid_us_sum %>%
  filter(day == "Saturday") %>%
  select(new_death, date) %>%
  slice(1) %>%
  ungroup()

##### MODEL #####

Day <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")

Date <- as.Date(c(ifelse(mon$date >= sun$date, mon$date, NA),
                  ifelse(tues$date >= sun$date, tues$date, NA),
                  ifelse(wed$date >= sun$date, wed$date, NA),
                  ifelse(thurs$date >= sun$date, thurs$date, NA),
                  ifelse(fri$date >= sun$date, fri$date, NA),
                  ifelse(sat$date >= sun$date, sat$date, NA)))

Model <- c(sun$new_death * dat$mu[2],
           sun$new_death * dat$mu[3],
           sun$new_death * dat$mu[4],
           sun$new_death * dat$mu[5],
           sun$new_death * dat$mu[6],
           sun$new_death * dat$mu[7])

High <- c(sun$new_death * (dat$mu[2] + dat$se[2]),
          sun$new_death * (dat$mu[3] + dat$se[3]),
          sun$new_death * (dat$mu[4] + dat$se[4]),
          sun$new_death * (dat$mu[5] + dat$se[5]),
          sun$new_death * (dat$mu[6] + dat$se[6]), 
          sun$new_death * (dat$mu[7] + dat$se[7]))

Low <- c(sun$new_death * (dat$mu[2] - dat$se[2]),
         sun$new_death * (dat$mu[3] - dat$se[3]),
         sun$new_death * (dat$mu[4] - dat$se[4]),
         sun$new_death * (dat$mu[5] - dat$se[5]),
         sun$new_death * (dat$mu[6] - dat$se[6]), 
         sun$new_death * (dat$mu[7] - dat$se[7]))

Actual <- c(ifelse(mon$date >= sun$date, mon$new_death, NA),
            ifelse(tues$date >= sun$date, tues$new_death, NA),
            ifelse(wed$date >= sun$date, wed$new_death, NA),
            ifelse(thurs$date >= sun$date, thurs$new_death, NA),
            ifelse(fri$date >= sun$date, fri$new_death, NA),
            ifelse(sat$date >= sun$date, sat$new_death, NA))

this_week <- data.frame(Day, Date, Low, Model, High, Actual) %>%
  mutate(hit = ifelse(Actual >= Low & Actual <= High, TRUE, FALSE),
         lo_miss = ifelse(hit == FALSE, ifelse(Actual > High, NA, (Actual - Low) / Low), NA),
         miss = ifelse(hit == FALSE, (Actual - Model) / Model, NA),
         hi_miss = ifelse(hit == FALSE, ifelse(Actual < Low, NA, (Actual - High) / High), NA))

##### MODEL OUTPUT #####

this_week %>%
  kable()

this_week %>%
  summarise(low_avg = mean(Low),
            model_avg = mean(Model),
            high_avg = mean(High),
            low_sum = sum(Low),
            model_sum = sum(Model),
            high_sum = sum(High)) %>%
  kable()
