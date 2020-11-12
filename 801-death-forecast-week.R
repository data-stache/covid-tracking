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

# MEANS
mus <- deaths %>% group_by(day) %>% summarize(mu = mean(change))
mu_Sn <- mus$mu[1]
mu_M <- mus$mu[2]
mu_Tu <- mus$mu[3]
mu_W <- mus$mu[4]
mu_Th <- mus$mu[5]
mu_F <- mus$mu[6]
mu_St <- mus$mu[7]

# STANDARD DEVIATIONS
sds <- deaths %>% group_by(day) %>% summarize(sd = sd(change))
sd_Sn <- sds$sd[1]
sd_M <- sds$sd[2]
sd_Tu <- sds$sd[3]
sd_W <- sds$sd[4]
sd_Th <- sds$sd[5]
sd_F <- sds$sd[6]
sd_St <- sds$sd[7]

# Ns
Ns <- deaths %>% group_by(day) %>% summarize(Ns = n())
N_Sn <- Ns$Ns[1]
N_M <- Ns$Ns[2]
N_Tu <- Ns$Ns[3]
N_W <- Ns$Ns[4]
N_Th <- Ns$Ns[5]
N_F <- Ns$Ns[6]
N_St <- Ns$Ns[7]

# ERRORS
se_M <- qnorm(.975) * sd_M / sqrt(N_M)
se_Tu <- qnorm(.975) * sd_Tu / sqrt(N_Tu)
se_W <- qnorm(.975) * sd_W / sqrt(N_W)
se_Th <- qnorm(.975) * sd_Th / sqrt(N_Th)
se_F <- qnorm(.975) * sd_F / sqrt(N_F)
se_St <- qnorm(.975) * sd_St / sqrt(N_St)

# DATES
sun <- covid_us_sum %>%
  group_by(day) %>%
  filter(day == "Sunday" & date == max(date)) %>%
  select(new_death, date)

mon <- covid_us_sum %>%
  filter(day == "Monday") %>%
  select(new_death, date) %>%
  slice(1)

tues <- covid_us_sum %>%
  filter(day == "Tuesday") %>%
  select(new_death, date) %>%
  slice(1)

wed <- covid_us_sum %>%
  filter(day == "Wednesday") %>%
  select(new_death, date) %>%
  slice(1)

thurs <- covid_us_sum %>%
  filter(day == "Thursday") %>%
  select(new_death, date) %>%
  slice(1)

fri <- covid_us_sum %>%
  filter(day == "Friday") %>%
  select(new_death, date) %>%
  slice(1)

sat <- covid_us_sum %>%
  filter(day == "Saturday") %>%
  select(new_death, date) %>%
  slice(1)

Day <- c("Mon", "Tues", "Wed", "Thurs", "Fri", "Sat")
Date <- as.Date(c(ifelse(mon$date >= sun$date, mon$date, NA),
                  ifelse(tues$date >= sun$date, tues$date, NA),
                  ifelse(wed$date >= sun$date, wed$date, NA),
                  ifelse(thurs$date >= sun$date, thurs$date, NA),
                  ifelse(fri$date >= sun$date, fri$date, NA),
                  ifelse(sat$date >= sun$date, sat$date, NA)))
Model <- c(sun$new_death * mu_M, sun$new_death * mu_Tu, sun$new_death * mu_W, sun$new_death * mu_Th, sun$new_death * mu_F, sun$new_death * mu_St)
High <- c(sun$new_death * (mu_M + se_M), sun$new_death * (mu_Tu + se_Tu), sun$new_death * (mu_W + se_W), sun$new_death * (mu_Th + se_Th), sun$new_death * (mu_F + se_F), 
          sun$new_death * (mu_St + se_St)) 
Low <- c(sun$new_death * (mu_M - se_M), sun$new_death * (mu_Tu - se_Tu), sun$new_death * (mu_W - se_W), sun$new_death * (mu_Th - se_Th), sun$new_death * (mu_F - se_F), 
         sun$new_death * (mu_St - se_St))
Actual <- c(ifelse(mon$date >= sun$date, mon$new_death, NA),
            ifelse(tues$date >= sun$date, tues$new_death, NA),
            ifelse(wed$date >= sun$date, wed$new_death, NA),
            ifelse(thurs$date >= sun$date, thurs$new_death, NA),
            ifelse(fri$date >= sun$date, fri$new_death, NA),
            ifelse(sat$date >= sun$date, sat$new_death, NA))

this_week <- data.frame(Day, Date, Low, Model, High, Actual) %>%
  mutate(hit = ifelse(Actual >= Low & Actual <= High, TRUE, FALSE),
         lo_miss = ifelse(hit == FALSE, (Actual - Low) / Low, NA),
         miss = ifelse(hit == FALSE, (Actual - Model) / Model, NA),
         hi_miss = ifelse(hit == FALSE, (Actual - High) / High, NA))

this_week
this_week %>%
  summarise(low_avg = mean(Low),
            model_avg = mean(Model),
            high_avg = mean(High),
            low_sum = sum(Low),
            model_sum = sum(Model),
            high_sum = sum(High))
