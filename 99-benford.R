library(benford.analysis)
load("rda/covid.rda")

# Pick a State
st <- "NY"
##### COVID BENFORD CASES #####

covid_ben <- covid %>%
  filter(state == st & !is.na(new_cases)) %>%
  select(state, new_cases)

p_cov_ben <- benford(covid_ben$new_cases, number.of.digits = 1, discrete = TRUE, sign = "both")

plot(p_cov_ben)
p_cov_ben



covid_ben <- covid %>%
  filter(state == st & !is.na(new_tests)) %>%
  select(state, new_tests)

p_cov_ben <- benford(covid_ben$new_tests, number.of.digits = 1, discrete = TRUE, sign = "both")

plot(p_cov_ben)
p_cov_ben
