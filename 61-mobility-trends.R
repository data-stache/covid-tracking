load("rda/mobility.rda")
load("rda/covid.rda")
head(mobility)
head(covid)

dat <- covid %>% inner_join(mobility) %>%
  select(state, date, new_cases_percap, new_cases_percap_07da, retail_recreation_07da, grocery_pharmacy_07da, parks_07da, transit_07da, workplace_07da, residential_07da, mask_law)

names(dat)

cor_state <- dat %>%
  filter(date >= ymd(20200601)) %>%
  group_by(state) %>%
  summarise(ret = cor(retail_recreation_07da, new_cases_percap_07da),
            groc = cor(grocery_pharmacy_07da, new_cases_percap_07da),
            parks = cor(parks_07da, new_cases_percap_07da),
            transit = cor(transit_07da, new_cases_percap_07da),
            work = cor(workplace_07da, new_cases_percap_07da),
            res = cor(residential_07da, new_cases_percap_07da)) %>%
  arrange(desc(res))
cor_state

define_model <- function(data){
  model <- lm(new_cases_percap_07da ~ retail_recreation_07da + grocery_pharmacy_07da + workplace_07da + residential_07da, data = data)
  data.frame(term = names(model$coefficients),
             estimate = model$coefficients, 
             se = summary(model)$coefficient[,2])
}

dat %>%
  filter(date >= ymd(20200601)) %>%
  group_by(state) %>%
  do(define_model(.))

dat %>%
  filter(date >= ymd(20200601)) %>%
  group_by(state) %>% 
  do(tidy(lm(new_cases_percap_07da ~ retail_recreation_07da + grocery_pharmacy_07da + workplace_07da + residential_07da, data = .), conf.int = T)) %>%
  filter(p.value < .05) %>%
  group_by(term) %>%
  summarize(N = n())

dat %>%
  filter(date >= ymd(20200601)) %>%
  group_by(state) %>% 
  do(tidy(lm(new_cases_percap_07da ~ retail_recreation_07da + grocery_pharmacy_07da + workplace_07da + residential_07da, data = .), conf.int = T)) %>%
  filter(term == "workplace_07da") %>%
  arrange(estimate) %>%
  print(n = Inf)



##### MISC #####
any(is.na(cor_state$ret))
any(is.na(cor_state$groc))
any(is.na(cor_state$work))
any(is.na(cor_state$res))

any(is.na(cor_state$parks))
any(is.na(cor_state$transit))


dat %>%
  lm(new_cases_percap_07da ~ retail_recreation_07da + grocery_pharmacy_07da + workplace_07da + residential_07da, data = .)

dat %>%
  filter(date >= ymd(20200701)) %>%
  mutate(work = rank(workplace_07da),
         cases = new_cases_percap_07da) %>%
  select(work, cases) %>%
  ggplot(aes(x = work, y = cases)) +
  geom_point(alpha = .5) +
  geom_smooth()

dat %>%
  ggplot(aes(x=date, y=grocery_pharmacy_07da, col = state)) +
  geom_line() +
  facet_wrap(.~mask_law)



