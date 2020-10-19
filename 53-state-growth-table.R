# LOAD DATA
load("rda/covid_state_growth.rda")
load("rda/covid_state_zones.rda")

covid_state_table <- covid_state_growth %>%
  left_join(covid_state_zones) %>%
  arrange(desc(sum_cases_percap))

covid_state_table %>% head

### USA TABLE
TBL_USA_State_Growth <- covid_state_table %>%
  select(state_name,
         sum_cases_percap,
         sum_tests_percap,
         percent_pos,
         sum_death_percap,
         cases_zone,
         percent_zone) %>%
  gt() %>% 
  ## LABELS STUB / TITLE / SUBTITLE
  tab_options(table.font.size = pct(75)) %>%
  tab_header(title = md("**COVID GROWTH BY STATE IN THE USA**"),  
             subtitle = md("*Totals Per 100k People in the Last Week*")) %>%
  ## RENAMES COLUMNS
  cols_label(state_name = md("**State**"), 
             sum_cases_percap = md("**New Cases**"),
             sum_tests_percap = md("**New Tests**"),
             percent_pos = md("**Percent Positive**"),
             sum_death_percap = md("**New Deaths**"),
             cases_zone = md("**Zone by Cases**"),
             percent_zone = md("**Zone by Positivity**")) %>%
  fmt_percent(columns = vars(percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  ## FORMATS DATA
  fmt_number(columns =  vars(sum_cases_percap,
                             sum_tests_percap,
                             sum_death_percap),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(state_name))) %>% 
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Data from The Covid Tracking Project*"))

TBL_USA_State_Growth
TBL_USA_State_Growth %>%
  gtsave("TBL_USA_State_Growth.html", path = "/Users/andrewgriffin/projects/covid-tracking/figs")
