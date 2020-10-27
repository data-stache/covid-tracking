# LOAD DATA
load("rda/covid_us_growth.rda")

covid_us_growth

### USA TABLE
TBL_USA_Growth <- covid_us_growth %>%
  select(weeks,
         sum_new_cases,
         sum_new_tests,
         mean_percent_pos,
         sum_new_death) %>%
  gt() %>% 
  ## LABELS STUB / TITLE / SUBTITLE
  tab_options(table.font.size = pct(75)) %>%
  tab_header(title = md("**COVID GROWTH IN THE USA**"),  
             subtitle = md("*Comparing the Last Two Weeks*")) %>%
  ## RENAMES COLUMNS
  cols_label(weeks = md("**Weeks**"), 
             sum_new_cases = md("**Total New Cases**"),
             sum_new_tests = md("**Total New Tests**"),
             mean_percent_pos = md("**Average Percent Positive**"),
             sum_new_death = md("**New Deaths**")) %>%
  fmt_percent(columns = vars(mean_percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  ## FORMATS DATA
  fmt_number(columns =  vars(sum_new_cases,
                             sum_new_tests,
                             sum_new_death),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(weeks))) %>% 
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Data from The Covid Tracking Project*"))

TBL_USA_Growth
TBL_USA_Growth %>%
  gtsave("TBL_USA_Growth.html", path = "/Users/andrewgriffin/projects/covid-tracking/figs")
