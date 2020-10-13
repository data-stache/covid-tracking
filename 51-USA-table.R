# LOAD DATA
load("rda/covid_us_sum.rda")
load("rda/ind_tywm_date.rda")

### USA TABLE
TBL_USA_Daily <- covid_us_sum %>%
  select(date,
         new_cases,
         new_tests,
         percent_pos,
         new_death) %>%
  filter(date %in% ind_tywm_date) %>%
  gt() %>% 
  ## LABELS STUB / TITLE / SUBTITLE
  tab_options(table.font.size = pct(75)) %>%
  tab_header(title = md("**TOTAL COVID REPORTS IN THE USA**"),  
             subtitle = md("*Today, Yesterday, Last Week, Last Month*")) %>%
  ## RENAMES COLUMNS
  cols_label(date = md("**Date**"), 
             new_cases = md("**New Cases**"),
             new_tests = md("**New Tests**"),
             percent_pos = md("**Percent Positive**"),
             new_death = md("**New Deaths**")) %>%
  fmt_percent(columns = vars(percent_pos),
              decimals = 1) %>%
  cols_align(align = "center") %>%
  ## FORMATS DATA
  fmt_number(columns =  vars(new_cases,
                             new_tests,
                             new_death),
             decimals = 0) %>%
  ## BOLD STATES COLUMN
  tab_style(style = list(cell_text(weight = "bold")),
            locations = cells_body(columns = vars(date))) %>% 
  ## STRIPING
  opt_row_striping(row_striping = TRUE) %>%
  tab_source_note(source_note = md("*Created by Andrew F. Griffin, Data from The Covid Tracking Project*"))

TBL_USA_Daily
TBL_USA_Daily %>%
  gtsave("TBL_USA_Daily.html", path = "/Users/andrewgriffin/projects/covid-tracking/figs")
