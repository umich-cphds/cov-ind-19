# libraries ----------
library(tidyverse)
library(gt)
library(glue)
library(lubridate)
library(janitor)
library(scales)

if (Sys.getenv("production") == "TRUE") {
  data_repo <- "~/cov-ind-19-data/"
  today     <- Sys.getenv("today")
} else {
  data_repo <- "~/cov-ind-19-test/"
  today     <- max(as.Date(grep("[0-9]", list.files(data_repo), value = T)))
}

India_gt_table = function() {

daily = function(x) { c(x[1], diff(x)) }

cfr1  <- data$India$pforest_cfr1$data %>%
  dplyr::select(name, cfr)

dbl   <- data$India$pforest_dbl$data %>%
  dplyr::select(name, dbl)

r_est <- data$India$pforest_r_est$data %>%
  dplyr::select(name, r)

tp    <- data$India$pforest_tp$data %>%
  dplyr::rename(name = state) %>%
  dplyr::select(name, test_pos)

# shortfall -----------
# @MKLEINSA: USE ABBREVS INSTEAD??
use_states <- r_est %>% filter(name != "National estimate") %>% pull(name)

# state data ----------
state_tib <- read_tsv(paste0(data_repo, glue("{today}/covid19india_data.csv")), col_types = cols()) %>%
  clean_names() %>%
  filter(name %in% use_states) %>%
  group_by(name) %>%
  arrange(date) %>%
  mutate(
    day         = seq(n()),
    daily_cases = daily(cases),
    total_cases = max(cases),
    date_max    = max(date)
  ) %>% 
  ungroup()
# @MKLEINSA: PROBABLY REMOVE AFTER INCORPORATING USE_ABBREVS
use_abbrevs <- state_tib %>% pull(state) %>% unique()

state_test <- read_csv(url("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv"), col_types = cols(), guess_max = 1100) %>%
  clean_names() %>%
  mutate(date = as.Date(updated_on, format = '%d/%m/%Y')) %>%
  dplyr::select(date, positive, total_tested, state, population_ncp_2019_projection) %>%
  #drop_na() %>%
  group_by(state) %>%
  mutate(
    population_ncp_2019_projection = median(population_ncp_2019_projection, na.rm = TRUE),
    daily_tests = daily(total_tested),
    prop_pop_test = round((total_tested / population_ncp_2019_projection) * 100, 2)
  ) %>%
  ungroup() %>%
  dplyr::select(-population_ncp_2019_projection) %>%
  filter(date <= (as.Date(today) - 1))

test <- read_csv(paste0(data_repo, glue("{today}/everything.csv")))

# test <- state_tib %>% 
#   left_join(state_test, by = c("date", "name" = "state")) %>%
#   drop_na() %>%
#   group_by(name) %>%
#   mutate(test_pos = positive / total_tested) %>%
#   slice(tail(row_number(), 6)) %>%
#   summarize(
#     tpr_obs = mean(test_pos)
#   ) %>%
#   mutate(
#     tpr_ratio = tpr_obs / 0.02,
#     sf_factor = tpr_ratio - 1
#   ) %>%
#   ungroup() %>%
#   left_join(state_test %>% 
#               group_by(state) %>% 
#               filter(date == max(date) - 1) %>%
#               ungroup(), by = c("name" = "state")) %>%
#   mutate(
#     sf = case_when(
#       sf_factor * total_tested < 0 ~ 0,
#       sf_factor * total_tested >= 0 ~ sf_factor * total_tested
#     )
#   )

# nat_testing <- read_csv(url("https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv"), col_types = cols(), guess_max = 1200) %>%
#   clean_names() %>%
#   mutate(date = as.Date(updated_on, format = '%d/%m/%Y')) %>%
#   dplyr::select(date, positive, total_tested, state, population_ncp_2019_projection) %>%
#   group_by(state) %>%
#   mutate(population_ncp_2019_projection = median(population_ncp_2019_projection, na.rm = TRUE)) %>%
#  # drop_na(c(positive, total_tested, population_ncp_2019_projection)) %>%
#   filter(date == max(date) - 1) %>%
#   ungroup() %>%
#   summarise(
#     positive     = sum(positive, na.rm = TRUE),
#     total_tested = sum(total_tested, na.rm = TRUE),
#     population   = sum(population_ncp_2019_projection, na.rm = TRUE)
#   ) %>%
#   mutate(
#     test_pos       = positive / total_tested,
#     prop_pop_test  = round((total_tested / population) * 100, 2)
#   ) %>%
#   mutate(
#     tpr_ratio = test_pos / 0.02,
#     sf_factor = tpr_ratio - 1
#   ) %>%
#   mutate(
#     sf = case_when(
#       sf_factor * total_tested < 0 ~ 0,
#       sf_factor * total_tested >= 0 ~ sf_factor * total_tested
#     )
#   )

# sf <- test %>%
#   dplyr::select(place, total_tests, ppt, shortfall) %>%
#   mutate(
#     place == "India" ~ "National estimate",
#     TRUE ~ place
#     mutate(
#       shortfall = trimws(format(round(shortfall), big.mark = ",")),
#       total_tested = trimws(format(total_tests, big.mark = ","))
#     )

today = as.Date(today)

sf <- test %>%
  dplyr::group_by(place) %>%
  dplyr::filter(date == max(date)) %>%
  ungroup() %>%
  dplyr::select(place, total_tests, ppt, shortfall) %>%
  mutate(
    place = case_when(
      place == "India" ~ "National estimate",
      TRUE ~ place
    ),
    shortfall = trimws(format(round(shortfall), big.mark = ",")),
    total_tested = trimws(format(total_tests, big.mark = ","))
  ) 
    

# pull forecast estimates ----------
  # cautious 
    for (i in seq_along(use_abbrevs)) {
      eval(parse(text = glue("{use_abbrevs[i]} <- read_tsv('{data_repo}{today}/1wk/{use_abbrevs[i]}_cautious_data.txt', col_types = cols()) %>% filter(date == '{today + 21}') %>% add_column(abbrev = use_abbrevs[i])")))
    }

    cautious_india <- read_tsv(paste0(data_repo, glue("{today}/1wk/india_cautious_data.txt")), col_types = cols()) %>% filter(date == today + 21) %>% add_column(abbrev = "India")
    
    eval(parse(text = glue("cautious_est <- bind_rows({paste0(use_abbrevs, collapse = ', ')}, cautious_india)")))
    cautious_est <- cautious_est %>%
      left_join(
        state_tib %>%
          dplyr::select(abbrev = state, name), by = "abbrev") %>%
      distinct() %>%
      mutate(
        name = case_when(
          abbrev == "India" ~ "National estimate",
          abbrev != "India" ~ name)
        ) %>%
      mutate(
        cautious = value
      ) %>%
      dplyr::select(name, cautious)
    
  # moderate
    for (i in seq_along(use_abbrevs)) {
      eval(parse(text = glue("{use_abbrevs[i]} <- read_tsv('{data_repo}/{today}/1wk/{use_abbrevs[i]}_moderate_data.txt', col_types = cols()) %>% filter(date == '{today + 21}') %>% add_column(abbrev = use_abbrevs[i])")))
    }
    
    moderate_india <- read_tsv(paste0(data_repo, glue("{today}/1wk/india_moderate_data.txt")), col_types = cols()) %>% filter(date == today + 21) %>% add_column(abbrev = "India")
    
    eval(parse(text = glue("moderate_est <- bind_rows({paste0(use_abbrevs, collapse = ', ')}, moderate_india)")))
    moderate_est <- moderate_est %>%
      left_join(
        state_tib %>%
          dplyr::select(abbrev = state, name), by = "abbrev") %>%
      distinct() %>%
      mutate(
        name = case_when(
          abbrev == "India" ~ "National estimate",
          abbrev != "India" ~ name)
      ) %>%
      mutate(
        moderate = value
      ) %>%
      dplyr::select(name, moderate)

# table ----------
tib <- cfr1 %>%
  left_join(dbl, by = "name") %>%
  left_join(r_est, by = "name") %>%
  left_join(tp, by = "name") %>%
  left_join(sf, by = c("name" = "place")) %>%
  left_join(cautious_est, by = "name") %>%
  left_join(moderate_est, by = "name") %>%
  rename(
    Location               = name,
    CFR                    = cfr,
    `Doubling time (days)` = dbl,
    R                      = r,
    `Test-positive rate`   = test_pos,
    `Total tested`         = total_tested,
    `PPT (%)`              = ppt,
    `Testing shortfall`    = shortfall,
    `Cautious return`      = cautious,
    `Moderate return`      = moderate
    ) %>%
    arrange(desc(`Cautious return`)) %>%
    mutate(
      `Testing shortfall` = trimws(`Testing shortfall`),
      `Cautious return`   = trimws(format(`Cautious return`, big.mark = ",")),
      `Moderate return`   = trimws(format(`Moderate return`, big.mark = ","))
    ) %>%
    dplyr::select(Location, R, `Doubling time (days)`, CFR, `Test-positive rate`, `Total tested`, `PPT (%)`, `Testing shortfall`, `Cautious return`, `Moderate return`)
    

tabl <- tib %>%
  gt() %>%
  # format table body text
  tab_style(
    style     = cell_text(size = px(14), font = "helvetica"),
    locations = cells_body()
  ) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(vars(Location))
  ) %>%
  # format column names
  tab_style(
    style = cell_text(
      size      = px(12),
      color     = "#999",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_labels(everything())
  ) %>%
  # format numbers
  fmt_number(
    columns  = vars(CFR, `Test-positive rate`),
    decimals = 3
  ) %>%
  fmt_number(
    columns  = vars(R),
    decimals = 2
  ) %>%
  fmt_number(
    columns  = vars(`Doubling time (days)`),
    decimals = 1
  ) %>%
  # random formatting
  tab_options(
    column_labels.border.top.style    = "none",
    column_labels.border.bottom.width = 1,
    column_labels.border.bottom.color = "#334422",
    table_body.border.bottom.color    = "#0000001A",
    data_row.padding                  = px(4)
  ) %>%
  # column widths
  cols_width(
    vars(Location) ~ px(150),
    vars(R, CFR, `PPT (%)`) ~ px(75),
    everything() ~ px(100)
  ) %>%
  cols_align(
    align   = "center",
    columns = everything()
  ) %>%
  # title
  tab_header(
    title    = md("**Assessing COVID-19 in India**"),
    subtitle = glue("as of {format(today, '%B %e')}")
  ) %>%
  # caption
  tab_source_note(
    source_note = md(glue(
      "**\uA9 COV-IND-19 Study Group**<br>**Source data:** covid19india.org<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Predicted cases are for {format(today + 21, '%B %d')} based on data through {format(today, '%B %e')}. 
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown. 
      National Commission on Population 2019 projections used to calculate PPT.<br>
      **Abbrev:** CFR, Case-fatality rate; PPT, Proportion of population tested"
      ))
  ) %>%
  # add and format column spanners
  tab_spanner(
    label   = glue("Predicted cases ({format(today + 21, '%m/%d')})"),
    columns = vars(`Cautious return`, `Moderate return`)
  ) %>%
  tab_spanner(
    label   = "Metrics",
    columns = vars(R, `Doubling time (days)`, CFR, `Test-positive rate`)
  ) %>%
  tab_style(
    style = cell_text(
      size      = px(14),
      color     = "#999",
      font      = "helvetica",
      transform = "uppercase"
    ),
    locations = cells_column_spanners(spanners = c("Metrics", glue("Predicted cases ({format(today + 21, '%m/%d')})")))
  ) %>%
  # adjust title font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(24))),
    locations = list(cells_title(groups = "title"))
  ) %>%
  # adjust subtitle font
  tab_style(
    style     = list(cell_text(font = "helvetica", size = px(18))),
    locations = list(cells_title(groups = "subtitle"))
  ) %>%
  # color cells based on values
  data_color(
    columns = vars(R),
    colors = col_bin(c("#d8f5d5", "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1.5,100), pretty = F)
  ) %>%
  data_color(
    columns = vars(`Doubling time (days)`),
    colors = col_bin(c("#d8f5d5", "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 21, 28, 100), pretty = F, reverse = TRUE)
  ) %>%
  data_color(
    columns = vars(CFR),
    colors = col_bin(c("#d8f5d5", "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.03, 0.06, 1), pretty = F)
  ) %>%
  data_color(
    columns = vars(`Test-positive rate`),
    colors = col_bin(c("#d8f5d5", "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 0.02, 0.06, 1), pretty = F, na.color = "#e8e8e8")
  ) %>%
  # highlight national estimate
  tab_style(
    style = cell_fill(color = "#fcf8d4"),
    locations = cells_body(
      rows = Location == "National estimate")
  )
tabl
}

