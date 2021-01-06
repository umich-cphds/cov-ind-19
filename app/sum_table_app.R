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

tp = read_csv(paste0(data_repo, today, "/everything.csv"), col_types = cols())
cfr1 = read_csv(paste0(data_repo, today, "/cfr_t7_avg.csv"), col_types = cols())
r_est = read_csv(paste0(data_repo, today, "/r0_t7_avg.csv"), col_types = cols())

# shortfall -----------
use_abbrevs <- tp %>% pull(abbrev) %>% unique() %>% tolower()

# state data ----------

today = as.Date(today)

sf <- tp %>%
  dplyr::group_by(place) %>%
  dplyr::filter(date == max(as.Date(date))) %>%
  distinct(date, .keep_all = TRUE) %>%
  ungroup() %>%
  dplyr::select(place, total_tests, ppt, shortfall) %>%
  mutate(
    place = case_when(
      place == "India" ~ "National estimate",
      TRUE ~ place
    ),
    shortfall = trimws(format(round(shortfall), big.mark = ",")),
    total_tested = trimws(format(total_tests, big.mark = ",")),
    ppt = round(ppt * 100, digits = 2) 
  ) 

# pull forecast estimates ----------
  # no_int
    for (i in seq_along(use_abbrevs)) {
      eval(parse(text = glue("{use_abbrevs[i]} <- read_tsv('{data_repo}{today}/1wk/{use_abbrevs[i]}_no_int_data.txt', col_types = cols()) %>% filter(date == '{today + 21}') %>% add_column(abbrev = use_abbrevs[i])")))
    }

    no_int_india <- read_tsv(paste0(data_repo, glue("{today}/1wk/india_no_int_data.txt")), col_types = cols()) %>% filter(date == today + 21) %>% add_column(abbrev = "India")
    
    eval(parse(text = glue("no_int_est <- bind_rows({paste0(use_abbrevs, collapse = ', ')}, no_int_india)")))
    no_int_est <- no_int_est %>%
      left_join(
        tp %>%
          dplyr::select(abbrev, place), by = "abbrev") %>%
      distinct() %>%
      mutate(
        name = case_when(
          abbrev == "India" ~ "National estimate",
          abbrev != "India" ~ place)
        ) %>%
      mutate(
        no_int = value
      ) %>%
      dplyr::select(name, no_int)
   
    
    extract_latest <- function(data, group = place, cols = c("total_tests", "tpr", "dbl", "ppt")) {
      out <- data %>%
        group_by({{ group }}) %>%
        filter(date == max(date)) %>%
        distinct(date, .keep_all = TRUE) %>%
        ungroup() %>%
        select({{ group }}, date, all_of(cols))
      if ("India" %in% data[[paste0(substitute(group))]]) {
        out[[paste0(substitute(group))]] <- recode(out[[paste0(substitute(group))]],
                                                   "India" = "National estimate")
      }
      return(out)
    }
    tp %>% extract_latest()

# table ----------
tib <- cfr1 %>%
  distinct(place, .keep_all = TRUE) %>%
  left_join(r_est %>% mutate(place = recode(place, "India" = "National estimate")), by = c("place")) %>%
  left_join(tp %>% extract_latest(cols = c("tpr")), by = c("place")) %>%
  left_join(sf, by = c("place")) %>%
  left_join(no_int_est, by = c("place" = "name")) %>%
  rename(
    Location               = place,
    CFR                    = cfr,
    #`Doubling time (days)` = dbl,
    R                      = r,
    `Test-positive rate`   = tpr,
    `Total tested`         = total_tested,
    `PPT (%)`              = ppt,
    `Testing shortfall`    = shortfall,
    `No intervention`      = no_int
    ) %>%
    arrange(desc(`No intervention`)) %>%
    mutate(
      `Testing shortfall` = trimws(`Testing shortfall`),
      `No intervention`   = trimws(format(`No intervention`, big.mark = ","))
    ) %>%
    dplyr::select(Location, R, CFR, `Test-positive rate`, `Total tested`, `PPT (%)`, `Testing shortfall`, `No intervention`)
    

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
  # fmt_number(
  #   columns  = vars(`Doubling time (days)`),
  #   decimals = 1
  # ) %>%
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
    columns = vars(`No intervention`)
  ) %>%
  tab_spanner(
    label   = "Metrics",
    columns = vars(R, CFR, `Test-positive rate`)
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
  # data_color(
  #   columns = vars(`Doubling time (days)`),
  #   colors = col_bin(c("#d8f5d5", "#FFFFFF", "#fae0de"), domain = NULL, bins = c(0, 21, 28, 1000), pretty = F, reverse = TRUE)
  # ) %>%
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
