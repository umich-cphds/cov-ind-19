# packages -----
suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(data.table)
  library(gt)
  library(data.table)
  library(tidyfast)
  library(glue)
  library(janitor)
  library(lubridate)
  library(EpiEstim)
  library(rrapply)
  library(rjson)
  library(EpiEstim)
  library(scales)
})

# path -----------
data_repo = Sys.getenv("data_repo") #data_repo = "../../cov-ind-19-data/"
data_repo = paste0(data_repo, "/source_data/")

subfolder_package = "/package-data/raw/"
subfolder_app = "/package-data/processed/"

# load data -----------

# new data format
#source("https://gitlab.com/-/snippets/2391974")
# copied source directly and edited the text columns

## helper function
get_r_est = function(x) {
  
  x[order(date), .SD[ifelse(.N-6 < 1, 1, .N-6):.N], by = "place"][, .(
    r     = mean(r_est, na.rm = TRUE),
    lower = mean(r_lower, na.rm = TRUE),
    upper = mean(r_upper, na.rm = TRUE)),
    .(place)]
  
}

## helper function for calculating case-fatality rate
CFR = function(C,D) {
  cfr       = D / C
  cfr_logit = log(cfr) - log(1 - cfr)
  sd_logit  = sqrt(C / (D * (C - D)))
  
  lower_logit = cfr_logit - stats::qnorm(0.975) * sd_logit
  upper_logit = cfr_logit + stats::qnorm(0.975) * sd_logit
  
  upper = exp(upper_logit) / (1 + exp(upper_logit))
  lower = exp(lower_logit) / (1 + exp(lower_logit))
  
  return(c(cfr, upper, lower))
}

## calculate case_fataility rate
get_cfr = function(x) {
  
  tmp = x[x[, .I[date == max(date)], by = "place"]$V1][
    , .(place, C = total_cases, D = total_deaths)]
  
  tmp_out = data.table::data.table(
    place = tmp[, place],
    cfr   = rep(0, nrow(tmp)),
    upper = rep(0, nrow(tmp)),
    lower = rep(0, nrow(tmp))
  )
  
  for (i in 1:nrow(tmp_out)) {
    C = tmp$C[i]
    D = tmp$D[i]
    
    result = suppressWarnings(CFR(C,D))
    
    tmp_out$cfr[i]   = result[1]
    tmp_out$upper[i] = result[2]
    tmp_out$lower[i] = result[3]
    
  }
  
  unique(tmp_out[, place := data.table::fcase(place == "India", "National estimate", place != "India", place)])
  
}

## helper function for calculating R
estR0_out = function(x, incubation_days = 3) {
  
  t_start   = seq(2, nrow(x) - 4)
  t_end     = t_start + 4
  
  res = EpiEstim::estimate_R(
    incid = x$daily_cases,
    method = "parametric_si",
    config = EpiEstim::make_config(list(
      mean_si             = incubation_days,
      std_si              = 4.5,
      si_parametric_distr = "G",
      t_start             = t_start,
      t_end               = t_end,
      seed                = 46342))
  )
  
  tmp_out = data.table::merge.data.table(
    data.table::data.table(date_num = res$dates), res$R, by.x = "date_num", by.y = "t_end", all.x = TRUE)[
      , .(r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`)][
        , `:=` (date = x$date, place = x$place)]
  data.table::setcolorder(tmp_out, "date")
  
  return(tmp_out[])
  
}


## calculate r0
get_r0 = function(
    dat,
    daily_filter = 0,
    total_filter = 50,
    min_date     = "2020-03-23",
    inc_days     = c(7, 5, 3)
) {
  
  tmp_dat = dat[daily_cases > daily_filter & total_cases >= total_filter][, ns := .N, by = "place"][ns >= 7]
  
  a = c(as.Date("2020-12-31"), as.Date("2021-12-31"), Sys.Date())
  b = as.Date(c(min_date, "2021-01-01", "2022-01-01"))
  c = c(1:3)
  suppressWarnings({
    out = purrr::map_dfr(c,
                          ~{
                            stats::na.omit(
                              data.table::rbindlist(
                                lapply(tmp_dat[date <= a[.x]][, unique(place)],
                                       \(x) estR0_out(tmp_dat[date <= a[.x]][place == x], incubation_days = inc_days[c[.x]]))
                              )[date >= b[.x]]
                            )
                          }
    )
  })
  
  return(out)
  
}


covid19bharat_ts = function(
    path_to_json = "https://data.covid19bharat.org/v4/min/timeseries.min.json",
    path_to_pop = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/model/populations.csv",
    inc_days = c(7, 5, 3)
) {
  
  # read json data
  raw_json = rjson::fromJSON(file = path_to_json)
  
  # process raw json data
  processed_json = data.table::as.data.table(rrapply::rrapply(raw_json, how = "melt"))[]
  
  # do some clean up
  data.table::setnames(processed_json, old = c("L1", "L3", "L4", "L5"), new = c("abbrev", "date", "period", "stat"))
  processed_json = processed_json[, `:=` (
    L2 = NULL,
    date = as.Date(date)
  )][!(stat %in% c("other"))]
  
  # pull daily and total data
  dats = purrr::map2(c("daily", "total"), c("delta", "total"),
                      ~{
                        
                        tmp = data.table::dcast(processed_json[period == .y],
                                                 abbrev + date ~ stat, value.var = "value")
                        data.table::setnames( tmp, c("confirmed", "deceased", "recovered", "tested", "vaccinated1", "vaccinated2", "precautionary"),paste0(.x, c("_cases", "_deaths", "_recovered", "_tests", "_first_dose", "_second_dose", "_precaution_dose")))
                        
                        data.table::setkeyv(tmp, c("abbrev", "date"))
                        
                        return(tmp)
                        
                      })
  
  pop = data.table::fread(path_to_pop, showProgress = FALSE)[, .(place = full, abbrev, population)][]
  
  out = purrr::reduce(dats, data.table::merge.data.table)[, abbrev := tolower(abbrev)] |>
    data.table::merge.data.table(pop, by = "abbrev")
  
  out = out[, `:=` (
    pct_one_dose = round(total_first_dose * 100 / population, 2),
    pct_two_doses = round(total_second_dose * 100 / population, 2),
    tpr = round(daily_cases / daily_tests, 4)
  )][, !c("population")]
  
  out[order(date), daily_doses := daily_first_dose + daily_second_dose + (total_precaution_dose - data.table::shift(total_precaution_dose)), by = place]
  
  data.table::setcolorder(out, "place")
  
  out = data.table::merge.data.table(
    out,
    get_r0(dat = out, inc_days = inc_days)[, .(place, date, r_est = r, r = r, r_lower = lower, r_upper = upper)],
    by = c("place", "date"),
    all.x = TRUE
  )
  
  # text columns
  out = out[, `:=` (
    daily_cases_text = paste0(as.factor(format(date, format = "%b %e")), ": ",
                              format(daily_cases, big.mark = ",", scientific = F, trim = T), " New Cases"),
    daily_deaths_text = paste0(as.factor(format(date, format = "%b %e")), ": ",
                               format(daily_deaths, big.mark = ",", scientific = F, trim = T), " Fatalities"),
    daily_recovered_text = paste0(as.factor(format(date, format = "%b %e")), ": ",
                                  format(daily_recovered, big.mark = ",", scientific = F, trim = T), " Recoveries"),
    r_text = paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                    format(round(r_est, 2), nsmall = 2), "<br>CI: ",
                    paste0("[", format(round(r_lower, 2), nsmall = 2), ", ",
                           format(round(r_upper, 2), nsmall = 2), "]")),
    tpr_text = paste0("Date: ", format(date, format = '%b %d'), "<br>TPR: ",
                      format(round(tpr*100, 2), nsmall = 2), "%"),
    total_doses = total_first_dose + total_second_dose + total_precaution_dose,
    pct_one_dose_text = paste0(place, "<br>", date, ": ", pct_one_dose, " % one dose<br>"),
    pct_two_doses_text = paste0(place, "<br>", date, ": ", pct_two_doses, " % two doses<br>")
  )
  ][]
  
  return(out[date < Sys.Date()])
  
}

d = covid19bharat_ts()

## make metrics tables
get_metrics_tables = function(seed = 46342, inc_days = c(7, 5, 3)) {
  
  cli::cli_alert_info("getting data...")
  
  set.seed(set_seed <- seed)
  
  all_data = covid19bharat_ts()
  today    = max(all_data[, date], na.rm = TRUE)
  cfr1     = unique(get_cfr(all_data))[place == "National estimate", place := "India"][]
  r_est    = get_r_est(all_data[!is.na(r_est)])
  
  cli::cli_alert_success("data load success!!")
  
  # pull abbrevs -----------
  use_abbrevs = tolower(unique(all_data[abbrev != "la", abbrev]))
  
  # vax data ----------
  data.table::setnames(all_data, c("total_doses", "pct_one_dose", "pct_two_doses", "daily_doses"), c("total_vacc", "pct_at_least_one", "pct_second", "daily_vax_dose"))
  
  test_data = all_data[order(date), .SD[date >= max(date) - 8 & date < max(date)], by = "place"][
    , .SD, .SDcols = c("place", "date", "daily_cases", "daily_tests", "total_tests")
  ][
    , daily_tests := data.table::fifelse(daily_tests == 0, NA_real_, daily_tests), by = "place"
  ][, tpr7d := mean(daily_cases / daily_tests, na.rm = TRUE), by = "place"][
    , .SD, .SDcols = c("place", "tpr7d", "total_tests", "daily_tests")
  ][
    , .SD[nrow(.SD)], by = "place"
  ][
    , tpr7d := ifelse(is.nan(tpr7d), NA, tpr7d)
  ]
  
  vax_dat = all_data[!is.na(total_vacc)][, .SD[date == max(date)], by = "place"][, .(place, total_vacc, pct_at_least_one, pct_second, daily_vax_dose, pct_one_dose_text, pct_two_doses_text)]
  
  all_data = unique(all_data[, .SD[date > max(as.Date(date) - 7)], by = "place"][, dailyCFR7 := daily_deaths / daily_cases][, dailyCFR7d := mean(dailyCFR7, na.rm = T), by = "place"][, .SD[date == max(date)], by = "place"][])[, .(place, dailyCFR7d, daily_cases, daily_deaths, total_cases, total_deaths)]
  
  # all_data = data.table::merge.data.table(all_data, covid19india::pop[, .SD[1], by = "place"], by = "place", all.x = TRUE)
  
  all_data = data.table::merge.data.table(all_data, vax_dat, by = "place", all.x = TRUE)
  
  all_data = data.table::merge.data.table(all_data, test_data, by = "place")
  
  # table ----------
  tib = cfr1[, .(place, cfr)]
  
  tib = data.table::merge.data.table(tib, r_est[, .(place, r)], by = "place", all.x = TRUE)
  
  # tib = data.table::merge.data.table(tib, extract_latest(tp, clmns = c("tpr")), by = "place", all.x = TRUE)
  tib = data.table::merge.data.table(tib, all_data[, .(place, tpr7d, daily_tests)], by = "place", all.x = TRUE)
  
  tib = data.table::merge.data.table(tib, all_data[, tpr7d := NULL][, daily_tests := NULL], by = "place", all.x = TRUE)[
    , `:=` (
      perc_vaccine   = pct_at_least_one,
      total_vacc     = format(total_vacc, big.mark = ","),
      daily_cases    = format(daily_cases, big.mark = ","),
      daily_deaths   = format(daily_deaths, big.mark = ","),
      daily_vax_dose = format(daily_vax_dose, big.mark = ","),
      daily_tests    = format(daily_tests, big.mark = ","),
      cases          = format(total_cases, big.mark = ","),
      deaths         = format(total_deaths, big.mark = ","),
      tested         = format(total_tests, big.mark = ",")
    )
  ][]
  
  data.table::setnames(tib,
                       old = c( "daily_cases", "daily_deaths", "dailyCFR7d", "r", "tpr7d", "daily_vax_dose", "place", "cfr", "cases", "deaths", "perc_vaccine", "total_vacc", "pct_second", "pct_at_least_one", "tested", "daily_tests"),
                       new = c("# daily new cases", "# daily new deaths",  "7-day average daily CFR", "R", "7-day average daily TPR", "daily vaccine doses", "Location", "CFR", "total cases","total deaths",  "Percent with at least one dose", "Total doses", "% pop. with two shots", "% pop. with at least one shot", "total tests", "# daily new tests"))
  
  tib = tib[order(-`total_cases`)][
    , `:=` (
      `7-day average daily CFR`       = round(`7-day average daily CFR`, digits = 3),
      `7-day average daily TPR (%)`   = round(`7-day average daily TPR`, digits = 6)*100,
      `% pop. with two shots`         = round(`% pop. with two shots`, digits = 2),
      `% pop. with at least one shot` = round(`% pop. with at least one shot`, digits = 2)
    )
  ][
    , .(`# daily new cases`, `# daily new deaths`, `7-day average daily CFR`,
        `7-day average daily TPR (%)`, `# daily new tests`,
        Location, R, `daily vaccine doses`, CFR, `total tests`, `total cases`,
        `total deaths`, `Total doses`, `% pop. with two shots`,
        `% pop. with at least one shot`)
  ]
  
  tib = unique(tib)[!grepl("\\*\\*", tib$Location),]
  
  source_note_text = glue::glue(
    "**\uA9 COV-IND-19 Study Group**<br>**Source data ** COVID-19-Bharat: https://data.covid19bharat.org/<br>
      **Notes:** Cells highlighted in green indicates good performance for given metric while red indicates need for improvement.
      Only states/union territories with the highest cumulative case counts as of {format(today, '%B %e')} are shown.
      States are omitted if they have missing case count data.
      <br>
      R values are not reliable when case counts are below 100.
      <br>
      **Abbrev:** CFR, Case-fatality rate."
  )
  
  tabl = tib |>
    gt::gt() |>
    # format table body text
    gt::tab_style(
      style     = gt::cell_text(size = gt::px(14), font = "helvetica"),
      locations = gt::cells_body()
    ) |>
    gt::tab_style(
      style     = gt::cell_text(weight = "bold"),
      locations = gt::cells_body((Location))
    ) |>
    # format column names
    gt::tab_style(
      style = gt::cell_text(
        size      = gt::px(12),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = gt::cells_column_labels(everything())
    ) |>
    # format numbers
    gt::fmt_number(
      columns  = c(CFR),
      decimals = 3
    ) |>
    gt::fmt_number(
      columns  = c(R),
      decimals = 2
    ) |>
    gt::fmt_number(
      columns  = c(`7-day average daily TPR (%)`),
      decimals = 3
    ) |>
    # random formatting
    gt::tab_options(
      column_labels.border.top.style    = "none",
      column_labels.border.bottom.width = 1,
      column_labels.border.bottom.color = "#334422",
      table_body.border.bottom.color    = "#0000001A",
      data_row.padding                  = gt::px(4)
    ) |>
    # column widths
    gt::cols_width(
      Location ~ gt::px(150),
      c(R, CFR) ~ gt::px(75),
      everything() ~ gt::px(100)
    ) |>
    gt::cols_align(
      align   = "center",
      columns = everything()
    ) |>
    # title
    gt::tab_header(
      title    = gt::md("**Assessing COVID-19 in India**"),
      subtitle = glue::glue("data through {format(today, '%B %e')}")
    ) |>
    # caption
    gt::tab_source_note(
      source_note = gt::md(source_note_text)
    ) |>
    # add and format column spanners
    gt::tab_spanner(
      label   = "Point in time metrics",
      columns = c(`# daily new cases`, `# daily new deaths`,
                  `7-day average daily CFR`, R, `7-day average daily TPR (%)`, `# daily new tests`,
                  `daily vaccine doses`)
    ) |>
    gt::tab_spanner(
      label   = "Cumulative metrics",
      columns = c(`total cases`, `total deaths`, CFR, `total tests`,
                  `Total doses`, `% pop. with two shots`, `% pop. with at least one shot`)
    ) |>
    gt::cols_move_to_start((Location)) |>
    gt::tab_style(
      style = gt::cell_text(
        size      = gt::px(14),
        color     = "#999999",
        font      = "helvetica",
        transform = "uppercase"
      ),
      locations = gt::cells_column_spanners(spanners = c("Point in time metrics", "Cumulative metrics"))
    ) |>
    # adjust title font
    gt::tab_style(
      style     = list(gt::cell_text(font = "helvetica", size = gt::px(24))),
      locations = list(gt::cells_title(groups = "title"))
    ) |>
    # adjust subtitle font
    gt::tab_style(
      style     = list(gt::cell_text(font = "helvetica", size = gt::px(18))),
      locations = list(gt::cells_title(groups = "subtitle"))
    ) |>
    # color cells based on values
    gt::data_color(
      columns = c(R),
      colors = scales::col_bin(c("#FFFFFF", "#fae0de"), domain = NULL, bins = c(0,1,1000), pretty = F)
    ) |>
    # highlight national estimate
    gt::tab_style(
      style = gt::cell_fill(color = "#fcf8d4"),
      locations = gt::cells_body(
        rows = Location == "India")
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "left"),
      locations = gt::cells_body(columns = (`total cases`))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "left"),
      locations = gt::cells_column_labels(columns = (`total cases`))
    ) |>
    gt::tab_style(
      style = gt::cell_borders(sides = "left"),
      locations = gt::cells_column_spanners(("Cumulative metrics"))
    )
  
  return(list(full = tabl))
  
}

metrics_tables = get_metrics_tables()
gtsave(metrics_tables$full, filename = paste0(paste0(data_repo, subfolder_package), "metrics_table_full.html"))


# old version of the other app stuff
readr::write_csv(d, file = paste0(paste0(data_repo, subfolder_app), "new_everything.csv"))

write_snapshot = function() {
  
  snapshot = function() {
    # functions -----------
    get_snap = function(t = Sys.Date() - 1) {
      
      nat = d[place == "India", ]
      
      vax_dat = d[date <= nat[, max(date)]][place == "India", .(date = date, daily_doses)][, lag := daily_doses][]
      
      test_data = d %>%
        filter(place == "India") %>%
        mutate(confirmed = daily_cases,
               tested    = daily_tests) %>%
        group_by(date) %>%
        mutate(ntpr = confirmed / tested) %>%
        filter(tested > 100) %>%
        as.data.table()
      
      if (!is.null(t)) {
        today = as.Date(t)
      } else {
        today     = min(max(nat$date, na.rm = TRUE),
                         max(vax_dat$date, na.rm = TRUE))
      }
      
      yesterday = today - 1
      week_ago  = today - 7
      month_ago = today - 30
      
      get_stats = function(d) {
        
        tmp_nat = nat[date == d]
        tmp_deaths = tmp_nat[, daily_deaths]
        tmp_cases  = tmp_nat[, daily_cases]
        tmp_vax = vax_dat[date == d, lag]
        tmp_tpr = test_data[date == d, ntpr]
        tmp_test = test_data[date == d, daily_tests]
        
        data.table(
          Day        = fifelse(d == today, "Today",
                               fifelse(d == yesterday, "Yesterday",
                                       fifelse(d == week_ago, "One week ago",
                                               fifelse(d == month_ago, "One month ago", "")))),
          Date       = format(d, "%m/%d"),
          Deaths     = format(tmp_deaths, big.mark = ","),
          Cases      = format(tmp_cases, big.mark = ","),
          Tests      = format(tmp_test, big.mark = ","),
          TPR        = sprintf("%1.2f%%", tmp_tpr*100),
          Vaccines   = format(tmp_vax, big.mark = ",")
        )
        
      }
      
      today_stats     = get_stats(today)
      yesterday_stats = get_stats(yesterday)
      week_ago_stats  = get_stats(week_ago)
      month_ago_stats = get_stats(month_ago)
      
      rbindlist(list(
        today_stats,
        yesterday_stats,
        week_ago_stats,
        month_ago_stats
      ))
      
    }
    
    make_pretty = function(x) {
      
      source_note_text = glue::glue(
        "**\uA9 COV-IND-19 Study Group**<br>**Source data:** COVID-19-Bharat: https://data.covid19bharat.org/<br>"
      )
      
      x %>%
        gt() %>%
        # bold column headers
        tab_style(
          style = cell_text(
            font = "arial",
            weight = "bold"
          ),
          locations = cells_column_labels(everything())
        ) %>%
        # center column headers
        tab_style(
          style = cell_text(
            align = "center"
          ),
          locations = cells_column_labels(c(Date, Deaths, Cases, TPR, Vaccines))
        ) %>%
        # format columns
        tab_style(
          style = list(
            cell_text(
              font = "arial",
              align = "center"
            )
          ),
          locations = list(
            cells_body(columns = c(Date, Deaths, Cases, TPR, Vaccines))
          )
        ) %>%  # caption
        tab_source_note(
          source_note = md(source_note_text)
        )
    }
    
    # run ----------
    snap = get_snap()
    
    # today = as.Date(snap[Day == "Today", Date], "%m/%d")
    
    return(make_pretty(snap))
  }
  
  gtsave(snapshot(), paste0(data_repo, subfolder_app, "snapshot.html"))
  
}
write_snapshot()


# insert html for new site into snapshot.html
to_insert = "<script>
    function sendMessage() { window.parent.postMessage(document.getElementById('fzrccngwny').offsetHeight, '*'); }
    window.addEventListener('message', sendMessage)
    window.addEventListener('load', sendMessage)
</script>"

snap_lines = readLines(paste0(data_repo, subfolder_app, "snapshot.html"))
new_snap_lines = vector(mode = "character", length = length(snap_lines) + 1)

inside_header = FALSE
below_style = FALSE
line_count = 1
for(i in 1:length(snap_lines)) {
  #print(paste("iteration ", i))
  if(snap_lines[i] == "<head>") {
    inside_header = TRUE
  }
  if(i > 1 && snap_lines[i - 1] == "</style>") {
    below_style = TRUE
  }
  if(inside_header == TRUE & below_style == TRUE) {
    new_snap_lines[i] = to_insert
    #print(paste("INSERTED ", i))
    inside_header = FALSE
    below_style = FALSE
  } else {
    new_snap_lines[i] = snap_lines[line_count]
    line_count = line_count + 1
  }
}
new_snap_lines[length(new_snap_lines)] = snap_lines[length(snap_lines)]

writeLines(new_snap_lines, con = paste0(paste0(data_repo, subfolder_app), "snapshot.html"))


write_case_death_country_comp = function() {
  
  d = d[place == "India", ]
  
  Day_max          = 100
  cases_threshold  = 100
  deaths_threshold = 3
  
  fmt  = function(x) format(x, big.mark = ",", scientific = F, trim = T)
  
  # jhu data ----------
  not_these = c("Province/State", "Lat", "Long")
  countries = c("France", "Germany", "India", "Iran", "Italy", "Russia", "Brazil",
                 "Pakistan", "Bangladesh", "Korea, South", "US", "China", "Canada",
                 "Belgium", "Turkey", "Netherlands", "Switzerland", "United Kingdom"
  )
  
  read_n_clean = function(x, val_name) {
    tmp = fread(x, showProgress = FALSE)[, !..not_these]
    setnames(tmp, old = "Country/Region", new = "Country")
    tmp = tmp[Country %in% countries][Country == "Korea, South", Country := "South Korea"][
      , lapply(.SD, sum, na.rm=TRUE), by = Country
    ]
    data.table::melt(tmp, id.vars = "Country", variable.name = "Date", value.name = val_name)[, Date := as.Date(Date, format = "%m/%d/%y")]
  }
  
  case_data = read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                            val_name = "Cases")
  death_data = read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                             val_name = "Deaths")
  recovered_data = read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", val_name = "Recovered")
  
  combined = merge.data.table(x = case_data, death_data, by = c("Country", "Date"))
  combined = merge.data.table(x = combined, recovered_data, by = c("Country", "Date"))
  
  combined = rbindlist(list(
    combined[Country != "India"],
    d[
      , .(Country = place, Date = date, Cases = total_cases,
          Deaths = total_deaths, Recovered = total_recovered)
    ][Date < min(combined[Country != "India", max(Date)], d[, max(date)])][]
  ))
  
  max_date = combined[, max(Date)]
  min_date = unique(combined[, .SD[Cases >= cases_threshold], by = "Country"][
    , min(Date)])
  
  
  # cases data -----------
  cases_data = combined[Cases >= cases_threshold][order(Date)][
    , Day := 1:.N, by = "Country"
  ][Day > 30][
    order(Date),
    Incident_Cases := Cases - data.table::shift(Cases),
    by = "Country"][
      , Cases_fmt := fmt(Incident_Cases)
    ][
      , text := paste0(Country, "<br>", Date, ": ", Cases_fmt,
                       " incident cases<br>")
    ][,
      loess_cases := c(0, predict(loess(formula = Incident_Cases ~ Day, span = 0.025))), by = "Country"][
        Country != "India",
        .(Country, Date, Incident_Cases, Cases_fmt, text, loess_cases, Day)][]
  
  
  # deaths data ----------
  deaths_data = combined[Deaths >= deaths_threshold][order(Date)][
    , Day := 1:.N, by = "Country"
  ][Day > 30][
    order(Date),
    Incident_Deaths := Deaths - data.table::shift(Deaths),
    by = "Country"][
      , Deaths_fmt := fmt(Incident_Deaths)
    ][
      , text := paste0(Country, "<br>", Date, ": ", Deaths_fmt,
                       " incident deaths<br>")
    ][,
      loess_deaths := c(0, predict(loess(formula = Incident_Deaths ~ Day, span = 0.025))), by = "Country"][
        Country != "India",
        .(Country, Date, Incident_Deaths, Deaths_fmt, text, loess_deaths, Day)][]
  
  # india data -----------
  india_data = d
  
  india_cases = india_data[total_cases >= cases_threshold][
    order(date), Day := 1:.N][Day > 30][]
  setnames(india_cases,
           old = c("place", "date", "daily_cases"),
           new = c("Country", "Date", "Incident_Cases"))
  india_cases = india_cases[
    , Incident_Cases := ifelse(is.na(Incident_Cases), 0, Incident_Cases)
  ][
    , Cases_fmt := fmt(Incident_Cases)
  ][
    , text := paste0(Country, "<br>", Date, ": ", Cases_fmt,
                     " incident cases<br>")
  ][
    , loess_cases := c(predict(loess(formula = Incident_Cases ~ Day, span = 0.025)))
  ][, .(Country, Date, Incident_Cases, Cases_fmt, text, loess_cases, Day)]
  
  india_deaths = india_data[total_deaths >= deaths_threshold][
    order(date), Day := 1:.N][Day > 30][]
  setnames(india_deaths,
           old = c("place", "date", "daily_deaths"),
           new = c("Country", "Date", "Incident_Deaths"))
  india_deaths = india_deaths[
    , Incident_Deaths := ifelse(is.na(Incident_Deaths), 0, Incident_Deaths)
  ][
    , Deaths_fmt := fmt(Incident_Deaths)
  ][
    , text := paste0(Country, "<br>", Date, ": ", Deaths_fmt,
                     " incident deaths<br>")
  ][
    , loess_deaths := c(predict(loess(formula = Incident_Deaths ~ Day, span = 0.025)))
  ][, .(Country, Date, Incident_Deaths, Deaths_fmt, text, loess_deaths, Day)]
  
  # combine ----------
  cases_data  = rbindlist(list(cases_data, india_cases), fill = TRUE)
  deaths_data = rbindlist(list(deaths_data, india_deaths), fill = TRUE)
  
  fwrite(cases_data, file = paste0(data_repo, subfolder_app, "case_death_country_comp_cases.csv"))
  fwrite(deaths_data, file = paste0(data_repo, subfolder_app, "case_death_country_comp_deaths.csv"))
  
  cases_data = 
    cases_data %>% 
    rename(Cases_text = text,
           Cases_day = Day)
  
  deaths_data = 
    deaths_data %>% 
    rename(Deaths_text = text,
           Deaths_day = Day)
  
  all_data = 
    inner_join(cases_data, deaths_data, by = c("Country", "Date"))
  
  fwrite(all_data, file = paste0(data_repo, subfolder_app, "case_death_country_comp_cases_and_deaths.csv"))
  
}

write_case_death_country_comp()


write_r_forest = function() {
  
  r_forest_plot = function(dat) {
    
    today = max(dat[, date])
    
    # ggplot theme ------------
    covind19_base = theme_minimal() +
      theme(
        plot.title         = ggtext::element_markdown(size = 18, face = "bold"),
        plot.subtitle      = element_text(size = 14, color = "#36454f"),
        plot.caption       = ggtext::element_markdown(hjust = 0, size = 10, lineheight = 1.1),
        axis.text          = element_text(size = 10, color = "#36454f"),
        axis.title         = element_text(size = 12, face = "italic"),
        legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank()
      )
    
    r0 = covid19india::get_r_est(dat[, .(place, date, r_est = r_est, r_lower, r_upper)])[!is.nan(r)]
    
    fplot_colors = c(
      "alarm" = "#eb4034",
      "eh"    = "gray40",
      "good"  = "#138808",
      "india" = "black"
    )
    
    r_safe   = 0.85
    r_danger = 1
    
    r_est_for = r0 %>%
      mutate(
        fplot = ifelse(r > r_danger, "alarm", ifelse(r < r_safe, "good", "eh")),
        place = recode(place, "India" = "National estimate")
      ) %>%
      mutate(
        shape = ifelse(fplot == "india", "india", "not_india"),
        size  = ifelse(shape == "india", .5, .2),
        fplot = case_when(place == "National estimate" ~ "india", TRUE ~ fplot)
      ) %>%
      ggplot(aes(x = fct_relevel(reorder(place, r), "National estimate"), y = r, shape = shape)) +
      geom_hline(yintercept = r_safe, color = "gray40", linetype = 2) +
      geom_hline(yintercept = r_danger, color = "gray40", linetype = 2) +
      geom_pointrange(aes(ymin = lower, ymax = upper, color = fplot), size = 0.4) +
      scale_color_manual(values = fplot_colors) +
      scale_shape_manual(values = c("not_india" = 16, "india" = 18)) +
      labs(
        title    = "R for COVID-19 in India by state/union territory",
        subtitle = glue("as of {format(as.Date(today), '%B %e')}"),
        x        = "State/Union territory",
        y        = "R",
        caption  = glue("**\uA9 COV-IND-19 Study Group**<br>",
                        "**Source: ** COVID-19-Bharat: data.covid19bharat.org/",
                        "<br>**Note:**<br>",
                        " - Average estimate and 95% confidence interval for last 7 days are provided in each plot by state.<br>",
                        " - Colored red if estimate is above {r_danger} and green if below {r_safe}.<br>",
                        " - R values are not reliable when case counts are below 100.")
      ) +
      coord_flip(ylim = c(0, 3.5)) +
      covind19_base
    
    return(r_est_for)
    
  }
  
  r_forest_plot(d)
  ggsave(paste0(data_repo, subfolder_app, "r_forest_plot.png"),
         width = 10, height = 8, dpi = 300, units = "in", device='png')
}

write_r_forest()


