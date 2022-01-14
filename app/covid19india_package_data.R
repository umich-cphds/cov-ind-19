suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
  library(data.table)
  library(covid19india)
  library(gt)
  library(data.table)
  library(tidyfast)
  library(glue)
  library(janitor)
  library(lubridate)
  library(EpiEstim)
})

data_repo = Sys.getenv("data_repo")
data_repo = paste0(data_repo, "/source_data/")

subfolder_package = "/package-data/raw/"
subfolder_app = "/package-data/processed/"

# load data -----------
state_count_data <- get_state_counts()
fwrite(state_count_data, file = paste0(data_repo, subfolder_package, "state_count_data.csv"))

all_the_data     <- get_all_data()[, .(place, abbrev, date, r = r_est, lower = r_lower, upper = r_upper)]
fwrite(all_the_data, file = paste0(data_repo, subfolder_package, "all_the_data.csv"))

everything     <- get_all_data()
fwrite(everything, file = paste0(data_repo, subfolder_package, "everything.csv"))

nat_count_data   <- get_nat_counts()
fwrite(nat_count_data, file = paste0(data_repo, subfolder_package, "nat_count_data.csv"))

vax_data         <- get_state_vax()

testing_data     <- ((fread("https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/master/source_data/count_test_vax_latest.csv") %>%
                        as.data.table())[,
                                         .SD, .SDcols = c("state", "place", "date", "confirmed", "tested")
                        ][
                          , date := as.Date(date)
                        ][
                          order(date)
                        ][
                          , daily_confirmed := confirmed - shift(confirmed), by = "state"
                        ][
                          , daily_tested := tested - shift(tested), by = "state"
                        ][
                          , daily_tested := ifelse(daily_tested == 0 , NA, daily_tested), by = "state"
                        ][
                          , tpr := daily_confirmed / daily_tested
                        ])
fwrite(testing_data, file = paste0(data_repo, subfolder_package, "testing_data.csv"))

# extract names and abbrevs ----------
state_abbrev <- covid19india::pop[, .SD[1], by = place][order(place), abbrev]
state_names  <- covid19india::pop[, .SD[1], by = place][order(place), place]

# pre-process state vaccine data ---------

vax_data <- merge(vax_data, cbind(abbrev = state_abbrev, place = state_names), by = "place")
fwrite(vax_data, file = paste0(data_repo, subfolder_package, "vax_data.csv"))

metrics_table <- get_metrics_tables()$full
gtsave(metrics_table, paste0(data_repo, subfolder_package, "metrics_table_full.html"))

write_india_daily_barplot_data = function() {
  
  data <- nat_count_data
  
  data <- melt(data,
               id.vars       = c("place", "date"),
               measure.vars  = c("daily_cases", "daily_deaths", "daily_recovered"),
               variable.name = "Type",
               value.name    = "Count")[
                 , date.fmt := as.factor(format(date, format = "%b %e"))
               ][]
  
  data <- data[, lapply(.SD, function(x) replace(x, which(x == 0), NA))]
  
  data <- data[, Count := dt_fill(data, Count, .direction = "down")]
  
  data <- data[Type == "daily_cases", Type := "New Cases"][
    Type == "daily_recovered", Type := "Recovered"][
      Type == "daily_deaths", Type := "Fatalities"][
        , Type := factor(Type, levels = c("New Cases", "Fatalities", "Recovered"))][
          , count.fmt := format(Count, big.mark = ",", scientific = F, trim = T)
        ][
          , text := paste0(date.fmt, ": ", count.fmt, " ", Type)
        ][]
  
  fwrite(data, file = paste0(data_repo, subfolder_app, "india_daily_barplot.csv"))
  
}
write_india_daily_barplot_data()

write_case_death_country_comp = function() {
  
  Day_max          <- 100
  cases_threshold  <- 100
  deaths_threshold <- 3
  
  fmt  <- function(x) format(x, big.mark = ",", scientific = F, trim = T)
  
  # jhu data ----------
  not_these <- c("Province/State", "Lat", "Long")
  countries <- c("France", "Germany", "India", "Iran", "Italy", "Russia", "Brazil",
                 "Pakistan", "Bangladesh", "Korea, South", "US", "China", "Canada",
                 "Belgium", "Turkey", "Netherlands", "Switzerland", "United Kingdom"
  )
  
  read_n_clean <- function(x, val_name) {
    tmp <- fread(x, showProgress = FALSE)[, !..not_these]
    setnames(tmp, old = "Country/Region", new = "Country")
    tmp <- tmp[Country %in% countries][Country == "Korea, South", Country := "South Korea"][
      , lapply(.SD, sum, na.rm=TRUE), by = Country
    ]
    data.table::melt(tmp, id.vars = "Country", variable.name = "Date", value.name = val_name)[, Date := as.Date(Date, format = "%m/%d/%y")]
  }
  
  case_data <- read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv",
                            val_name = "Cases")
  death_data <- read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv",
                             val_name = "Deaths")
  recovered_data <- read_n_clean(x = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv", val_name = "Recovered")
  
  combined <- merge.data.table(x = case_data, death_data, by = c("Country", "Date"))
  combined <- merge.data.table(x = combined, recovered_data, by = c("Country", "Date"))
  
  combined <- rbindlist(list(
    combined[Country != "India"],
    nat_count_data[
      , .(Country = place, Date = date, Cases = total_cases,
          Deaths = total_deaths, Recovered = total_recovered)
    ][Date < min(combined[Country != "India", max(Date)], nat_count_data[, max(date)])][]
  ))
  
  max_date <- combined[, max(Date)]
  min_date <- unique(combined[, .SD[Cases >= cases_threshold], by = "Country"][
    , min(Date)])
  
  
  # cases data -----------
  cases_data <- combined[Cases >= cases_threshold][order(Date)][
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
  deaths_data <- combined[Deaths >= deaths_threshold][order(Date)][
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
  india_data <- nat_count_data
  
  india_cases <- india_data[total_cases >= cases_threshold][
    order(date), Day := 1:.N][Day > 30][]
  setnames(india_cases,
           old = c("place", "date", "daily_cases"),
           new = c("Country", "Date", "Incident_Cases"))
  india_cases <- india_cases[
    , Cases_fmt := fmt(Incident_Cases)
  ][
    , text := paste0(Country, "<br>", Date, ": ", Cases_fmt,
                     " incident cases<br>")
  ][
    , loess_cases := c(predict(loess(formula = Incident_Cases ~ Day, span = 0.025)))
  ][, .(Country, Date, Incident_Cases, Cases_fmt, text, loess_cases, Day)]
  
  india_deaths <- india_data[total_deaths >= deaths_threshold][
    order(date), Day := 1:.N][Day > 30][]
  setnames(india_deaths,
           old = c("place", "date", "daily_deaths"),
           new = c("Country", "Date", "Incident_Deaths"))
  india_deaths <- india_deaths[
    , Deaths_fmt := fmt(Incident_Deaths)
  ][
    , text := paste0(Country, "<br>", Date, ": ", Deaths_fmt,
                     " incident deaths<br>")
  ][
    , loess_deaths := c(predict(loess(formula = Incident_Deaths ~ Day, span = 0.025)))
  ][, .(Country, Date, Incident_Deaths, Deaths_fmt, text, loess_deaths, Day)]
  
  # combine ----------
  cases_data  <- rbindlist(list(cases_data, india_cases), fill = TRUE)
  deaths_data <- rbindlist(list(deaths_data, india_deaths), fill = TRUE)
  
  fwrite(cases_data, file = paste0(data_repo, subfolder_app, "case_death_country_comp_cases.csv"))
  fwrite(deaths_data, file = paste0(data_repo, subfolder_app, "case_death_country_comp_deaths.csv"))
  
  
}

write_case_death_country_comp()

write_india_cumul_perc_vax = function() {
  
  vax_dat <- vax_data[place == "India"]
  setnames(vax_dat, old = "date", new = "Day")
  
  #vax_india <- vax_dat[, text := paste0("India", "<br>", Day, ": ", format(pct_one_dose, big.mark = ",", digits = 1), "% with one dose<br>")][Day >= as.Date("2021-03-15")][]
  
  vax_india <- melt(vax_dat[,.(place, Day, pct_one_dose, pct_two_doses)], id.vars = c("place", "Day"),
                    measure.vars = c("pct_one_dose", "pct_two_doses"))
  
  vax_india <- vax_india[, text := paste0("India", "<br>", Day, ": ", round(value, digits = 2), ifelse(variable == "pct_one_dose", "% with one dose<br>", "% with two doses<br>"))][Day >= as.Date("2021-03-15")][]
  
  fwrite(vax_india, file = paste0(data_repo, subfolder_app, "india_cumul_perc_vax.csv"))
  
}
write_india_cumul_perc_vax()

write_india_daily_vax = function() {
  
  
  vax_dat <- vax_data[place == "India"]
  setnames(vax_dat, old = "date", new = "Day")
  vax_india <- vax_dat[, text := paste0("India", "<br>", Day, ": ", format(daily_doses, big.mark = ","), " daily vaccines<br>")][Day >= as.Date("2021-03-15")][]
  
  fwrite(vax_india, file = paste0(data_repo, subfolder_app, "india_daily_vax.csv"))
  
  
}
write_india_daily_vax()

write_india_seir = function() {
  
  state     = "India"
  repo_seir = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19-data/IRIS/source_data/seir/prediction_tt_latest.txt"
  
  data      = fread(repo_seir)[
    , date := as.Date(date)
  ][
    state == "India"
  ][
    , timestamp := .SD[pred == 0, max(date)]
  ][
    pred == 1
  ][
    date <= Sys.Date() + 30
  ]
  
  data = (((data[
    , c("state", "date", "section", "pred", "mean")
  ][
    , value := mean
  ][
    , mean := NULL
  ][
    
  ] %>%
    dcast(formula = date ~ section, value.var = "value"))[][
      , case_daily_reported := positive_daily_reported
    ][
      , death_daily_unreported := death_unreported - lag(death_unreported)
    ][
      , case_daily_unreported := unreported_daily
    ][
      , c("date", "case_daily_reported", "death_daily_reported")
    ])[] %>%
    melt(id.vars = c("date")))[][
      , text := paste0(format(round(value, digits = 0), big.mark=","), " daily ",
                       str_extract(variable, "[^_]+"), "s ", "on ", date)
    ]
  
  fwrite(data, file = paste0(data_repo, subfolder_app, "india_seir.csv"))
  
}
write_india_seir()

write_snapshot = function() {
  
  
  snapshot <- function() {
    # functions -----------
    get_snap <- function(t = Sys.Date() - 1) {
      
      nat <- nat_count_data
      
      vax_dat <- vax_data[date <= nat[, max(date)]][place == "India", .(date, daily_doses)][, lag := daily_doses][]
      
      test_data <- testing_data %>%
        filter(place == "tt") %>%
        mutate(confirmed = confirmed - lag(confirmed),
               tested    = tested - lag(tested)) %>%
        group_by(date) %>%
        mutate(ntpr = confirmed / tested) %>%
        filter(tested > 100) %>%
        as.data.table()
      
      if (!is.null(t)) {
        today <- as.Date(t)
      } else {
        today     <- min(max(nat$date, na.rm = TRUE),
                         max(vax_dat$date, na.rm = TRUE))
      }
      
      yesterday <- today - 1
      week_ago  <- today - 7
      month_ago <- today - 30
      
      get_stats <- function(d) {
        
        tmp_nat <- nat[date == d]
        tmp_deaths <- tmp_nat[, daily_deaths]
        tmp_cases  <- tmp_nat[, daily_cases]
        tmp_vax <- vax_dat[date == d, lag]
        tmp_tpr <- test_data[date == d, ntpr]
        tmp_test <- test_data[date == d, tested]
        
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
      
      today_stats     <- get_stats(today)
      yesterday_stats <- get_stats(yesterday)
      week_ago_stats  <- get_stats(week_ago)
      month_ago_stats <- get_stats(month_ago)
      
      rbindlist(list(
        today_stats,
        yesterday_stats,
        week_ago_stats,
        month_ago_stats
      ))
      
    }
    
    make_pretty <- function(x) {
      
      source_note_text <- glue::glue(
        "**\uA9 COV-IND-19 Study Group**<br>**Source data:** count data (mohfw.gov.in), vaccine data (cowin.gov.in), testing data (covid19bharat.org)<br>"
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
    snap <- get_snap()
    
    # today <- as.Date(snap[Day == "Today", Date], "%m/%d")
    
    return(make_pretty(snap))
  }
  
  gtsave(snapshot(), paste0(data_repo, subfolder_app, "snapshot.html"))
  
}
write_snapshot()

write_r_forest = function() {
  
  r_forest_plot <- function(x) {
    
    today <- max(x[, date])
    
    # ggplot theme ------------
    covind19_base <- theme_minimal() +
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
    
    r0 <- covid19india::get_r_est(x[, .(place, date, r_est = r, r_lower = lower, r_upper = upper)])[!is.nan(r)]
    
    fplot_colors <- c(
      "alarm" = "#eb4034",
      "eh"    = "gray40",
      "good"  = "#138808",
      "india" = "black"
    )
    
    r_safe   <- 0.85
    r_danger <- 1
    
    r_est_for <- r0 %>%
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
                        "**Source:** Up to 10/17/2021: covid19india.org. After 10/17/2021: count data (mohfw.gov.in), vaccine data (cowin.gov.in)",
                        "<br>**Note:**<br>",
                        " - Average estimate and 95% confidence interval for last 7 days are provided in each plot by state.<br>",
                        " - Colored red if estimate is above {r_danger} and green if below {r_safe}.<br>",
                        " - R values are not reliable when case counts are below 100.")
      ) +
      coord_flip(ylim = c(0, 3.5)) +
      covind19_base
    
    return(r_est_for)
    
  }
  
  r_forest_plot(all_the_data)
  ggsave(paste0(data_repo, subfolder_app, "r_forest_plot.pdf"))
}
write_r_forest()


write_state_cumul_perc_vax = function() {
  forecast = "tt"
  vax_dat <- vax_data[abbrev == forecast]
  setnames(vax_dat, old = "date", new = "Day")
  
  #vax_india <- vax_dat[, text := paste0("India", "<br>", Day, ": ", format(pct_one_dose, big.mark = ",", digits = 1), "% with one dose<br>")][Day >= as.Date("2021-03-15")][]
  
  vax_india <- melt(vax_dat[,.(place, Day, pct_one_dose, pct_two_doses)], id.vars = c("place", "Day"),
                    measure.vars = c("pct_one_dose", "pct_two_doses"))
  
  vax_india <- vax_india[, text := paste0("India", "<br>", Day, ": ", round(value, digits = 2), ifelse(variable == "pct_one_dose", "% with one dose<br>", "% with two doses<br>"))][Day >= as.Date("2021-03-15")][]
  
  fwrite(vax_india, file = paste0(data_repo, subfolder_app, "state_cumul_perc_vax.csv"))
  
}
write_state_cumul_perc_vax()

write_tpr_national = function() {
  
  all_data = testing_data
  
  all_data <- all_data[state == "India"]
  
  plot_data <- na.omit(all_data[
    , text := paste0("Date: ", format(date, format = '%b %d'), "<br>TPR: ",
                     format(round(tpr*100, 2), nsmall = 2), "%")
  ][])[
    order(date)
  ]
  
  plot_data <- plot_data[date >= "2020-03-15"]
  
  fwrite(plot_data, file = paste0(data_repo, subfolder_app, "tpr_national.csv"))
  
}
write_tpr_national()

write_tvr_national = function() {
  
  all_data <- all_the_data
  
  all_data <- all_data[place == "India"]
  
  plot_data <- all_data[
    , text := paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                     format(round(r, 2), nsmall = 2), "<br>CI: ",
                     paste0("[", format(round(lower, 2), nsmall = 2), ", ",
                            format(round(upper, 2), nsmall = 2), "]"))
  ][]
  
  plot_data <- plot_data[date >= "2020-03-15"]
  
  fwrite(plot_data, file = paste0(data_repo, subfolder_app, "tvr_national.csv"))
  
}
write_tvr_national()