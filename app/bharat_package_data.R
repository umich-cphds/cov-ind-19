
# path -----------
Sys.getenv("data_repo") #data_repo = "../../cov-ind-19-data/"
data_repo = paste0(data_repo, "/source_data/")

subfolder_package = "/package-data/raw/"
subfolder_app = "/package-data/processed/"

# load data -----------

# new data format
#source("https://gitlab.com/-/snippets/2391974/raw/main/micro_covid19india.R")
# copied source directly and edited the text columns
## helper function for calculating R
estR0_out <- function(x, incubation_days = 3) {
  
  t_start   <- seq(2, nrow(x) - 4)
  t_end     <- t_start + 4
  
  res <- EpiEstim::estimate_R(
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
  
  tmp_out <- data.table::merge.data.table(
    data.table::data.table(date_num = res$dates), res$R, by.x = "date_num", by.y = "t_end", all.x = TRUE)[
      , .(r = `Mean(R)`, lower = `Quantile.0.025(R)`, upper = `Quantile.0.975(R)`)][
        , `:=` (date = x$date, place = x$place)]
  data.table::setcolorder(tmp_out, "date")
  
  return(tmp_out[])
  
}

## calculate r0
get_r0 <- function(
    dat,
    daily_filter = 0,
    total_filter = 50,
    min_date     = "2020-03-23",
    inc_days     = c(7, 5, 3)
) {
  
  tmp_dat <- dat[daily_cases > daily_filter & total_cases >= total_filter][, ns := .N, by = "place"][ns >= 7]
  
  a <- c(as.Date("2020-12-31"), as.Date("2021-12-31"), Sys.Date())
  b <- as.Date(c(min_date, "2021-01-01", "2022-01-01"))
  c <- c(1:3)
  suppressWarnings({
    out <- purrr::map_dfr(c,
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


covid19bharat_ts <- function(
    path_to_json = "https://data.covid19bharat.org/v4/min/timeseries.min.json",
    path_to_pop = "https://raw.githubusercontent.com/umich-cphds/cov-ind-19/master/model/populations.csv",
    inc_days = c(7, 5, 3)
) {
  
  # read json data
  raw_json <- rjson::fromJSON(file = path_to_json)
  
  # process raw json data
  processed_json <- data.table::as.data.table(rrapply::rrapply(raw_json, how = "melt"))[]
  
  # do some clean up
  data.table::setnames(processed_json, old = c("L1", "L3", "L4", "L5"), new = c("abbrev", "date", "period", "stat"))
  processed_json <- processed_json[, `:=` (
    L2 = NULL,
    date = as.Date(date)
  )][!(stat %in% c("other"))]
  
  # pull daily and total data
  dats <- purrr::map2(c("daily", "total"), c("delta", "total"),
                      ~{
                        
                        tmp <- data.table::dcast(processed_json[period == .y],
                                                 abbrev + date ~ stat, value.var = "value")
                        data.table::setnames( tmp, c("confirmed", "deceased", "recovered", "tested", "vaccinated1", "vaccinated2", "precautionary"),paste0(.x, c("_cases", "_deaths", "_recovered", "_tests", "_first_dose", "_second_dose", "_precaution_dose")))
                        
                        data.table::setkeyv(tmp, c("abbrev", "date"))
                        
                        return(tmp)
                        
                      })
  
  pop <- data.table::fread(path_to_pop, showProgress = FALSE)[, .(place = full, abbrev, population)][]
  
  out <- purrr::reduce(dats, data.table::merge.data.table)[, abbrev := tolower(abbrev)] |>
    data.table::merge.data.table(pop, by = "abbrev")
  
  out <- out[, `:=` (
    pct_one_dose = round(total_first_dose * 100 / population, 2),
    pct_two_doses = round(total_second_dose * 100 / population, 2),
    tpr = round(daily_cases / daily_tests, 4)
  )][, !c("population")]
  
  out[order(date), daily_doses := daily_first_dose + daily_second_dose + (total_precaution_dose - data.table::shift(total_precaution_dose)), by = place]
  
  data.table::setcolorder(out, "place")
  
  out <- data.table::merge.data.table(
    out,
    get_r0(dat = out, inc_days = inc_days)[, .(place, date, r_est = r, r_lower = lower, r_upper = upper)],
    by = c("place", "date"),
    all.x = TRUE
  )
  
  # text columns
  out = out[, 
    daily_cases_text := paste0(as.factor(format(date, format = "%b %e")), ": ", 
      format(daily_cases, big.mark = ",", scientific = F, trim = T), " New Cases")
  ][,
    daily_deaths_text := paste0(as.factor(format(date, format = "%b %e")), ": ", 
      format(daily_deaths, big.mark = ",", scientific = F, trim = T), " Fatalities")
  ][,
    daily_recovered_text := paste0(as.factor(format(date, format = "%b %e")), ": ", 
      format(daily_recovered, big.mark = ",", scientific = F, trim = T), " Recoveries")
  ][,
    r := r_est
  ][,
    r_text := paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
      format(round(r, 2), nsmall = 2), "<br>CI: ", 
      paste0("[", format(round(r_lower, 2), nsmall = 2), ", ", 
        format(round(r_upper, 2), nsmall = 2), "]"))
  ][,
    tpr_text := paste0("Date: ", format(date, format = '%b %d'), "<br>TPR: ",
      format(round(tpr*100, 2), nsmall = 2), "%")
  ][,
    pct_one_dose_text := paste0(place, "<br>", date, ": ", pct_one_dose, " % one dose<br>"), 
  ][,
    pct_two_doses_text := paste0(place, "<br>", date, ": ", pct_two_doses, " % two doses<br>")
  ]
  
  return(out)
  
}

dat = covid19bharat_ts()

readr::write_csv(dat, file = paste0(paste0(data_repo, subfolder_app), "new_everything.csv"))
