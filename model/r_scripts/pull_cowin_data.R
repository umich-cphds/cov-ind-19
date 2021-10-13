# script does not rely on {covid19india} package
# pulls and processes data directly from cowin.gov.in

# libraries -----------
library(data.table)
library(httr)
library(glue)
library(cli)

# data_repo path ---------
data_repo <- paste0(Sys.getenv("data_repo"), "/")

# load latest content ----------
request <- httr::GET("https://api.cowin.gov.in/api/v1/reports/v2/getPublicReports")
content <- httr::content(request)

# check for latest pull ----------
if (file.exists(paste0(data_repo, "source_data/vax/cowin_vax_latest.csv"))) {

latest      <- fread(paste0(data_repo, "source_data/vax/cowin_vax_latest.csv"))
latest_date <- max(latest[, date])

  if (latest_date >= as.Date(content$timestamp)){
    cli::cli_alert_info(glue("Data for {format(as.Date(content$timestamp), '%Y%m%d')} already exists, not modifying csv"))
  }

}

# process ---------
latest_vax_data <- rbindlist(content[["getBeneficiariesGroupBy"]])[, !c("state_id", "id", "title", "today")]

setnames(latest_vax_data,
         old = c("state_name", "total", "partial_vaccinated", "totally_vaccinated"),
         new = c("place", "total_doses", "first_dose", "second_dose"))

dd <- latest_vax_data[place %in% c("Dadra and Nagar Haveli", "Daman and Diu")][, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("total_doses", "first_dose", "second_dose")][, place := "Dadra and Nagar Haveli and Daman and Diu"][]

india <- latest_vax_data[, lapply(.SD, sum, na.rm = TRUE), .SDcols = c("total_doses", "first_dose", "second_dose")][, place := "India"][]

vax <- rbindlist(list(
  latest_vax_data[!(place %in% c("Dadra and Nagar Haveli", "Daman and Diu"))],
  dd,
  india
), use.names = TRUE)[, `:=` (
  date          = as.Date(content$timestamp),
  raw_timestamp = content$timestamp,
  pull_time     = Sys.time()
  )][]

# append to existing data
if (file.exists(paste0(data_repo, "source_data/vax/cowin_vax_latest.csv"))) {
  if (exists("latest_date") & (latest_date < as.Date(content$timestamp))) {
  vax <- rbindlist(list(
    latest[, date := as.Date(date)],
    vax[, raw_timestamp := as.POSIXct(raw_timestamp)]
  ), use.names = TRUE, fill = TRUE)
  }
}
setcolorder(vax, neworder = c("place", "date"))

# save -----------
if (exists("latest_date")) {
  if (latest_date < as.Date(content$timestamp)) {
    fwrite(x = vax, file = paste0(data_repo, "source_data/vax/cowin_vax_", format(as.Date(content$timestamp), '%Y%m%d'), ".csv"))
    fwrite(x = vax, file = paste0(data_repo, "source_data/vax/cowin_vax_latest.csv"))
  } else {
    cli::cli_alert_info(glue("Data for {as.Date(content$timestamp)} already exists, not modifying csv"))
  }
} else {
  fwrite(x = vax, file = paste0(data_repo, "source_data/vax/cowin_vax_latest.csv"))
}
