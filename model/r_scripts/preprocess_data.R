# libraries ----------
suppressPackageStartupMessages({
  library(httr)
  library(tidyverse)
  library(data.table)
  library(covid19india)
})

# environment variables ----------
code_repo <- Sys.getenv("code_repo")
data_repo <- Sys.getenv("data_repo")
today     <- Sys.getenv("today")

# directory ----------
if (!dir.exists(paste0(data_repo, "/", today))) {
    message("Creating" , paste0(data_repo, "/", today))
    dir.create(paste0(data_repo, "/", today), recursive = T)
}

# script variables ---------
start.date <- as.Date("2020-03-01")

countries <- c("France", "Germany", "India", "Iran", "Italy", "Russia",
               "Brazil","Pakistan", "Bangladesh", "Korea, South", "US", "China",
               "Canada","Belgium", "Turkey", "Netherlands", "Switzerland",
               "United Kingdom"
)

# JHU data ---------
jhu_path <- paste0("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

jhu_files <- list(
    Cases      = paste0(jhu_path, "/time_series_covid19_confirmed_global.csv"),
    Recovered  = paste0(jhu_path, "/time_series_covid19_recovered_global.csv"),
    Deaths     = paste0(jhu_path, "/time_series_covid19_deaths_global.csv")
)

jhu_data <- reduce(imap(jhu_files,
    function(file, var) {
        data.table::fread(file, showProgress = FALSE) %>%
        select(Country = matches("Country"), matches("[0-9]+")) %>%
        filter(Country %in% countries) %>%
        mutate(Country = as.factor(case_when(
            Country == "Korea, South" ~  "South Korea",
            TRUE ~ Country))
        ) %>%
        group_by(Country) %>%
        # Since we don't care about counts in each state we collapse into a
        # single count per country of interest.
        summarise_all(sum, na.rm = T) %>%
        gather(matches("[0-9]+"), key = "Date", value = !!var) %>%
        mutate(Date = as.Date(Date, format = "%m/%d/%y")) %>%
        group_by(Date, )
    }
), ~ left_join(.x, .y, by = c("Country", "Date"))) %>%
ungroup() %>%
arrange(Country, Date)

data.table::fwrite(x = jhu_data, file = paste0(data_repo, "/", today, "/jhu_data.csv"))

# state count data ----------
state_count <- covid19india::get_state_counts()[
  , .(Date = date, Name = place, Cases = total_cases,
      Deaths = total_deaths, Recovered = total_recovered)
]

state_count <- data.table::merge.data.table(
  state_count,
  covid19india::pop[, .(Name = place, State = abbrev)][rowid(Name) == 1],
  by = "Name", all.x = TRUE)

data.table::fwrite(state_count,
                   paste0(data_repo, "/", today, "/covid19india_data.csv"))

# national count data -----------
national_count <- covid19india::get_nat_counts()[
  , .(Country = place, Date = date, Cases = total_cases,
      Deaths = total_deaths, Recovered = total_recovered)
][Date < today][]

rbind(filter(jhu_data, Country != "India"), national_count) %>%
data.table::fwrite(paste0(data_repo, "/", today, "/jhu_data_mod.csv"))

# national testing data ----------
national_testing <- covid19india::get_nat_tests()[
  , .(Tests = total_tests, Date = date, Country = place)
][]

data.table::fwrite(national_testing, paste0(data_repo, "/", today, "/testing.csv"))

# global testing data ----------
data.table::fwrite(data.table::fread('https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv'),
          paste0(data_repo, "/", today, "/global_testing.csv"))

# statewise testing data [DEPRECATED] ----------
# data.table::fwrite(data.table::fread('https://api.covid19india.org/csv/latest/statewise_tested_numbers_data.csv'),
#           paste0(data_repo, "/", today, '/statewise_tested_numbers_data.csv'))

# replace cleanr_covind Rscript -----------
all_data <- covid19india::get_all_data()
data.table::fwrite(all_data, paste0(data_repo, "/", today, "/everything.csv"))
data.table::fwrite(covid19india::get_cfr(all_data), paste0(data_repo, "/", today, '/cfr_t7_avg.csv'))
data.table::fwrite(covid19india::get_r_est(all_data), paste0(data_repo, "/", today, '/r0_t7_avg.csv'))

