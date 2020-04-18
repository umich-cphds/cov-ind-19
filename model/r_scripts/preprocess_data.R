library(httr)
library(tidyverse)
library(vroom)

# Set variables based on testing or production
if ( Sys.getenv("production") == "TRUE" ) {
	data_repo <- "~/cov-ind-19-data/"
} else {
	data_repo <- "~/cov-ind-19-test/"
}

today     <- Sys.getenv("today")
if (!dir.exists(paste0(data_repo, today))) {
    message("Creating" , paste0(data_repo, today))
    dir.create(paste0(data_repo, today), recursive = T)
}

start.date <- as.Date("2020-03-01")

countries <- c("France", "Germany", "India", "Iran", "Italy",
               "Korea, South", "US", "China", "Canada", "Belgium", "Turkey",
               "Netherlands", "Switzerland", "United Kingdom"
)

jhu.path <- paste0("~/COVID-19/csse_covid_19_data/csse_covid_19_time_series")

jhu.files <- list(
    Cases      = paste0(jhu.path, "/time_series_covid19_confirmed_global.csv"),
    Recovered  = paste0(jhu.path, "/time_series_covid19_recovered_global.csv"),
    Deaths     = paste0(jhu.path, "/time_series_covid19_deaths_global.csv")
)

jhu.data <- reduce(imap(jhu.files,
    function(file, var)
    {
        vroom(file) %>%
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
        # filter(Date >= start.date - 1)
    }
), ~ left_join(.x, .y)) %>%
ungroup() %>%
arrange(Country, Date) %>%
vroom_write(path = paste0(data_repo, today, "/jhu_data.csv"))


states.map <- c("Andhra Pradesh" =  "AP", "Arunachal Pradesh" =  "AR",
	"Assam" =  "AS", "Bihar" =  "BR", "Chhattisgarh" =  "CG", "Goa" =  "GA",
	"Gujarat" =  "GJ", "Haryana" =  "HR", "Himachal Pradesh" =  "HP",
	"Jammu and Kashmir" =  "JK", "Jharkhand" =  "JH", "Karnataka" =  "KA",
	"Kerala" =  "KL", "Madhya Pradesh" =  "MP",  "Maharashtra" =  "MH",
	"Manipur" =  "MN", "Meghalaya" =  "ML", "Mizoram" =  "MZ", "Nagaland" =  "NL",
	"Orissa" =  "OR", "Punjab" =  "PB", "Rajasthan" =  "RJ", "Sikkim" =  "SK",
	"Tamil Nadu" =  "TN", "Tripura" =  "TR", "Uttarakhand" =  "UK",
	"Uttar Pradesh" =  "UP", "West Bengal" =  "WB", "Tamil Nadu" =  "TN",
	"Tripura" =  "TR", "Andaman and Nicobar Islands" =  "AN",
	"Chandigarh" =  "CH", "Dadra and Nagar Haveli" =  "DH",
	"Daman and Diu" = "DD", "Delhi" =  "DL", "Lakshadweep" =  "LD",
	"Pondicherry" =  "PY", "Telangana" =  "TG", "Dadra and Nagar Haveli" =  "DN",
	"Chhattisgarh" =  "CT", "Ladakh" =  "LA", "Uttarakhand" =  "UT"
)

x <- names(states.map)
names(x) <- states.map

# get state level data from covid19india.org and preprocess it
request <- GET("https://api.covid19india.org/states_daily.json")
json    <- content(request)
data    <- map_dfr(json[[1]], ~ .x)

data$tt <- NULL
state.codes <- setdiff(names(data), c("date", "status"))
data <- data %>% gather(!!state.codes, key = state, value = count) %>%
mutate(
    count = as.numeric(count),
    date = as.Date(date, format = "%d-%b-%y")
) %>%
spread(status, count, fill = 0) %>%
rename(
    Cases = Confirmed,
    Deaths = Deceased,
    Date = date,
    State = state
) %>%
mutate(
	Name = recode(str_to_upper(State), !!!x)
) %>%
arrange(State, Date) %>%
group_by(State) %>%
mutate(
    Cases = accumulate(Cases, `+`),
    Deaths = accumulate(Deaths, `+`),
    Recovered = accumulate(Recovered, `+`)
) %>%
ungroup() %>%
filter(Date >= "2020-03-15" & Date < today) %>%
vroom_write(path = paste0(data_repo, today, "/covid19india_data.csv"))


request <- GET("https://api.covid19india.org/data.json")
json    <- content(request)
data    <- map_dfr(json[[1]], ~ .x)

data <- data %>%
select(
    Cases = totalconfirmed,
    Deaths = totaldeceased,
    Recovered = totalrecovered,
    Date = date
) %>%
mutate(
    Date = as.Date(paste0(Date, "2020"), format = "%d %B %Y"),
    Cases = as.numeric(Cases),
    Deaths = as.numeric(Deaths),
    Recovered = as.numeric(Recovered),
    Country = "India"
) %>% filter (Date < today)


rbind(filter(jhu.data, Country != "India"), data) %>%
vroom_write(path = paste0(data_repo, today, "/jhu_data_mod.csv"))
