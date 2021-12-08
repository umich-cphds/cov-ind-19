suppressPackageStartupMessages({
  library(tidyverse)
  library(httr)
})

data_repo = Sys.getenv("data_repo")
data_file = paste0(data_repo, "/source_data/", "count_test_vax_latest.csv")

path_historical = "https://data.covid19bharat.org/v4/min/timeseries.min.json"
path_current = "https://data.covid19bharat.org/v4/min/data.min.json"

raw_historical = content(GET(path_historical))
raw_current = content(GET(path_current))

today = Sys.getenv("today")

raw_historical = 
  path_historical %>% 
  GET() %>% 
  content() %>% 
  unlist() %>% 
  enframe() %>% 
  separate(name, into = paste0("x", 1:10)) %>% 
  mutate(date = paste(x3, x4, x5, sep = "-")) %>% 
  select(-x2, -x3, -x4, -x5, -x8, -x9, -x10) %>%
  filter(x6 == "total") %>% 
  select(-x6) %>%
  pivot_wider(names_from = "x7") %>% 
  select(-other) %>% 
  mutate(place = tolower(x1)) %>% 
  select(place, date, confirmed, recovered, deceased, tested, vaccinated1, vaccinated2) %>% 
  filter(date != today)

raw_current = 
  path_current %>% 
  GET() %>% 
  content() %>% 
  unlist() %>% 
  enframe() %>% 
  separate(name, into = paste0("x", 1:10)) %>% 
  filter((x2 %in% c("total") & x3 %in% c("tested", "confirmed", "recovered", 
    "deceased", "vaccinated1", "vaccinated2")) | (x2 == "meta" & x3 == "date")) %>% 
  select(x1, x3, value) %>% 
  mutate(place = tolower(x1)) %>% 
  select(-x1) %>% 
  pivot_wider(names_from = "x3") %>% 
  mutate(confirmed   = as.numeric(confirmed), 
         recovered   = as.numeric(recovered), 
         deceased    = as.numeric(deceased), 
         tested      = as.numeric(tested), 
         vaccinated1 = as.numeric(vaccinated1), 
         vaccinated2 = as.numeric(vaccinated2))

data = 
  raw_historical %>% 
  bind_rows(raw_current)

write.csv(data, file = data_file)
