
library(tidyverse)
library(httr)

path_historical = "https://data.covid19bharat.org/v4/min/timeseries.min.json"
path_current = "https://data.covid19bharat.org/v4/min/data.min.json"

raw_historical = content(GET(path_historical))
raw_current = content(GET(path_current))

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
  select(-other, -vaccinated1, -vaccinated2) %>% 
  mutate(place = tolower(x1)) %>% 
  select(place, date, confirmed, recovered, deceased, tested)

raw_current = 
  path_current %>% 
  GET() %>% 
  content() %>% 
  unlist() %>% 
  enframe() %>% 
  separate(name, into = paste0("x", 1:10)) %>% 
  filter((x2 %in% c("total") & x3 %in% c("tested", "confirmed", "recovered", "deceased")) | (x2 == "meta" & x3 == "date")) %>% 
  select(x1, x3, value) %>% 
  mutate(place = tolower(x1)) %>% 
  select(-x1) %>% 
  pivot_wider(names_from = "x3")

