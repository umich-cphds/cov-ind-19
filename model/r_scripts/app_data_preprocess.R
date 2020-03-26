library(tidyverse)
library(vroom)

reports <- JHU_COVID19_PATH

files <- grep(".csv", list.files(reports, full.names = T), value = T)
dates <- as.Date(gsub(".+/|.csv", "", files), format = format("%m-%d-%y"))

vroom_write(map2_df(files, dates,
    function(file, date)
    {
        vroom(file) %>%
        select(Recovered, Confirmed, Deaths, Country = matches("Country")) %>%
        group_by(Country) %>%
        mutate_all(list(~ ifelse(is.na(.x), 0, .x))) %>%
        summarise_all(sum) %>%
        mutate(Date = date)
    }
), path = "jhu_data_collated.csv")
