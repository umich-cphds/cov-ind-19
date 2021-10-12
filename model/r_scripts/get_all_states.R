suppressPackageStartupMessages({
  library(data.table)
  library(covid19india)
})

data_repo <- Sys.getenv("data_repo")
today     <- Sys.getenv("today")

x <- covid19india::get_state_counts()[, .SD[date == max(date)], by = "place"]

x <- merge.data.table(x, covid19india::pop[, !c("population")][rowid(place) == 1], by = "place")
setnames(x, old = "abbrev", new = "State")

x$State[x$State == "ct"] <- "cg"
x$State[x$State == "ut"] <- "uk"

cat(x$State, "\n")
