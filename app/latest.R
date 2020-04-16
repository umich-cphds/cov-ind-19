library(jsonlite)
library(httr)

get_latest <- function(github.path)
{
    github.auth <- read_json(".github.json")

    auth    <- authenticate(github.auth$user, github.auth$key)
    request <- GET(github.path, auth)

    stop_for_status(request)

    header <- headers(request)

    if (http_type(request) != "application/json")
        stop(header$date, ": GET did not result in the correct content type.")

    limit     <- as.numeric(header["x-ratelimit-limit"])
    remaining <- as.numeric(header["x-ratelimit-remaining"])

    if (limit == 60)
        warning(header$date, ": Github authorization failed",
                ". Limited to 60 queries an hour!")

    if (remaining < limit / 10)
        warning(header$date, ": ", remaining, " remaining api calls!")

    json <- content(request)

    trees <- keep(json$tree, ~.x$type == "tree")
    tree <-  trees[[which.max(as.Date(map_chr(trees, ~ .x$path)))]]


    trees <- keep(json$tree, ~.x$type == "tree")
    tree <-  trees[[which.max(as.Date(map_chr(trees, ~ .x$path)))]]

    latest <- as.Date(tree$path)
}
