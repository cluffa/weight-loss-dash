library(readr)
library(dplyr)
library(lubridate)

years_ago <- Sys.Date() - years(4)

WEIGHT_URL <- "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv"
LOSEIT_URL <- "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1838432377&format=csv"

get_weight <- function(url = WEIGHT_URL) {
    read_csv(
        url,
        col_names = c("date", "weight"),
        col_types = "cn---",
        skip = 1,
        lazy = TRUE
    ) |>
    mutate(date = as_datetime(date) |>
    force_tz(tzone = "EST")) |>
    filter(date >= years_ago) |>
    arrange(date) |>
    select(date, weight)
}

get_loseit <- function(url = LOSEIT_URL) {
    KGCONST <- 0.4536 # lbs to kg
    HEIGHT <- 188 # cm
    AGE <- 26
    ACTIVITY <- 1.4
    
    mifflin <- function(weight, isMale = TRUE) {
        (10 * weight * KGCONST) + (6.25 * HEIGHT) - (5 * AGE) + ifelse(isMale, 5,-161)
    }
    
    out <- read_csv(
        url,
        skip = 1,
        col_names = c(
            "date",
            "budget",
            "food",
            "exercise",
            "net",
            "difference",
            "weight",
            "weighed",
            "carbp",
            "fatp",
            "garmin",
            "proteinp",
            "protein",
            "sugar",
            "goal_deficit"
        ),
        col_types = "c-nn--n-ccncnnn",
        na = c("-")
    ) |> mutate(
        date = as_datetime(date, format = "%m/%d/%y") |> force_tz(tzone = "EST"),
    ) |> filter(
        date >= years_ago
    ) |> mutate(
        food = if_else(food < 1100, NA_real_, food),
        tdee = if_else(is.na(garmin),
                       mifflin(weight) * ACTIVITY,
                       garmin),
        diff = if_else(is.na(garmin),
                       food - tdee - exercise,
                       food - garmin),
        tdee_method = if_else(is.na(garmin),
                              "Mifflin-St Jeor",
                              "Garmin"),
        carbp = as.numeric(parse_number(carbp)),
        fatp = as.numeric(parse_number(fatp)),
        proteinp = as.numeric(parse_number(proteinp))
    )
    
    week <- out |>
        mutate(weekdate = floor_date(
            date,
            unit = "1 week",
            week_start = getOption("lubridate.week.start", 1)
        )) |>
        group_by(weekdate) |>
        reframe(date = date,
                diff = mean(diff, na.rm = TRUE)) |>
        mutate(diff = if_else(is.na(out$food),
                              NA_real_,
                              diff)) |>
        select(diff)
    
    out$weekdiff <- week$diff
    
    return(out)
}
