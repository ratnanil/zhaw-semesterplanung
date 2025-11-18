# Semesterplanung


FS

- Circular Economy Management (Di Nachmittag)
- Patterns and Trends (Mi Vormittag, 2. Quartal)
- Remote Sensing UI (Montag Vormittag, 2 Einheiten)
- ADLS Spatiotemproral DataScience (Di Vormittag)

HS

- Research Methods (Mo / Di Vormittag)
- ADLS Image Processing for Remote Sensing
- ADLS GIScience and Geodatabases
- UI Geospatial Computing
- ADLS DataLab

``` r
library(readODS)
library(calendar)
library(tidyr)
library(dplyr)
```


    Attaching package: 'dplyr'

    The following objects are masked from 'package:stats':

        filter, lag

    The following objects are masked from 'package:base':

        intersect, setdiff, setequal, union

``` r
library(hms)
library(lubridate)
```


    Attaching package: 'lubridate'

    The following object is masked from 'package:hms':

        hms

    The following objects are masked from 'package:base':

        date, intersect, setdiff, union

``` r
library(ISOweek)
library(glue)
library(stringr)
library(purrr)
ISOweek2date(weekdate = "2025-W47-2")
```

    [1] "2025-11-18"

``` r
year <- 2026
fs26 <- read_ods("Modultage.ods", "FS26") |> 
  fill(Studiengang, Modul, Weekday) |> 
  filter(is.na(Ex)) |> 
  select(-Ex)


events <- fs26 |>
  mutate(
    Kalenderwoche = str_pad(Kalenderwoche, 2, pad = "0"),
    isoweekdate = glue("{year}-W{Kalenderwoche}-{Weekday}"),
    date = ISOweek2date(isoweekdate),
    date_from = as.POSIXct(paste(date, as.character(Von)), tz = "CET"),
    date_to = as.POSIXct(paste(date, as.character(Bis)), tz = "CET"),
    summary = glue("{Studiengang}: {Modul} ({Topic})")
    # description = glue("{Topic}\n{Comment}", .na = "")
  )

# Create iCalendar from dataframe directly
cal <- events |>
  mutate(
    UID = paste0(Studiengang, "_", Modul, "_W", Semesterwoche, "@zhaw-semesterplanung")
  ) |>
  select(
    UID,
    DTSTART = date_from,
    DTEND = date_to,
    SUMMARY = summary
  ) |>
  ical()

# Write to single .ics file
ic_write(cal, "calendar.ics")
```
