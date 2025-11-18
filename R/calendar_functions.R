#' Extract year from sheet name
#'
#' @param sheet_name Name of the sheet (e.g., "FS26", "HS26")
#' @return Four-digit year (e.g., 2026)
#'
#' @examples
#' extract_year_from_sheet("FS26")  # Returns 2026
#' extract_year_from_sheet("HS27")  # Returns 2027
extract_year_from_sheet <- function(sheet_name) {
  # Extract the 2-digit year from sheet name (e.g., "FS26" -> "26")
  year_suffix <- stringr::str_extract(sheet_name, "\\d{2}$")

  if (is.na(year_suffix)) {
    stop("Could not extract year from sheet name: ", sheet_name)
  }

  # Convert to 4-digit year (assuming 20xx)
  year <- as.numeric(paste0("20", year_suffix))

  return(year)
}


#' Process ODS sheet and create iCalendar file
#'
#' @param ods_file Path to the ODS file
#' @param sheet_name Name of the sheet to process
#' @param output_dir Directory where .ics file should be saved (default: "ics")
#'
#' @return Path to the created .ics file
#'
#' @examples
#' process_sheet_to_ics("Modultage.ods", "FS26")
process_sheet_to_ics <- function(ods_file, sheet_name, output_dir = "ics") {

  # Extract year from sheet name
  year <- extract_year_from_sheet(sheet_name)

  # Read and process the data
  df <- readODS::read_ods(ods_file, sheet_name) |>
    tidyr::fill(Studiengang, Modul, Weekday) |>
    dplyr::filter(is.na(Ex)) |>
    dplyr::select(-Ex)

  # Create events dataframe
  events <- df |>
    dplyr::mutate(
      Kalenderwoche = stringr::str_pad(Kalenderwoche, 2, pad = "0"),
      isoweekdate = glue::glue("{year}-W{Kalenderwoche}-{Weekday}"),
      date = ISOweek::ISOweek2date(isoweekdate),
      date_from = as.POSIXct(paste(date, as.character(Von)), tz = "CET"),
      date_to = as.POSIXct(paste(date, as.character(Bis)), tz = "CET"),
      summary = glue::glue("{Studiengang}: {Modul} ({Topic})")
    )

  # Create iCalendar from dataframe
  # Convert to UTC for better calendar app compatibility
  cal <- events |>
    dplyr::mutate(
      UID = paste0(Studiengang, "_", Modul, "_W", Semesterwoche, "@zhaw-semesterplanung"),
      DTSTART = lubridate::with_tz(date_from, "UTC"),
      DTEND = lubridate::with_tz(date_to, "UTC"),
      DTSTAMP = lubridate::with_tz(Sys.time(), "UTC")
    ) |>
    dplyr::select(
      UID,
      DTSTART,
      DTEND,
      DTSTAMP,
      SUMMARY = summary
    ) |>
    calendar::ical()

  # Create output directory if it doesn't exist
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }

  # Write to .ics file
  output_file <- file.path(output_dir, paste0(tolower(sheet_name), ".ics"))
  calendar::ic_write(cal, output_file)

  # Post-process to fix formatting issues
  fix_ics_formatting(output_file)

  return(output_file)
}


#' Fix iCalendar formatting to be RFC-compliant
#'
#' @param ics_file Path to the .ics file to fix
#'
#' @return Invisibly returns the path to the fixed file
fix_ics_formatting <- function(ics_file) {
  ics_lines <- readLines(ics_file)

  # Fix DTSTAMP format and add Z suffix to UTC times
  ics_lines <- ics_lines |>
    # Fix DTSTAMP: convert "2025-11-18 14:58:49" to "20251118T145849Z"
    stringr::str_replace("^DTSTAMP:(.{4})-(.{2})-(.{2}) (.{2}):(.{2}):(.{2})",
                        "DTSTAMP:\\1\\2\\3T\\4\\5\\6Z") |>
    # Add Z suffix to DTSTART and DTEND for UTC times
    stringr::str_replace("^(DTSTART:[0-9]{8}T[0-9]{6})$", "\\1Z") |>
    stringr::str_replace("^(DTEND:[0-9]{8}T[0-9]{6})$", "\\1Z")

  writeLines(ics_lines, ics_file)

  invisible(ics_file)
}
