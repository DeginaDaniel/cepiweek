#' Generate Continuous Epidemiological Week Index for a Date Column
#'
#' This function adds a continuous epidemiological week index to a data frame
#' by assigning each date to a sequential week number starting from a reference
#' date.
#'
#' The generated weeks are continuous and **do not reset at calendar year boundaries**. 
#' They are **not ISO 8601 or MMWR weeks**. This function is intended for 
#' epidemiological modeling, time-series analysis, and nowcasting applications.
#'
#' You can specify a `start` date for counting weeks; if none is provided, the minimum
#' date in the column is used. The function also allows specifying the `format` of the
#' dates to handle different conventions (day-month-year vs month-day-year).
#'
#' @param data A data frame containing the date column.
#' @param col_date Name of the date column (string).
#' @param start Optional start date for counting weeks (Date or string). 
#'   Defaults to the minimum date in the column `col_date`.
#' @param format Optional date format flag. Use `"mdy"` if all dates are in 
#'   month-day-year format (common in US). Defaults to `"dmy"`/`"ymd"` which handles 
#'   day-month-year or year-month-day formats.
#'
#' @return A data frame with an added `cepiweek` column containing continuous week indices.
#'
#' @examples
#' # Standard dmy/ymd dates
#' k <- data.frame(
#'   num = c(1, 2, 3),
#'   date = c("15-01-2024", "12/02/2025", "2026-08-01")
#' )
#' cepiweek(k, col_date = "date")
#'
#' # American format mm-dd-yyyy
#' k2 <- data.frame(
#'   num = c(1, 2, 3),
#'   date = c("01/15/2024", "02-12-2025", "08/01/2026")
#' )
#' cepiweek(k2, col_date = "date", format = "mdy")
#'
#' @details
#' - The function automatically replaces `/` with `-` for consistency.
#' - If any dates cannot be converted, the function stops with an error.
#' - A warning is issued if the `start` date is after the earliest date in the column.
#' - Continuous weeks are aligned to Mondays.
#'
#' @seealso \href{https://github.com/DeginaDaniel/cepiweek}{GitHub page of the package}.
#' 
#' @export
cepiweek = function(data, col_date, start = NULL, format = "dmy") {
  # Nettoyer séparateurs
  data[[col_date]] = gsub("/", "-", data[[col_date]])
  
  # Conversion en Date selon le format choisi
  if (format == "mdy") {
    data[[col_date]] = as.Date(
      lubridate::parse_date_time(
      data[[col_date]], orders = "mdy"))
  } else {
    # Par défaut: dmy ou ymd
    data[[col_date]] = as.Date(
      lubridate::parse_date_time(
      data[[col_date]], 
      orders = c("dmy", "ymd")))
  }
  
  data[[col_date]] = as.Date(data[[col_date]])  # assure que c'est Date
  
  if (any(is.na(data[[col_date]]))) {
    stop("Non conversion de certaines dates")
  }

  # dates minimale et maximale
  date_min = if (is.null(start))
    min(data[[col_date]], na.rm = TRUE)
  else
    as.Date(start)
  date_max = max(data[[col_date]], na.rm = TRUE)
  
  # Vérification cohérence du start
  if (!is.null(start) &&
      as.Date(start) > min(data[[col_date]], na.rm = TRUE)) {
    warning("'start' est superieure a la date minimale")
  }

  # premier lundi à partir de date_min
  date_min_d1w = lubridate::floor_date(date_min, unit = "week", week_start = 1)

  # dernier lundi avant date_max
  date_max_d1w = lubridate::floor_date(date_max, unit = "week", week_start = 1)
  date_max_d7w = date_max_d1w + 6

  data$cepiweek = as.integer(
    floor(as.numeric(
      difftime(data[[col_date]],
               date_min_d1w,
               units = "days"))/7) + 1
  )

  return(data)
}
