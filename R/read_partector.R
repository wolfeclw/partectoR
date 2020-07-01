

#' Title
#'
#' @param path path of Partector output file.
#' @param tz 	a character string that specifies which time zone to parse the
#' datetime. Default = 'America/New_York.'
#' @param metadata include Partector output data in addition to LDSA, particle
#' number, and mass. Default = FALSE.
#'
#' @return a tibble.
#' @export
#'
#' @examples
#' #'
#' \dontrun{
#'
#' read_partector(path, tz = "America/New_York", metadata = FALSE)
#' }
read_partector <- function(path, tz = "America/New_York", metadata = FALSE) {
  d_raw <- readr::read_tsv(path, skip = 17, col_names = TRUE, col_types = readr::cols())

  dt <- readr::read_lines(path)[8] %>%
    stringr::str_replace_all(., "[[A-Za-z]:]", "") %>%
    stringr::str_trim() %>%
    lubridate::dmy_hms(tz = tz) %>%
    tibble::enframe(name = NULL, value = "begin_dt")

  d_partector <- cbind(dt, d_raw)

  d_partector <- d_partector %>%
    dplyr::mutate(
      t_minus = time - 1,
      Date_Time = begin_dt + t_minus,
      Date = lubridate::date(Date_Time),
      Time = hms::as_hms(Date_Time)
    ) %>%
    dplyr::select(-c(time, t_minus, begin_dt)) %>%
    dplyr::select(Date_Time, Date, Time, everything())

  if (metadata == FALSE) {
    d_partector %>% dplyr::select(-c(A1:DV, P:Ipump))
  } else {
    d_partector
  }
}
