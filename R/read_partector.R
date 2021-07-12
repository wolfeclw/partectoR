

#' Import naneos Partector data
#'
#' @param path path of Partector output file.
#' @param tz 	character; specifies which time zone to parse the
#' datetime. Default = 'America/New_York.'
#' @param metadata logical; include Partector output data in addition to LDSA, particle
#' number, and mass? Default = FALSE.
#' @param participant_id  user defined string to denote a personal identifier.
#' This is useful if the Partector is deployed during personal sampling.  If specified,
#' a new column is created ('ID'). Default is NULL.
#' @param sample_col character; user defined character string specifying the name of the
#' column to denote sample ID. Default is NULL.
#' @param sample_id user defined string to denote sample ID. If assigned, a
#' value must also be supplied to `sample_col`. Default is NULL.
#'
#' @importFrom stats time
#'
#' @return a tibble.
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#' read_partector(path, tz = "America/New_York", metadata = FALSE)
#' }
read_partector <- function(path, tz = "America/New_York", metadata = FALSE,
                           participant_id = NULL, sample_col = NULL,
                           sample_id = NULL) {

  head_rws <- readr::read_lines(path)[1:20]
  skp_rw <- which(stringr::str_detect(head_rws, '[[:digit:]:alpha:]') == FALSE)

  time_rw <- which(stringr::str_detect(head_rws, 'Start:') == TRUE)

  d_raw <- readr::read_tsv(path, skip = skp_rw, col_names = TRUE, col_types = readr::cols())

  dt <- head_rws[time_rw]
  dt <- stringr::str_split(dt, ' ') %>%
    unlist()
  dt <- dt[-1]
  dt <- stringr::str_c(dt, collapse = ' ') %>%
    lubridate::dmy_hms(tz = tz) %>%
    tibble::enframe(name = NULL, value = "begin_dt")

  d_partector <- dplyr::bind_cols(dt, d_raw)

  d_partector <- d_partector %>%
    dplyr::mutate(
      t_minus = time - 1,
      Date_Time = begin_dt + t_minus,
      Date = lubridate::date(Date_Time),
      Time = hms::as_hms(Date_Time),
      Time = chron::as.times(Time)
    ) %>%
    dplyr::select(-c(time, t_minus, begin_dt)) %>%
    dplyr::select(Date_Time, Date, Time, dplyr::everything())

  if (!is.null(sample_col) & !is.character(sample_col)) {
    stop("`sample_col` must be a character string.",
         call. = FALSE
    )
  }

  if (sum(is.null(sample_col), is.null(sample_id)) == 1) {
    stop("Both `sample_col` and `sample_id` must be assigned a value, not one or the other.",
         call. = FALSE
    )
  } else if (sum(is.null(sample_col), is.null(sample_id)) == 0) {
    d_partector <- dplyr::mutate(d_partector, {{ sample_col }} := sample_id) %>%
      dplyr::relocate({{ sample_col }})
  } else {
    d_partector <- d_partector
  }

  if (!is.null(participant_id)) {
    d_partector <- dplyr::mutate(d_partector, ID = participant_id) %>%
      dplyr::relocate(ID)
  }

  if (metadata == FALSE & 'PWMpump' %in% names(d_partector)) {
    d_partector %>% dplyr::select(-c(A1:DV, P:PWMpump))
  } else if (metadata == FALSE) {
    d_partector %>% dplyr::select(-c(A1:DV, P:Ipump))
  } else {
    d_partector
  }
}
