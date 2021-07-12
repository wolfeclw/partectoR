
#' Plot Partector Data
#'
#' `plot_partector` creates an interactive Highchart to visualize Partector data.
#' @seealso <https://jkunst.com/highcharter/>
#'
#' @param d a dataframe created by `read_partector()`
#' @param yvar character; name of Partector measurement to plot. Default = 'number'.
#' @param type character; type of plot ('line', 'scatter', 'column'). Default = 'line'.
#' @param ymax numeric; the maximum value of the y-axis. Default = NULL.
#' @param group_var character; name of grouping variable in the input data frame.
#' If specified, different series will be plotted for each group. Designating a
#' grouping variable can be useful if the input data frame includes data from multiple
#' units (i.e colocation test).
#' @param crosshairs logical; should a vertical crosshair be drawn when the cursor
#' is placed on the plot area?
#' @param plot_alpha  numeric; controls opacity of plot lines. Default = 1.
#'
#' @return a *highchart*
#' @export
#'
#' @examples
#' \dontrun{
#'
#' plot_partector <- function(d, yvar = 'number', type = 'line', crosshairs = TRUE,
#'                            ymax = NULL, group_var = NULL, plot_alpha = 1)
#' }
#'
#'
plot_partector <- function(d, yvar = 'number', type = 'line', crosshairs = TRUE,
                           ymax = NULL, group_var = NULL, plot_alpha = 1) {

  if (!{{yvar}} %in% names(d)) {
    stop(paste0("'", {{yvar}}, "'", ' is not a column in the input data frame.'))
  }

  if (!'Date_Time' %in% names(d)) {
    stop("Column 'Date_Time' not found. Did you use `read_partector()` to create the input data frame?",
         call. = FALSE)
  }

  if (!toupper(type) %in% c('LINE', 'SCATTER', 'COLUMN')) {
    stop("Plot type must be one of c('line', 'scatter', 'column').")
  }

  d <- d %>%
    dplyr::arrange('Date_Time') %>%
    dplyr::mutate(
      dt_utc = lubridate::ymd_hms(Date_Time, tz = "UTC"),
      char_time = as.character(Time),
      c_yvar = d[[yvar]])

  if (type == 'scatter') {
    x <- c("Time", ifelse({{yvar}} %in% c('LDSA', 'T', 'RH'), {{yvar}}, stringr::str_to_title({{yvar}})))
  } else {
    x <- ifelse({{yvar}} %in% c('LDSA', 'T', 'RH'), {{yvar}}, stringr::str_to_title({{yvar}}))
  }

  if (type == 'scatter') {
    y <- sprintf("{point.%s}", c("char_time", "c_yvar"))
  } else {
    y <- sprintf("{point.%s}", "c_yvar")
  }

  if (!is.null(group_var) & type == 'scatter') {
    tltip <- highcharter::tooltip_table(x, y)
  } else {
    tltip <- NULL
  }

  if (!is.null(group_var)) {
    pal_len <- length(unique(d[[group_var]]))
  } else {
    pal_len <- 1
  }

  tltp_table_lgl <- ifelse(!is.null(group_var) & type == 'scatter', TRUE, FALSE)


  pal <- viridis::plasma(pal_len)
  pal <- highcharter::hex_to_rgba(pal, alpha = plot_alpha)

  h_base <- plot_opts(yvar = yvar, ymax = ymax, crosshairs = crosshairs,
                      tltp_fmt = tltip, tltp_table = tltp_table_lgl)

  if (is.null(group_var)) {

    h_base %>%
      highcharter::hc_add_series(d, highcharter::hcaes(highcharter::datetime_to_timestamp(dt_utc), .data[[yvar]]),
                                 type = {{type}},
                                 name = 'Partector') %>%
      highcharter::hc_colors(colors = pal)

  } else {
    h_base %>%
      highcharter::hc_add_series(d, highcharter::hcaes(highcharter::datetime_to_timestamp(dt_utc), .data[[yvar]],
                                                       group = .data[[group_var]]),
                                 type = {{type}}) %>%
      highcharter::hc_colors(colors = pal)  %>%
      highcharter::hc_tooltip(shared = TRUE, split = FALSE)
  }

}

