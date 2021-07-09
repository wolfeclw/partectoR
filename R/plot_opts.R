
plot_opts <- function(crosshairs = TRUE, shared = TRUE, yvar = NULL,
                             tltp_fmt = NULL, tltp_table = NULL, ymax = NULL) {

  h_opts <- highcharter::highchart() %>%
    highcharter::hc_tooltip(crosshairs = crosshairs,
                            #headerFormat = "",
                            useHTML = TRUE,
                            sort = TRUE,
                            pointFormat = tltp_fmt,
                            table = tltp_table) %>%
    highcharter::hc_xAxis(type = 'datetime',
                          title = list(text = 'TIME',
                                       offset = 35,
                                       style = list(fontSize = '16px',
                                                    color = 'black')),
                          labels = list(y = 25, x = -16)) %>%
    highcharter::hc_yAxis(title = list(text = stringr::str_to_upper({{yvar}}),
                                       style = list(fontSize = '16px',
                                                    color = 'black')))
  if(is.null(ymax)) {
    h_opts
  } else {
    h_opts %>%
      highcharter::hc_yAxis(max = ymax)}
}
