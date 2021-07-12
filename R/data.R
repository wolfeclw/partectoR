#' Partector data from one unit.
#'
#' A dataset of Partector measurements recorded indoors. Elevated particle counts and
#' LDSA correspond to cooking events. Measurements were aggregated to 5-second intervals
#' using `circleclust::dt_aggregate()`.
#'
#' @format A data frame with 5344 rows and 12 variables:
#' \describe{
#'   \item{Date_Time}{Datetime of measurements}
#'   \item{Unit}{Character string indicating a specific Partector unit}
#'   \item{Date}{Date of measurements}
#'   \item{Time}{Time of measurements}
#'   \item{number}{Particle number}
#'   \item{diam}{Average particle diameter}
#'   \item{LDSA}{Lung deposited surface area}
#'   \item{surface}{Total surface area}
#'   \item{mass}{Mass of particles smaller than 0.3 micrometers}
#'   \item{T}{Internal temperature in Celcius}
#'   \item{RH}{Internal relative humidity (%)}
#'   \item{error}{current error status (0 = no error)}
#'   }
"d_partector"

#' Partector data recorded during colocation of two units.
#'
#' A dataset of Partector measurements recorded indoors. Elevated particle counts and
#' LDSA correspond to cooking events. Measurements were aggregated to 5-second intervals
#' using `circleclust::dt_aggregate()`.
#'
#' @format A data frame with 10688 rows and 12 variables:
#' \describe{
#'   \item{Date_Time}{Datetime of measurements}
#'   \item{Unit}{Character string indicating a specific Partector unit
#'   (Partector 1, Partector 2)}
#'   \item{Date}{Date of measurements}
#'   \item{Time}{Time of measurements}
#'   \item{number}{Particle number}
#'   \item{diam}{Average particle diameter}
#'   \item{LDSA}{Lung deposited surface area}
#'   \item{surface}{Total surface area}
#'   \item{mass}{Mass of particles smaller than 0.3 micrometers}
#'   \item{T}{Internal temperature in Celcius}
#'   \item{RH}{Internal relative humidity (%)}
#'   \item{error}{current error status (0 = no error)}
#'   }
"d_colocation"
