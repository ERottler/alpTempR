#' Water vapor pressure to absolute humidity.
#'
#' Calcuation of absolute air humidty based on measurements of water vapor pressure
#' and ambient temperature using formula derived from Ideal gas law.
#' Absolute humidity ahum = vapor_pres / (Rw * tmean) where vapor_pres
#' is the vapor pressure, tmean the ambient temperature and Rw is the specific gas
#' constant and for water equals 461.5 J/(kg*K).
#'
#' @param vapor_pres  Vapor pressure [hPa]
#' @param temp Ambient temperature [°C]
#' @param Rw Specific gas constant [J/(kg*K)]
#' @return Absolute humidity of air [g/m³].
#' @examples
#'
#' pres2ahum(vapor_pres = 5.5, temp = 20)
#' @export
pres2ahum <- function(vapor_pres, temp, Rw=461.5){

  temp <- temp + 273.15 #to get temperature in Kelvin
  vapor_pres <- vapor_pres * 100 #to get water vapor pressure in Pa

  ahum <- vapor_pres / (Rw * temp)

  ahum <- ahum *1000 #to get absolute humidity in g/cm³

  return(ahum)

}

