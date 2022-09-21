#' Function phasor can calculate a phasor
#'
#' @param mod is a real number representing the module of a complex number.
#' @param degree is a degree number representing the argument of a complex number.
#'
#' @return a complex number
#' @export
#'
#' @examples phasor(1,30)
phasor <- function (mod, degree) {
    arg_rad <- pi/180*degree
    return(complex(real=mod*cos(arg_rad), imaginary=mod*sin(arg_rad)))
}

#' Function rephasor can develop a phasor
#'
#' @param complex is a complex number to be calculated.
#'
#' @return a atomic numeric vector representing a phasor
#' @export
#'
#' @examples rephasor(sqrt(3)/2+0.5i)
rephasor <- function (complex) {
    degree <- Arg(complex)*180/pi
    module <- Mod(complex)
    return(c(mod = module, degree = degree))
}
