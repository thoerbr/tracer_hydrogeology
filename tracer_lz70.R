#' 2D tracer equation according to Lenda & Zuber 1970.
#'
#' Normalized tracer modelling equation for combined pumping tracer experiments. Taken from Leibundgut et al. (2009), p. 131.
#'
#' @param TMR total recovered Mass in microgram
#' @param Q flow rate in l/time unit (time unit must be the same in Q and t)
#' @param t time since start of tracer test
#' @param t0 fitting parameter t0
#' @param PD fitting parameter PD
tracer_lz70 <- function(TMR, Q, t, t0, PD){
  conc     <- ( TMR / (Q * t0) ) * 1 / (4 * pi * PD * ( t / t0 )^3 )^0.5 * exp( -( ( 1 - t / t0 )^2) / ( 4 * PD * ( t / t0 ) ) ) 
  return(conc)
}