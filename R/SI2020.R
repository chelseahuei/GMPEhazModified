#' GMPE function for Si et al.(2020)
#'
#' \code{SI2020} returns the ground-motion prediction with it sigma of Si et al.(2020) GMPE.
#'
#'Si, H., Midorikawa, S., & Kishida, T. (2022). Development of NGA-Sub ground-motion prediction equation of 5%-damped 
#'pseudo-spectral acceleration based on database of subduction earthquakes in Japan. Earthquake Spectra, 38(4), 2682-2706.
#'\url{http://dx.doi.org/10.1193/051712EQS188MR}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Vs30 Vs30(m/s).
#' @param z25 .
#' @param depth (km).
#' @param mohodepth (km).
#'
#' @return A list will be return, including mag, Ftype, rRup, vs30, z25, lnSa, sigma, phi, tau,
#'                                          specT, period2, iflag, depth, mohodepth.
#'
#' @examples
#' SI2020(6, 20, 0, 0, 760, 1, 10, 7, 1)
#' SI2020(7, 10, 0, 0, 760, 1, 10, 7, 1)
#'
#' @export
SI2020 <- function(Mag, Rrup, Prd, ftype=0, Vs30, z25, depth, Dmoho) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #SI2020 ( mag, Ftype, rRup, vs30, z25, lnSa, sigma, phi, tau,
  #                                      specT, period2, iflag, depth, mohodepth )
  retvals <- .Fortran("S35_SMK2020", mag=as.single(Mag), Ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), z25=as.single(z25),
                      lnSa=as.single(0.0), sigma=as.single(0.1), phi=as.single(0.0), tau=as.single(0.0), 
                      specT=as.single(Prd), period2=as.single(0), iflag=as.integer(1),
                      depth=as.single(depth), mohodepth=as.single(Dmoho))
  names(retvals) <- c("mag", "Ftype", "rRup", "vs30", "z25", "lnSa", "sigma", "phi", "tau",
                      "specT", "period2", "iflag", "depth", "Dmoho")
  return(retvals)
}
