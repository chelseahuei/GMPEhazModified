#' GMPE function for Abrahamson and Gülerce et al.(2020)
#'
#' \code{AG2020} returns the ground-motion prediction with it sigma of Abrahamson and Gülerce et al.(2020) GMPE.
#'
#'Norman Abrahamson and Zeynep Gülerce(2020) Regionalized Ground-Motion Models for
#'Subduction Earthquakes Based on the NGA-SUB Database, PEER Report No. 2020/25.
#'\url{http://dx.doi.org/10.1193/051712EQS188MR}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Vs30 Vs30(m/s).
#' @param z25 .
#' @param ztor (km).
#' @param region regional flag, region=7 for Taiwan, region=8 for Global.
#'
#' @return A list will be return, including mag, evType, rRup, vs30, z25, ztor, region, mu, sigma, phi, tau,
#'                                          rockPGA, specT, period2, iFlag, ACadjfac, epiflag.
#'
#' @examples
#' AG2020(6, 20, 0, 0, 760, 1, 10, 7)
#' AG2020(7, 10, 0, 0, 760, 1, 10, 7)
#'
#' @export
AG2020 <- function(Mag, Rrup, Prd, ftype=0, Vs30, z25, ztor, region=7) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #AG2020 ( mag, evType, rRup, vs30, z25, ztor, region, mu, sigma, phi, tau,
  #             rockPGA, specT, period2, iFlag, ACadjfac, epiflag )
  retvals <- .Fortran("AG2020", mag=as.single(Mag), evType=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), z25=as.single(z25), ztor=as.single(ztor), region=as.single(region),
                      mu=as.single(0.0), sigma=as.single(0.1), phi=as.single(0.0), tau=as.single(0.0), 
                      rockPGA=as.single(0), specT=as.single(Prd), period2=as.single(0), iflag=as.integer(1),
                      ACadjfac=as.single(0), epiflag=as.single(0))
  names(retvals) <- c("mag", "evType", "rRup", "vs30", "z25", "ztor", "region", "mu", "sigma", "phi", "tau",
                      "rockPGA", "specT", "period2", "iFlag", "ACadjfac", "epiflag")
  return(retvals)
}
