#' GMPE function for Abrahamson and Gülerce et al.(2020)
#'
#' \code{PSHAB2019} returns the ground-motion prediction with it sigma of Abrahamson and Gülerce et al.(2020) GMPE.
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
#' @param epiflag .
#'
#' @return A list will be return, including mag, Ftype, rRup, vs30, z25, lnY, sigma, phi, tau,
#'                                          specT, period2, iflag, depth, disthypo, iRegion, mbInter,
#'                                          mbSlab, pnwflag.
#'
#' @examples
#' PSHAB2019(6, 20, 0, 0, 760, 1, 10, 7, 1)
#' PSHAB2019(7, 10, 0, 0, 760, 1, 10, 7, 1)
#'
#' @export
PSHAB2019 <- function(Mag, Rrup, Prd, ftype=0, Vs30, z25, depth, Rhypo, region=7, pnwflag=1) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #PSHAB2019 ( mag, evType, rRup, vs30, z25, ztor, region, lnY, sigma, phi, tau,
  #             rockPGA, specT, period2, iflag, ACadjfac, epiflag )
  retvals <- .Fortran("S35_PSHAB2019", mag=as.single(Mag), Ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), z25=as.single(z25),
                      lnY=as.single(0.0), sigma=as.single(0.1), phi=as.single(0.0), tau=as.single(0.0), 
                      specT=as.single(Prd), period2=as.single(0), iflag=as.integer(1),
                      depth=as.single(depth), disthypo=as.single(Rhypo), iRegion=as.integer(region), mbInter=as.single(0), mbSlab=as.single(0),
                      pnwflag=as.integer(pnwflag))
  names(retvals) <- c("mag", "Ftype", "rRup", "vs30", "z25", "lnY", "sigma", "phi", "tau",
                      "specT", "period2", "iflag", "depth", "disthypo", "iRegion", "mbInter", 
                      "mbSlab", "pnwflag")
  return(retvals)
}
