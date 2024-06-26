#' GMPE function for Kuehn, Nicolas, et al.(2019)
#'
#' \code{KBCG2019} returns the ground-motion prediction with it sigma of Kuehn, Nicolas, et al.(2019) GMPE.
#'
#'Kuehn, N., Bozorgnia, Y., Campbell, K., & Gregor, N. (2020). Partially non-ergodic ground-motion model for 
#'subduction regions using the NGA subduction database. PEER Reports.
#'\url{http://dx.doi.org/10.1193/051712EQS188MR}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Vs30 Vs30(m/s).
#' @param z25 Depth to a shear wave horizon of 2500 m/s(m).
#' @param ztor Depth to top of rupture(km).
#' @param region regional flag, region=7 for Taiwan, region=0 for Global.
#' @param epiflag .
#' @param mbInter Magnitude Break Point, 7.1 for Taiwan.
#' @param mbSlab Magnitude Break Point, 7.7 for Taiwan.
#'
#' @return A list will be return, including mag, Ftype, rRup, vs30, z25, lnY, sigma, phi, tau,
#'                                          specT, period2, iflag, depth, disthypo, iRegion, mbInter,
#'                                          mbSlab, ztor, CasBas, Z10.
#'
#' @examples
#' KBCG2019(6, 20, 0, 0, 760, 1, 10, 7, 1)
#' KBCG2019(7, 10, 0, 0, 760, 1, 10, 7, 1)
#'
#' @export
KBCG2019 <- function(Mag, Rrup, Prd, ftype=0, Vs30, z25, mbInter, mbSlab, region=7, ztor, Z1.0, CasBas=1) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #KBCG2019 ( mag, evType, rRup, vs30, z25, ztor, region, lnY, sigma, phi, tau,
  #             rockPGA, specT, period2, iflag, ACadjfac, epiflag )
  retvals <- .Fortran("S35_KBCG2019", mag=as.single(Mag), Ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), z25=as.single(z25), lnY=as.single(0.0), sigma=as.single(0.1), phi=as.single(0.0), tau=as.single(0.0), 
                      specT=as.single(Prd), period2=as.single(0), iflag=as.integer(1),
                      depth=as.single(0), disthypo=as.single(0), iRegion=as.integer(region), mbInter=as.single(mbInter), mbSlab=as.single(mbSlab),
                      ztor=as.single(ztor), CasBas=as.integer(CasBas), Z10=as.single(Z1.0))
  names(retvals) <- c("mag", "Ftype", "rRup", "vs30", "z25", "lnY", "sigma", "phi", "tau",
                      "specT", "period2", "iflag", "depth", "disthypo", "iRegion", "mbInter", 
                      "mbSlab", "ztor", "CasBas", "Z10")
  return(retvals)
}
