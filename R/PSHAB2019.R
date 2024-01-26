#' GMPE function for Parker, Grace A., et al.(2019)
#'
#' \code{PSHAB2019} returns the ground-motion prediction with it sigma of Parker, Grace A., et al.(2019) GMPE.
#'
#'Parker, G. A., Stewart, J. P., Boore, D. M., Atkinson, G. M., & Hassani, B. (2022). NGA-subduction global ground motion models 
#'with regional adjustment factors. Earthquake Spectra, 38(1), 456-493.
#'\url{http://dx.doi.org/10.1193/051712EQS188MR}
#'
#' @param Mag Earthquake momnet magnitude, Numeric.
#' @param Rrup Rupture distance(km), Numeric.
#' @param Prd Period of spectral acceleration.
#' @param ftype fytpe=0 for interface, ftype=1 for intraslab.
#' @param Vs30 Vs30(m/s).
#' @param z25 Depth to a shear wave horizon of 2500 m/s(m).
#' @param depth Depth to Hypocenter(km).
#' @param ztor Depth to top of rupture(km).
#' @param region regional flag, region=9 for Taiwan, region=0 for Global.
#' @param mbInter Magnitude Break Point, 7.1 for Taiwan.
#' @param mbSlab Magnitude Break Point, 7.7 for Taiwan.
#'
#' @return A list will be return, including mag, Ftype, rRup, vs30, z25, lnY, sigma, phi, tau,
#'                                          specT, period2, iflag, depth, Rhypo, region, mbInter,
#'                                          mbSlab, pnwflag.
#'
#' @examples
#' PSHAB2019(6, 20, 0, 0, 760, 1, 10, 7, 1)
#' PSHAB2019(7, 10, 0, 0, 760, 1, 10, 7, 1)
#'
#' @export
PSHAB2019 <- function(Mag, Rrup, Prd, ftype=0, Vs30, z25, depth, mbInter, mbSlab, region=7, pnwflag=1) {
  if (Prd != 0 & (Prd < 0.01 | Prd > 10)) {
    stop("Period out of range! \n\n")
  }
  #PSHAB2019 ( mag, evType, rRup, vs30, z25, ztor, region, lnY, sigma, phi, tau,
  #             rockPGA, specT, period2, iflag, ACadjfac, epiflag )
  retvals <- .Fortran("S35_PSHAB2019", mag=as.single(Mag), Ftype=as.single(ftype), rRup=as.single(Rrup),
                      vs30=as.single(Vs30), z25=as.single(z25), lnY=as.single(0.0), sigma=as.single(0.1), 
                      phi=as.single(0.0), tau=as.single(0.0), specT=as.single(Prd), period2=as.single(0), 
                      iflag=as.integer(1), depth=as.single(depth), Rhypo=as.single(0), region=as.integer(region), 
                      mbInter=as.single(mbInter), mbSlab=as.single(mbSlab), pnwbflag=as.integer(pnwflag))
  names(retvals) <- c("mag", "Ftype", "rRup", "vs30", "z25", "lnY", "sigma", "phi", "tau",
                      "specT", "period2", "iflag", "depth", "Rhypo", "region", "mbInter", 
                      "mbSlab", "pnwflag")
  return(retvals)
}
