c ----------------------------------------------------------------------

      subroutine S35_SMK2020 ( mag, Ftype, rRup, vs30, z25, lnY, sigma, phi, tau,
     2                     specT, period2, iflag, depth, mohodepth )

      implicit none
      integer MAXPER
      parameter (MAXPER=23)
      integer count1, count2, iflag, nPer, i1, i

      real Period(MAXPER), e(MAXPER), a1(MAXPER), d1(MAXPER),
     1     a2(MAXPER), h(MAXPER), cd(MAXPER), Dd(MAXPER), C(MAXPER),
     2     Vc(MAXPER), f4(MAXPER), f5(MAXPER), sigphi(MAXPER), sigtau(MAXPER)
      real eT, a1T, d1T, a2T, hT, cdT, DdT, CT, VcT, f4T, f5T, sigPhiT, sigTauT
      real Vref, f1, f3
      real sigma, lnY, pgaRock, vs30, rRup, mag, depth, Ftype, mohodepth
      real period2, specT, z25, phi, tau
      real kterm, ktermpga, cterm, ctermpga, cutDD, gterm, gtermpga, gdterm, gdtermpga
      real bterm, btermpga, flin, fnl, gsterm, f2, median


C     Coefficients 08/25/2021

c     data period / 0.00, -1.00, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
c    1              0.5, 0.75, 1.00, 1.5, 2.00, 3.00, 4.00, 5.00, 7.00, 10.00 /
c     data e  / -2.7267439, -1.933444, -2.7079904, -2.7080162, -2.6903921, -2.6167275, -2.525663,
c    1          -2.4914814, -2.5189582, -2.5989579, -2.7092901, -2.8172584, -3.0714905, -3.3136917,
c    2          -3.785221385, -4.16659131, -4.901364443, -5.470871899, -6.337799541, -6.921473929,
c    3          -7.319266537, -7.945565121, -8.779899021 /
c     data a1 / 0.4908892, 0.6445959, 0.50175701, 0.503327193, 0.504897379, 0.508037773, 0.511178213,
c    1          0.515888791, 0.523737608, 0.531579061, 0.539407049, 0.547215792, 0.56275672,
c    2          0.578170887, 0.615999628, 0.652553714, 0.720559114, 0.77991566, 0.868274098,
c    3          0.923481828, 0.96007551, 1.025041319, 1.137022039 /
c     data d1 / 0.2118015, 0.1805972, 0.213370189, 0.223607193, 0.233844197, 0.246864583, 0.25625647,
c    1          0.263091069, 0.259885833, 0.2515129, 0.243631882, 0.232014968, 0.217884443, 0.204814643,
c    2          0.187899494, 0.176347416, 0.171592549, 0.174051474, 0.181359962, 0.187096027,
c    3          0.193129321, 0.197171869, 0.203235691 /
c     data a2 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1348718,
c    1          0.1302177, 0.2549352, 0.5137211, 0.5672251, 0.6083083, 0.6428719, 0.6942251, 0.5671919 /
c     data h / 0.007210868, 0.005419657, 0.007202214, 0.007430087, 0.007657961, 0.007979789,
c    1         0.008247439, 0.008447657, 0.008211716, 0.007787228, 0.00738587, 0.006845832,
c    2         0.006224042, 0.005800522, 0.005048717, 0.004544581, 0.004168906, 0.004056775,
c    3         0.004123161, 0.003972515, 0.003648234, 0.002182624, 0.0 /
c     data Cd / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
c    1          0.002032605, 0.025594005, 0.031828072, 0.030465723, -0.001921725, -0.042352667,
c    2         -0.077322458, -0.100115037 /
c     data Dd / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
c    1          0.085030533, 0.097461559, 0.11418673, 0.133516203, 0.163964771, 0.196531265,
c    2          0.199814063, 0.172679121 /
c     data c / -0.6, -0.84, -0.6037, -0.5739, -0.5341, -0.458, -0.4441, -0.4872, -0.5796,
c    1         -0.6876, -0.7718, -0.8417, -0.9109, -0.9693, -1.0154, -1.05, -1.0454, -1.0392,
c    2         -1.0112, -0.9694, -0.9195, -0.8046, -0.6558 /
c     data Vc / 1500.0, 1300.0, 1500.2, 1500.36, 1502.95, 1501.42, 1494.0, 1479.12, 1442.85,
c    1          1392.61, 1356.21, 1308.47, 1252.66, 1203.91, 1147.59, 1109.95, 1072.39,
c    2          1009.49, 922.43, 844.48, 793.13, 772.68, 775.0 /
c     data f4 / -0.15, -0.1, -0.1483, -0.1471, -0.1549, -0.1963, -0.2287, -0.2492, -0.2571,
c    1          -0.2466, -0.2357, -0.2191, -0.1958, -0.1704, -0.1387, -0.1052, -0.0679,
c    2          -0.0361, -0.0136, -0.0032, -0.0003, 0.0, 0.0 /
c     data f5 / -0.00701, -0.00844, -0.00701, -0.00728, -0.00735, -0.00647, -0.00573,
c    1          -0.0056, -0.00585, -0.00614, -0.00644, -0.0067, -0.00713, -0.00744, -0.00812,
c    2          -0.00844, -0.00771, -0.00479, -0.00183, -0.00152, -0.00144, -0.00137, -0.00136 /
c     data sigphi / 0.602511929, 0.554426633, 0.587138878, 0.586636948, 0.591884361, 0.616636506,
c    1              0.662828733, 0.696266316, 0.712217671, 0.703333328, 0.683114937, 0.664253842,
c    2              0.635447131, 0.624947234, 0.63600237, 0.654737997, 0.692422766, 0.714571508,
c    3              0.702827819, 0.685656575, 0.653101909, 0.606218533, 0.53921879 /
c     data sigtau / 0.678907779, 0.458422498, 0.651303557, 0.654062863, 0.672310989, 0.738544673,
c    1              0.788641603, 0.76633797, 0.651840928, 0.610739167, 0.567120468, 0.537991628,
c    2              0.506166155, 0.486000441, 0.481555738, 0.484183713, 0.4689282, 0.45925422,
c    3              0.47022716, 0.500884413, 0.510577956, 0.516643095, 0.493780604 /


       data period / 0.000, -1.000, 0.010, 0.020, 0.030, 0.050, 0.075, 0.100, 0.150,
      1              0.200, 0.250, 0.300, 0.400, 0.500, 0.750, 1.000, 1.500, 2.000,
      2              3.000, 4.000, 5.000, 7.000, 10.000 /
       data e / -2.7319602, -1.9377316, -2.6899752, -2.6889864, -2.668915, -2.5900945,
      1         -2.5040933, -2.4680936, -2.5026276, -2.5946368, -2.7083263, -2.8238059,
      2         -3.0871773, -3.3423891, -3.844262719, -4.26099559, -5.052034618,
      3         -5.708858434, -6.731732648, -7.432432738, -7.871883886,
      4         -8.274318056, -8.49984373 /
       data a1 / 0.4917519, 0.6449087, 0.498552103, 0.500247032, 0.501941964, 0.505331864,
      1          0.509569357, 0.513806919, 0.52228081, 0.530749161, 0.539206716, 0.547648573,
      2          0.564469723, 0.581187948, 0.622416389, 0.662622688, 0.738788056, 0.807385789,
      3          0.915403426, 0.98511283, 1.025265403, 1.056000115, 1.071204328 /
       data d1 / 0.2114314, 0.1818077, 0.216457821, 0.223950035, 0.23144225, 0.241116032,
      1          0.250082418, 0.255436924, 0.255226857, 0.25035302, 0.245524039, 0.235806898,
      2          0.221382562, 0.208054723, 0.191214688, 0.180722026, 0.177385417, 0.180865794,
      3          0.179712346, 0.176580661, 0.174348944, 0.167460034, 0.157126668  /
       data a2 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      1          0.1118103, 0.1339921, 0.2557821, 0.5168177, 0.5906224, 0.6243605, 0.6601545,
      2          0.7026209, 0.617846 /
       data h / 0.007203427, 0.005427202, 0.007287751, 0.007438551, 0.007589351, 0.007853247,
      1         0.008122348, 0.008257189, 0.008109089, 0.007767407, 0.007413394, 0.006935728,
      2         0.006314757, 0.005913315, 0.00514768, 0.004683866, 0.00441547, 0.004410852,
      3         0.004658857, 0.004796962, 0.004725937, 0.003719351, 0.002209471 /
       data Cd / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      1         -0.0008447, 0.01982, 0.02578, 0.02093, -0.01318, -0.05295, -0.08429, -0.10673 /
       data Dd / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
      1          0.0, 0.0849857, 0.10041, 0.1174, 0.13777, 0.16772, 0.20112, 0.20962, 0.19631 /
       data c / -0.84, -0.6, -0.604, -0.574, -0.534, -0.458, -0.444, -0.487, -0.58,
      1         -0.688, -0.772, -0.842, -0.911, -0.969, -1.02, -1.05, -1.05, -1.04,
      2         -1.01, -0.969, -0.92, -0.805, -0.656 /
       data Vc / 1300.0, 1500.0, 1500.0, 1500.0, 1500.0, 1500.0, 1490.0, 1480.0, 1440.0,
      1          1390.0, 1360.0, 1310.0, 1250.0, 1200.0, 1150.0, 1110.0, 1070.0, 1010.0,
      2           922.0, 844.0, 793.0, 773.0, 775.0 /
       data f4 / -0.1, -0.15, -0.148, -0.147, -0.155, -0.196, -0.229, -0.249, -0.257,
      1          -0.247, -0.236, -0.219, -0.196, -0.17, -0.139, -0.105, -0.0679, -0.0361,
      2          -0.0136, -0.00321, -0.000255, -0.0000455, 0.0 /
       data f5 / -0.00844, -0.00701, -0.00701, -0.00728, -0.00735, -0.00647, -0.00573,
      1          -0.0056, -0.00585, -0.00614, -0.00644, -0.0067, -0.00713, -0.00744,
      2          -0.00812, -0.00844, -0.00771, -0.00479, -0.00183, -0.00152, -0.00144,
      3          -0.00137, -0.00136 /
c     Updated 11/16/2020
       data sigphi / 0.698487036, 0.698567591, 0.707423348, 0.745178656, 0.801901481,
      1              0.824902609, 0.811419926, 0.79259586, 0.765145342, 0.742104729,
      2              0.711555585, 0.699247374, 0.707484704, 0.724389736, 0.763758646,
      3              0.795142376, 0.799800905, 0.796173644, 0.774495359, 0.741211134,
      4              0.699833142, 0.719869168, 0.638371395 /
       data sigtau / 0.468873526, 0.469119244, 0.482129367, 0.528220944, 0.56902824,
      1              0.560503577, 0.487844172, 0.45530486, 0.433521089, 0.400213051,
      2              0.363497143, 0.337707584, 0.297657575, 0.295706859, 0.307184299,
      3              0.267014442, 0.302249866, 0.342410131, 0.354900285, 0.356886341,
      4              0.333790792, 0.484805644, 0.335441147 /


C     Constant parameters
      Vref = 760.0
      f1 = 0.0
      f3 = 0.1

C Find the requested spectral period and corresponding coefficients
      nPer = 23

C First check for the PGA case
      if (specT .eq. 0.0) then
         i1=1
         period2 = period(i1)
         eT = e(i1)
         a1T = a1(i1)
         d1T = d1(i1)
         a2T = a2(i1)
         hT  = h(i1)
         CdT = Cd(i1)
         DdT = Dd(i1)
         CT  = C(i1)
         VcT = Vc(i1)
         f4T = f4(i1)
         f5T = f5(i1)
         sigphiT = sigphi(i1)
         sigtauT = sigtau(i1)

         goto 1011

C First check for the PGV case
      elseif (specT .eq. -1.0) then
         i1=2
         eT = e(i1)
         a1T = a1(i1)
         d1T = d1(i1)
         a2T = a2(i1)
         hT  = h(i1)
         CdT = Cd(i1)
         DdT = Dd(i1)
         CT  = C(i1)
         VcT = Vc(i1)
         f4T = f4(i1)
         f5T = f5(i1)
         sigphiT = sigphi(i1)
         sigtauT = sigtau(i1)

         goto 1011

      endif

C   For other periods, loop over the spectral period range of the attenuation relationship.
      do i=3,nper-1
         if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
            count1 = i
            count2 = i+1
            goto 1020
         endif
      enddo
      write (*,'( i5,2f12.6)') nper, specT, period(nper)

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'SMK (2020) Horizontal'
      write (*,*) 'attenuation model is not defined for a '
      write (*,*) ' spectral period of: '
      write (*,'(a10,f10.5)') ' Period = ',specT
      write (*,*) 'This spectral period is outside the defined'
      write (*,*) 'period range in the code or beyond the range'
      write (*,*) 'of spectral periods for interpolation.'
      write (*,*) 'Please check the input file.'
      write (*,*)
      stop 99

C Interpolate the coefficients for the requested spectral period.
 1020 call S24_interp (period(count1),period(count2),sigphi(count1),sigphi(count2),
     +                   specT,sigphiT,iflag)
      call S24_interp (period(count1),period(count2),sigtau(count1),sigtau(count2),
     +                   specT,sigtauT,iflag)
      call S24_interp (period(count1),period(count2),e(count1),e(count2),
     +                   specT,eT,iflag)
      call S24_interp (period(count1),period(count2),a1(count1),a1(count2),
     +                   specT,a1T,iflag)
      call S24_interp (period(count1),period(count2),d1(count1),d1(count2),
     +                   specT,d1T,iflag)
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,a2T,iflag)
      call S24_interp (period(count1),period(count2),h(count1),h(count2),
     +                   specT,hT,iflag)
      call S24_interp (period(count1),period(count2),Cd(count1),Cd(count2),
     +                   specT,CdT,iflag)
      call S24_interp (period(count1),period(count2),Dd(count1),Dd(count2),
     +                   specT,DdT,iflag)
      call S24_interp (period(count1),period(count2),C(count1),C(count2),
     +                   specT,CT,iflag)
      call S24_interp (period(count1),period(count2),Vc(count1),Vc(count2),
     +                   specT,VcT,iflag)
      call S24_interp (period(count1),period(count2),f4(count1),f4(count2),
     +                   specT,f4T,iflag)
      call S24_interp (period(count1),period(count2),f5(count1),f5(count2),
     +                   specT,f5T,iflag)

 1011 period2 = specT

C     Compute the base model including PGARock

C     Path Terms
C     k Function Term
      if (specT .eq. -1.0) then
         kterm = 0.002
      elseif (specT .lt. 0.3) then
         kterm = 0.003
      elseif (specT .le. 0.6) then
         kterm = 0.00126 - 0.00332*log10(specT)
      else
         kterm = 0.002
      endif
      ktermpga = 0.003

c     c function term
      if (specT .eq. -1.0) then
         cterm = 0.0028*10**(0.5*min(mag,8.3))
      elseif (specT .lt. 0.3) then
         cterm = 0.0055*10**(0.5*min(mag,8.3))
      elseif (specT .le. 0.6) then
         cterm = (0.000810-0.00897*log10(specT))*10**(0.5*min(mag,8.3))
      else
         cterm = 0.0028*10**(0.5*min(mag,8.3))
      endif
      ctermpga = 0.0055*10**(0.5*min(mag,8.3))

C     Depth Cut off function
      cutDD = 1.7*depth

C     g Function term
      if (depth .gt. mohodepth .and. Rrup .ge. CutDD) then
         gterm = 0.6*log10(CutDD+Cterm) - 1.6*log10(Rrup+Cterm)
         gtermpga = 0.6*log10(CutDD+Ctermpga) - 1.6*log10(Rrup+Ctermpga)
      else
         gterm = -1.0*log10(Rrup + Cterm)
         gtermpga = -1.0*log10(Rrup + Ctermpga)
      endif

C     Basin terms.
c     gd function term
      gdterm = CdT + DdT*z25
      gdtermpga = Cd(1) + Dd(1)*z25

C     Source Terms
c     b function term
      if (specT .lt. 2.0 ) then
         if (mag .lt. 8.3) then
            bterm = a1T*mag + d1T*ftype + hT*depth + eT
            btermpga = a1(1)*mag + d1(1)*ftype + h(1)*depth + e(1)
         else
            bterm = a1T*mag + (a2T-a1T)*(mag-8.3) + d1T*ftype + hT*depth + eT
            btermpga = a1(1)*mag + (a2(1)-a1(1))*(mag-8.3) + d1(1)*ftype + h(1)*depth + e(1)
         endif
      elseif (specT .ge. 2.0) then
         if (mag .lt. 7.5) then
            bterm = a1T*mag + d1T*ftype + hT*depth + eT
            btermpga = a1(1)*mag + d1(1)*ftype + h(1)*depth + e(1)
         else
            bterm = a1T*mag + (a2T-a1T)*(mag-7.5) + d1T*ftype + hT*depth + eT
            btermpga = a1(1)*mag + (a2(1)-a1(1))*(mag-7.5) + d1(1)*ftype + h(1)*depth + e(1)
         endif
      endif

C     Reference Rock PGA

      pgarock = 10**(btermpga + gtermpga - ktermpga*Rrup + gdtermpga)

C     Site Response Terms
c     gs function term
c     linear term
      if (Vs30 .le. VcT) then
         flin = CT*alog(Vs30/Vref)
      else
         flin = CT*alog(VcT/Vref)
      endif
      f2 = 0.5*f4T*(exp(f5T*(min(Vs30,760.0)-360))-exp(f5T*(760.0-360.0)))
      fnl = f2*alog((pgarock+f3)/f3)
      gsterm = (flin + fnl)/alog(10.0)

C     Median Ground motion values
      median = 10**(bterm + gterm - kterm*Rrup + gsterm + gdterm)

C     Convert to LN units
      lnY = alog(Median)
c     Convert units spectral acceleration in gal
      lnY = lnY + 6.89

C     Set sigma values to return
      sigma = sqrt(sigPhiT*sigPhiT + sigTauT*sigTauT)
      phi = sigPhiT
      tau = sigTauT

      return
      end
