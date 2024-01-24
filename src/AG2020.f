c  ***** PEER NGASubduction Models (2019/2020) **********

c ----------------------------------------------------------------------
      subroutine S35_AG2020 ( mag, evType, rRup, vs30, z25, ztor, region, lnY, sigma, phi, tau,
     1            rockPGA, specT, period2, iflag, ACadjfac, epiflag )

C     Model Version: March 16, 2021 - PEER Report Version
C     PGA (T=0.0) set equal to T=0.01 coefficients

      implicit none
      integer MAXPER, NPER
      real sigma, ACadjfac
      parameter (MAXPER=25)

c      character*80 RegName(8), RegName1

      integer i, j, k, region, i9, iParam, count1, count2, iflag
      real mag, rRup, ZTOR, evType
      real rockPGA, period2, period(MAXPER), specT
      real lnY, r
      real VLin(MAXPER), b_soil(MAXPER), c1, vs
      real vs30, sum1, angle1, taperTheta
      real n, c, T1, T2
      real vs1
      real Z25ref
c      integer iprint
      real c1_slab(8), c1_inter(MAXPER), depthLimit, c4
      real z25
      real term1, term1a, term1b, term1c, term2, term3, term4,
     1    term5, term6, term7, term6a
      real theta(45), part(45), NL_soil, lnY1
      real a1(MAXPER), a2(MAXPER), a3(MAXPER), a4(MAXPER), a5(MAXPER),
     1     a6(MAXPER), a7(MAXPER), a8(MAXPER), a9(MAXPER), a10(MAXPER),
     1     a11(MAXPER), a12(MAXPER), a13(MAXPER), a14(MAXPER), a15(MAXPER),
     1     a16(MAXPER), a17(MAXPER), a18(MAXPER), a19(MAXPER), a20(MAXPER),
     1     a21(MAXPER), a22(MAXPER), a23(MAXPER), a24(MAXPER), a25(MAXPER),
     1     a26(MAXPER), a27(MAXPER), a28(MAXPER), a29(MAXPER), a30(MAXPER),
     1     a31(MAXPER), a32(MAXPER), a33(MAXPER), a34(MAXPER), a35(MAXPER),
     1     a36(MAXPER), a37(MAXPER), a38(MAXPER), a39(MAXPER), a40(MAXPER),
     1     a41(MAXPER), a42(MAXPER), a43(MAXPER), a44(MAXPER), a45(MAXPER)
      real AKFac(MAXPER), CasFac(MAXPER), AKFacT, CasFacT
      real rhow(MAXPER), rhob(MAXPER), rhowT, rhobT
      real c1_inter_T, vLin_T, b_soil_T, temp1
      real d1(MAXPER), d2(MAXPER), d1_T, d2_T
      real alpha1, alpha2, f2, f3, phi, tau, phi1, phi2, phi3
      real d0, d3, d4, d5, d6
      real a1Global(MAXPER), a1G_T
      real dAmp_dPGA, taulinPGA, taulin, f2PGA, f3PGA, philinPGA, phiamp, philin
      real phiB, phiBPGA, phiNL, tauNL
      real e1T, e2T, e3T, e1(MAXPER), e2(MAXPER), e3(MAXPER), Cepi
      integer epiflag
      real T1_phi2, T2_phi2, T3_phi2, T4_phi2, T1_phi3, T2_phi3, T3_phi3, T4_phi3
      real d3_phi2, d4_phi2, d5_phi2, d3_phi3, d4_phi3, d5_phi3, alpha_phi3
      real tau_lin, tau_lin_PGA, phi1_sq_100, phi1_sq_PGA_100, phi1_sq, phi1_sq_PGA
      real A_phi2_100, Alpha_phi2, A_phi2, A_phi3, f2_PGA, f3_PGA, phi_lin, phi_lin_PGA, phi_S2S, phi_S2S_PGA
      real phiSS_lin, phiSS_lin_PGA, partial_f_PGA, phi_Amp, Phi_B_PGA, Phi_B
      real phiSQ_NL, tauSQ_NL, phiSS_B_PGA, PhiSS_B, phiSS_sq_NL, phiSS, sigmaSS

c     slab c1: Alaska, CAS, CenAm, Japan, NewZealand, SouthAm, Taiwan, global
      data c1_slab / 7.9, 7.1, 7.4, 7.6, 8.0, 7.5, 7.7, 7.5 /

c      data RegName / 'Alaska', 'Cascadia', 'Central_America', 'Japan',
c     1       'New_Zealand', 'South_America', 'Taiwan', 'Global' /

C     Updated Coefficients 3/16/21 (Consistent with PEER Report)
      data period / 0.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4,
     1              0.5, 0.6, 0.75, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 6.0, 7.5, 10.0 /
      data vLin / 865.1, 865.1, 865.1, 907.8, 1053.5, 1085.7, 1032.5, 877.6, 748.2, 654.3, 587.1,
     1            503.0, 456.6, 430.3, 410.5, 400.0, 400.0, 400.0, 400.0, 400.0, 400.0, 400.0,
     2            400.0, 400.0, 400.0  /
      data b_soil / -1.186, -1.186, -1.219, -1.273, -1.346, -1.471, -1.624, -1.931, -2.188,
     1              -2.381, -2.518, -2.657, -2.669, -2.599, -2.401, -1.955, -1.025, -0.299,
     2               0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data c1_inter / 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2, 8.2,
     1                8.15, 8.1, 8.05, 8.0, 7.95, 7.9, 7.85, 7.8, 7.8, 7.8, 7.8 /
      data A1Global  / 4.596, 4.596, 4.678, 4.773, 5.029, 5.334, 5.455, 5.376, 4.936,
     1                 4.636, 4.423, 4.124, 3.838, 3.562, 3.152, 2.544, 1.636, 1.076,
     2                 0.658, 0.424, 0.093, -0.145, -0.32, -0.556, -0.86 /

      data a2 / -1.45, -1.45, -1.45, -1.45, -1.45, -1.45, -1.45, -1.425, -1.335, -1.275,
     1          -1.231, -1.165, -1.115, -1.071, -1.02, -0.95, -0.86, -0.82, -0.798, -0.793,
     2          -0.793, -0.793, -0.793, -0.793, -0.793 /
      data a6 / -0.0043, -0.0043, -0.0043, -0.0044, -0.0046, -0.0047, -0.0048, -0.0047,
     1          -0.0045, -0.0043, -0.0042, -0.004, -0.0037, -0.0035, -0.0032, -0.0029,
     2          -0.0026, -0.0024, -0.0022, -0.0021, -0.002, -0.002, -0.002, -0.002, -0.002 /
      data a7 / 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21,
     1          3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.13, 2.985, 2.818, 2.682, 2.515, 2.3 /
      data a8 / 0.044, 0.044, 0.044, 0.044, 0.044, 0.044, 0.044, 0.044, 0.043, 0.042, 0.041,
     1          0.04, 0.039, 0.038, 0.037, 0.035, 0.034, 0.032, 0.031, 0.03, 0.029, 0.028,
     2          0.027, 0.026, 0.025 /
      data a10 / 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21,
     1           3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.21, 3.13, 2.985, 2.818, 2.682, 2.515, 2.3 /
      data a11 / 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.007, 0.0062, 0.0056,
     1           0.0051, 0.0043, 0.0037, 0.0033, 0.0027, 0.0019, 0.0008, 0.0, 0.0,
     2           0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data a12 / 0.9, 0.9, 1.008, 1.127, 1.333, 1.565, 1.679, 1.853, 2.022, 2.181, 2.281,
     1           2.379, 2.339, 2.217, 1.946, 1.416, 0.394, -0.417, -0.725, -0.695, -0.638,
     2          -0.597, -0.561, -0.53, -0.486 /
      data a13 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.002, -0.007, -0.011,
     1          -0.015, -0.021, -0.028, -0.041, -0.05, -0.057, -0.065, -0.077, -0.088,
     2          -0.098, -0.11, -0.127 /
      data a14 / -0.46, -0.46, -0.46, -0.46, -0.46, -0.46, -0.46, -0.46, -0.46, -0.46,
     1           -0.46, -0.47, -0.48, -0.49, -0.5, -0.51, -0.52, -0.53, -0.54, -0.54,
     2           -0.54, -0.54, -0.54, -0.54, -0.54 /
C      data a16 / 0.09, 0.09, 0.09, 0.09, 0.09, 0.09, 0.09, 0.09, 0.08412, 0.08038,
C     1           0.078, 0.07463, 0.07213, 0.06975, 0.06713, 0.06325, 0.05887, 0.05875,
C     2           0.05975, 0.05912, 0.05037, 0.04287, 0.03788, 0.03162, 0.02413 /
      data a16 / 0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.090, 0.084, 0.080,
     1           0.078, 0.075, 0.072, 0.070, 0.067, 0.063, 0.059, 0.059, 0.060, 0.059,
     2           0.050, 0.043, 0.038, 0.032, 0.024 /
      data a18 / -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.2, -0.186, -0.15, -0.14, -0.12,
     1           -0.1, -0.08, -0.06, -0.047, -0.035, -0.018, -0.01, -0.005, 0.0, 0.0,
     2            0.0, 0.0, 0.0, 0.0 /
      data a20 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.055, -0.105, -0.134, -0.15,
     1          -0.15, -0.15, -0.15, -0.15, -0.15, -0.13, -0.11, -0.095, -0.085,
     2          -0.073, -0.065, -0.06, -0.055, -0.045 /
      data a21 / 0.04, 0.04, 0.04, 0.04, 0.04, 0.06, 0.1, 0.135, 0.17, 0.17, 0.17, 0.17,
     1           0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17 /
      data a22 / 0.04, 0.04, 0.04, 0.04, 0.04, 0.06, 0.1, 0.135, 0.17, 0.17, 0.17, 0.17,
     1           0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17, 0.17 /
      data a23 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.069, 0.14, 0.164, 0.19, 0.206, 0.22,
     1           0.225, 0.217, 0.185, 0.083, 0.045, 0.026, 0.035, 0.053, 0.072,
     2           0.086, 0.115, 0.151 /
      data a24 / 0.0015, 0.0015, 0.0015, 0.0015, 0.0011, 0.0011, 0.0012, 0.0013, 0.0013,
     1           0.0013, 0.0014, 0.0015, 0.0015, 0.0015, 0.0014, 0.0013, 0.0014, 0.0015,
     2           0.0014, 0.0014, 0.0014, 0.0014, 0.0014, 0.0014, 0.0014 /
      data a25 / 0.0007, 0.0007, 0.0006, 0.0006, 0.0006, 0.0004, 0.0003, -0.0002, -0.0007,
     1          -0.0009, -0.001, -0.001, -0.0011, -0.0012, -0.0011, -0.0008, -0.0004, 0.0002,
     2           0.0004, 0.0007, 0.001, 0.0013, 0.0015, 0.0017, 0.0017 /
      data a26 / 0.0036, 0.0036, 0.0036, 0.0037, 0.0039, 0.0039, 0.0039, 0.0037, 0.0031,
     1           0.0027, 0.002, 0.0013, 0.0009, 0.0006, 0.0003, 0.0001, -0.0001, 0.0,
     2           0.0, 0.0003, 0.0007, 0.0014, 0.0015, 0.0015, 0.0015 /
      data a27 / -0.0004, -0.0004, -0.0005, -0.0007, -0.0009, -0.0009, -0.0008, -0.0009,
     1           -0.001, -0.0011, -0.0009, -0.0007, -0.0007, -0.0007, -0.0007, -0.0008,
     2           -0.0008, -0.0007, -0.0007, -0.0007, -0.0006, -0.0004, -0.0003, -0.0002, -0.0001 /
      data a28 / 0.0025, 0.0025, 0.0025, 0.0025, 0.0026, 0.0026, 0.0026, 0.0022, 0.0018,
     1           0.00155, 0.0014, 0.0011, 0.0008, 0.0006, 0.0004, 0.0002, 0.0001, 0.0002,
     2           0.0002, 0.0004, 0.0006, 0.0008, 0.0011, 0.0014, 0.0017 /
c      data a29 / 0.0006, 0.0006, 0.0005, 0.0005, 0.0004, 0.0003, 0.0003, 0.0001, -0.0001,
c     1          -0.0003, -0.0002, 0.0, 0.0002, 0.0002, 0.0002, 0.0001, 0.0, 0.0, -0.00015,
c     2          -0.0002, -0.00015, -0.00005, 0.0, 0.0001, 0.0002 /
      data a29 / 0.0006, 0.0006, 0.0005, 0.0005, 0.0004, 0.0003, 0.0003, 0.0001, -0.0001,
     1          -0.0003, -0.0002, 0.0, 0.0002, 0.0002, 0.0002, 0.0001, 0.0, 0.0, -0.00015,
     2          -0.0002, -0.0002, -0.0001, 0.0, 0.0001, 0.0002 /
      data a30 / 0.0033, 0.0033, 0.0033, 0.0034, 0.0036, 0.0037, 0.0038, 0.0037, 0.0035,
     1           0.0033, 0.0032, 0.003, 0.0027, 0.0025, 0.0022, 0.0019, 0.0016, 0.0014,
     2           0.0012, 0.0011, 0.001, 0.001, 0.001, 0.001, 0.001 /
      data a31 / 3.77831, 3.77831, 3.82805, 3.89327, 4.2867, 4.59402, 4.70773, 4.6065,
     1           4.1866, 3.85147, 3.57825, 3.24933, 2.98178, 2.77838, 2.47799, 1.92518,
     2           0.99242, 0.46763, 0.0579, -0.13913, -0.30296, -0.40939, -0.50103,
     3          -0.62091, -0.62208 /
      data a32 / 3.34676, 3.34676, 3.44008, 3.50871, 3.65526, 3.97985, 4.1312, 4.27372,
     1           3.96502, 3.68208, 3.5415, 3.32556, 3.13343, 2.92152, 2.53804, 1.96256,
     2           1.35679, 0.81801, 0.43889, 0.10455, -0.15972, -0.20626, -0.32229,
     3          -0.42231, -0.59085 /
      data a33 / 3.80252, 3.80252, 3.90532, 4.01892, 4.2952, 4.54636, 4.61378, 4.52897,
     1           4.1656, 3.91471, 3.78463, 3.57016, 3.35516, 3.09216, 2.65718, 2.14585,
     2           1.3499, 0.81478, 0.39788, 0.1046, -0.23242, -0.57223, -0.86305, -1.17731, -1.40701 /
      data a34 / 5.03612, 5.03612, 5.13749, 5.26986, 5.61569, 6.02041, 6.16254, 5.96141,
     1           5.39202, 5.01168, 4.70573, 4.28959, 3.93215, 3.61493, 3.17852, 2.57218,
     2           1.64993, 1.06575, 0.63103, 0.38818, 0.01644, -0.28023, -0.4822, -0.75661, -1.08704 /
      data a35 / 4.62715, 4.62715, 4.69578, 4.78092, 5.02105, 5.3474, 5.50651, 5.51804, 5.1668,
     1           4.87436, 4.6544, 4.36597, 4.07791, 3.81455, 3.4391, 2.80555, 1.85463, 1.30203,
     2           0.8017, 0.59579, 0.35222, 0.18743, -0.12426, -0.33163, -0.67834 /
      data a36 / 4.80444, 4.80444, 4.89425, 5.00277, 5.28185, 5.61229, 5.76676, 5.7313, 5.29426,
     1           5.00579, 4.75879, 4.3789, 4.03937, 3.73663, 3.29299, 2.64751, 1.68417, 1.10019,
     2           0.67373, 0.41262, 0.0097, -0.27152, -0.45911, -0.68215, -0.91734 /
      data a37 / 3.56691, 3.56691, 3.64249, 3.70633, 3.91843, 4.22072, 4.35356, 4.36641,
     1           4.01685, 3.75904, 3.59143, 3.37039, 3.15641, 2.95835, 2.65564, 2.06665,
     2           1.33157, 0.76066, 0.36483, 0.16882, -0.03231, -0.15158, -0.22172,
     3          -0.33381, -0.54408 /

      data a39  / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.101, 0.184, 0.315, 0.416,
     1            0.499, 0.6, 0.731, 0.748, 0.761, 0.77, 0.778, 0.79, 0.799, 0.807, 0.817, 0.829 /
      data a41  / -0.029, -0.029, -0.024, -0.034, -0.061, -0.076, -0.049, -0.026, -0.011, -0.009,
     1             0.005, 0.04, 0.097, 0.145, 0.197, 0.269, 0.347, 0.384, 0.397, 0.404, 0.397,
     2             0.378, 0.358, 0.333, 0.281 /

      data d1 / 0.325, 0.325, 0.325, 0.325, 0.325, 0.325, 0.325, 0.325, 0.325, 0.325,
     1          0.325, 0.325, 0.325, 0.325, 0.325, 0.325, 0.312, 0.302, 0.295, 0.289,
     2          0.28, 0.273, 0.267, 0.259, 0.25 /
      data d2 / 0.137, 0.137, 0.137, 0.137, 0.137, 0.137, 0.137, 0.137, 0.137, 0.137,
     1          0.137, 0.137, 0.137, 0.137, 0.137, 0.137, 0.113, 0.096, 0.082, 0.072,
     2          0.055, 0.041, 0.03, 0.017, 0.0 /
c      data AKfac / 0.487, 0.487, 0.519, 0.543, 0.435, 0.410, 0.397, 0.428, 0.442,
c     1             0.494, 0.565, 0.625, 0.634, 0.581, 0.497, 0.469, 0.509, 0.478,
c     2             0.492, 0.47, 0.336, 0.228, 0.151, 0.051, -0.251 /
c      data CasFac / 0.828, 0.828, 0.825, 0.834, 0.895, 0.863, 0.842, 0.737, 0.746,
c     1              0.796, 0.782, 0.768, 0.728, 0.701, 0.685, 0.642, 0.325, 0.257,
c     2              0.211, 0.296, 0.232, 0.034, -0.037, -0.178, -0.313 /
      data AKfac / 0.487, 0.487, 0.519, 0.543,  0.435, 0.410, 0.397, 0.428, 0.442, 0.494,
     2             0.565, 0.625, 0.634, 0.581,  0.498, 0.458, 0.499, 0.478, 0.495, 0.451,
     4             0.322, 0.194, 0.127, 0.051, -0.190 /
      data CasFac / 0.828, 0.828,  0.825,  0.834,  0.895, 0.863, 0.842, 0.737, 0.746, 0.796,
     1              0.782, 0.768,  0.728,  0.701,  0.686, 0.631, 0.315, 0.257, 0.214, 0.278,
     2              0.219, 0.001, -0.062, -0.177, -0.252 /

      data rhow / 1.0, 1.0, 0.99, 0.99, 0.97, 0.95, 0.92, 0.9, 0.87, 0.84, 0.82,
     1            0.74, 0.66, 0.59, 0.5, 0.41, 0.33, 0.3, 0.27, 0.25, 0.22, 0.19,
     2            0.17, 0.14, 0.1 /
      data rhob / 1.0, 1.0, 0.99, 0.99, 0.985, 0.98, 0.97, 0.96, 0.94, 0.93, 0.91,
     1            0.86, 0.8, 0.78, 0.73, 0.69, 0.62, 0.56, 0.52, 0.495, 0.43, 0.4,
     2            0.37, 0.32, 0.28 /
      data e1 / 0.550, 0.550, 0.550, 0.550, 0.560, 0.580, 0.590, 0.590, 0.570, 0.530,
     1          0.490, 0.425, 0.375, 0.345, 0.300, 0.240, 0.230, 0.230, 0.230, 0.240,
     2          0.270, 0.300, 0.320, 0.350, 0.350 /
      data e2 / -0.270, -0.270, -0.270, -0.270, -0.270, -0.270, -0.270, -0.270, -0.270,
     1          -0.224, -0.186, -0.126, -0.079, -0.041,  0.005,  0.065,  0.065,  0.065,
     2           0.065,  0.065,  0.065,  0.065,  0.065,  0.065,  0.065 /
      data e3 / 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.050, 0.043,
     1          0.037, 0.028, 0.022, 0.016, 0.009, 0.000, 0.000, 0.000, 0.000, 0.000,
     2          0.000, 0.000, 0.000, 0.000, 0.000 /

c     set period-independent terms
      depthLimit = 200.
      c4 = 10.
      n = 1.18
      c = 1.88
      theta(1) = 0.
      theta(3) = 0.1
      theta(4) = 0.73
      theta(5) = 0.
      theta(9) = 0.4
c      theta(15) = 0.
      theta(15) = -0.1
      theta(17) = 0.
      theta(19) = 0.
      theta(38) = 0.
      theta(40) = 0.
      theta(42) = 0.
      theta(43) = 0.
      theta(44) = 0.
      theta(45) = 0.34
      d0 = 0.47
      d3 = 0.109
      d4 = 0.062
      d5 = 0.470
      d6 = 0.242
      alpha2 = 0.42

      nper = 25

C First check for the PGA case
      if (specT .eq. 0.0) then
         vlin_T = vLin(1)
         b_soil_T = b_soil(1)
         c1 = c1_inter(1)
         c1_inter_T = c1_inter(1)
c         if ( region .eq. 8 ) theta(1) = a1Global(1)
         if ( region .eq. 8 ) then
            theta(1) = a1Global(1)
         else
            theta(1) = 0.0
         endif
         theta(2) = a2(1)
         theta(6) = a6(1)
         theta(7) = a7(1)
         theta(8) = a8(1)
         theta(10) = a10(1)
         theta(11) = a11(1)
         theta(12) = a12(1)
         theta(13) = a13(1)
         theta(14) = a14(1)
         theta(16) = a16(1)
         theta(18) = a18(1)
         theta(20) = a20(1)
         theta(21) = a21(1)
         theta(22) = a22(1)
         theta(23) = a23(1)
         theta(24) = a24(1)
         theta(25) = a25(1)
         theta(26) = a26(1)
         theta(27) = a27(1)
         theta(28) = a28(1)
         theta(29) = a29(1)
         theta(30) = a30(1)
         theta(31) = a31(1)
         theta(32) = a32(1)
         theta(33) = a33(1)
         theta(34) = a34(1)
         theta(35) = a35(1)
         theta(36) = a36(1)
         theta(37) = a37(1)
         theta(39) = a39(1)
         theta(41) = a41(1)
         d1_T = d1(1)
         d2_T = d2(1)
         AKfacT = AKFac(1)
         CasfacT = CasFac(1)
         rhowT = rhow(1)
         rhobT = rhob(1)
         e1T = e1(1)
         e2T = e2(1)
         e3T = e3(1)
         goto 1011
      endif

C   For other periods, loop over the spectral period range of the attenuation relationship.
      do i=2,nper-1
         if (specT .ge. period(i) .and. specT .le. period(i+1) ) then
            count1 = i
            count2 = i+1
            goto 1020
         endif
      enddo

C Selected spectral period is outside range defined by attenuaton model.
      write (*,*)
      write (*,*) 'AG (2020) Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),b_soil(count1),b_soil(count2),
     +                   specT,b_soil_T,iflag)
      call S24_interp (period(count1),period(count2),vlin(count1),vlin(count2),
     +                   specT,vlin_T,iflag)
      call S24_interp (period(count1),period(count2),c1_inter(count1),c1_inter(count2),
     +                   specT,c1_inter_T,iflag)
      if (region .eq. 8) then
         call S24_interp (period(count1),period(count2),a1Global(count1),a1Global(count2),
     +                   specT,theta(1),iflag)
      endif
      call S24_interp (period(count1),period(count2),a2(count1),a2(count2),
     +                   specT,theta(2),iflag)
      call S24_interp (period(count1),period(count2),a6(count1),a6(count2),
     +                   specT,theta(6),iflag)
      call S24_interp (period(count1),period(count2),a7(count1),a7(count2),
     +                   specT,theta(7),iflag)
      call S24_interp (period(count1),period(count2),a8(count1),a8(count2),
     +                   specT,theta(8),iflag)
      call S24_interp (period(count1),period(count2),a10(count1),a10(count2),
     +                   specT,theta(10),iflag)
      call S24_interp (period(count1),period(count2),a11(count1),a11(count2),
     +                   specT,theta(11),iflag)
      call S24_interp (period(count1),period(count2),a12(count1),a12(count2),
     +                   specT,theta(12),iflag)
      call S24_interp (period(count1),period(count2),a13(count1),a13(count2),
     +                   specT,theta(13),iflag)
      call S24_interp (period(count1),period(count2),a14(count1),a14(count2),
     +                   specT,theta(14),iflag)
      call S24_interp (period(count1),period(count2),a16(count1),a16(count2),
     +                   specT,theta(16),iflag)
      call S24_interp (period(count1),period(count2),a18(count1),a18(count2),
     +                   specT,theta(18),iflag)
      call S24_interp (period(count1),period(count2),a20(count1),a20(count2),
     +                   specT,theta(20),iflag)
      call S24_interp (period(count1),period(count2),a21(count1),a21(count2),
     +                   specT,theta(21),iflag)
      call S24_interp (period(count1),period(count2),a22(count1),a22(count2),
     +                   specT,theta(22),iflag)
      call S24_interp (period(count1),period(count2),a23(count1),a23(count2),
     +                   specT,theta(23),iflag)
      call S24_interp (period(count1),period(count2),a24(count1),a24(count2),
     +                   specT,theta(24),iflag)
      call S24_interp (period(count1),period(count2),a25(count1),a25(count2),
     +                   specT,theta(25),iflag)
      call S24_interp (period(count1),period(count2),a26(count1),a26(count2),
     +                   specT,theta(26),iflag)
      call S24_interp (period(count1),period(count2),a27(count1),a27(count2),
     +                   specT,theta(27),iflag)
      call S24_interp (period(count1),period(count2),a28(count1),a28(count2),
     +                   specT,theta(28),iflag)
      call S24_interp (period(count1),period(count2),a29(count1),a29(count2),
     +                   specT,theta(29),iflag)
      call S24_interp (period(count1),period(count2),a30(count1),a30(count2),
     +                   specT,theta(30),iflag)
      call S24_interp (period(count1),period(count2),a31(count1),a31(count2),
     +                   specT,theta(31),iflag)
      call S24_interp (period(count1),period(count2),a32(count1),a32(count2),
     +                   specT,theta(32),iflag)
      call S24_interp (period(count1),period(count2),a33(count1),a33(count2),
     +                   specT,theta(33),iflag)
      call S24_interp (period(count1),period(count2),a34(count1),a34(count2),
     +                   specT,theta(34),iflag)
      call S24_interp (period(count1),period(count2),a35(count1),a35(count2),
     +                   specT,theta(35),iflag)
      call S24_interp (period(count1),period(count2),a36(count1),a36(count2),
     +                   specT,theta(36),iflag)
      call S24_interp (period(count1),period(count2),a37(count1),a37(count2),
     +                   specT,theta(37),iflag)
      call S24_interp (period(count1),period(count2),a39(count1),a39(count2),
     +                   specT,theta(39),iflag)
      call S24_interp (period(count1),period(count2),a41(count1),a41(count2),
     +                   specT,theta(41),iflag)
      call S24_interp (period(count1),period(count2),d1(count1),d1(count2),
     +                   specT,d1_T,iflag)
      call S24_interp (period(count1),period(count2),d2(count1),d2(count2),
     +                   specT,d2_T,iflag)
      call S24_interp (period(count1),period(count2),AKFac(count1),AKFac(count2),
     +                   specT,AKFacT,iflag)
      call S24_interp (period(count1),period(count2),CasFac(count1),CasFac(count2),
     +                   specT,CasFacT,iflag)
      call S24_interp (period(count1),period(count2),rhow(count1),rhow(count2),
     +                   specT,rhowT,iflag)
      call S24_interp (period(count1),period(count2),rhob(count1),rhob(count2),
     +                   specT,rhobT,iflag)
      call S24_interp (period(count1),period(count2),e1(count1),e1(count2),
     +                   specT,e1T,iflag)
      call S24_interp (period(count1),period(count2),e2(count1),e2(count2),
     +                   specT,e2T,iflag)
      call S24_interp (period(count1),period(count2),e3(count1),e3(count2),
     +                   specT,e3T,iflag)

 1011 period2 = specT


c     Set c1_slab
      if (evType .eq. 0. ) then
          c1 = c1_inter_T
      else
          c1 = c1_slab(region)
      endif

      r = rrup +  c4*exp((mag-6.)*theta(9))

c     Set break in slope of soil term
      VS1 = 1000.
      if ( vs30 .gt. VS1 ) then
        vs = VS1
      else
        vs = vs30
      endif
c      write (*,'( 2f10.3)') vs, vlin_T
c      pause 'vs'

c     set fixed non-linear soil
      if ( vs .lt. vLin_T) then
        nl_soil =  ( - b_soil_T*alog(c+rockPGA)
     1            + b_soil_T*alog(rockPga+c*(vs/vLin_T)**(n)) )
      else
        nl_soil = (b_soil_T*n*alog(vs/vLIN_T) )
      endif

c     Set partial derivatives with respect to coefficients
      part(1) = 1.
      part(2) = alog(r)
      part(3) = (mag - 7.) * alog(r)

      if ( mag .le. c1 ) then
        part(4) = mag - c1 + (c1_slab(region)-7.5) * evType
        part(5) = 0.
      else
        part(4) = (c1_slab(region)-7.5) * evType
        part(5) = mag - c1
      endif
      part(6) = rRup
      part(7) = 0.
      part(8) = 0.
      part(9) = 0.

      if ( region .eq. 3 .or. region .eq. 4 .or. region .eq. 6 ) then
        part(7) = 0.
        part(10) = evType
      else
        part(7) = evType
        part(10) = 0.
      endif

      if ( ztor .lt. depthLimit ) then
        if ( ztor .lt. 50. ) then
          part(8) = (ztor-50.) * evType
          part(11) = 0.
        else
          part(8) = 0.
          part(11) = evType * (ztor - 50.)
        endif
      else
         part(11) = evType * (depthLimit - 50.)
      endif

      part(12) = alog(vs/vLin_T)
      part(13) = (10. - mag )**2
      part(14) = alog(r) * evType

c      if ( region .eq. 3 )  then
c        part(15) =  alog(r)
c      else
c        part(15) = 0.
c      endif
C     Mainshock/Aftershock Flag not currently coded
      part(15) = 0.

      if ( region .eq. 7 )  then
        part(16) =  alog(r)
      else
        part(16) = 0.
      endif

c     Initialize regional terms
      do i9=17,45
        part(i9) = 0.
      enddo

c     Region-dependent VS30 terms
      if ( region .eq. 1 ) then
        part(17) =  alog(vs/vLin_T)
      elseif ( region .eq. 2 ) then
        part(18) =  alog(vs/vLin_T)
      elseif ( region .eq. 3 ) then
        part(19) =  alog(vs/vLin_T)
      elseif ( region .eq. 4 ) then
        part(20) =  alog(vs/vLin_T)
      elseif ( region .eq. 5 ) then
        part(21) =  alog(vs/vLin_T)
      elseif ( region .eq. 6 ) then
        part(22) =  alog(vs/vLin_T)
      elseif ( region .eq. 7 ) then
        part(23) =  alog(vs/vLin_T)
      endif

c     Region-dependent R terms (interface)
      if ( region .eq. 1 ) then
        part(24) =  rRup
      elseif ( region .eq. 2 ) then
        part(25) =  rRup
      elseif ( region .eq. 3 ) then
        part(26) =  rRup
      elseif ( region .eq. 4 ) then
        part(27) =  rRup
      elseif ( region .eq. 5 ) then
        part(28) =  rRup
      elseif ( region .eq. 6 ) then
        part(29) =  rRup
      elseif ( region .eq. 7 ) then
        part(30) =  rRup
      endif

c     Region-dependent Constant terms
      if ( region .eq. 1 ) then
        part(31) =  1.
      elseif ( region .eq. 2 ) then
        part(32) =  1.
      elseif ( region .eq. 3 ) then
        part(33) =  1.
      elseif ( region .eq. 4 ) then
        part(34) =  1.
      elseif ( region .eq. 5 ) then
        part(35) =  1.
      elseif ( region .eq. 6 ) then
        part(36) =  1.
      elseif ( region .eq. 7 ) then
        part(37) =  1.
      endif

c     set z2.5_ref
c     Calc reference z25 for given vs30 for Cascadia(Region=2) or Japan (Region=4)
      if ( Region .eq. 2 ) then
	    call S35_z25_interp ( vs30, 200., 570., 8.52, 7.6, z25ref )
      elseif ( Region .eq. 4 ) then
	    call S35_z25_interp ( vs30, 170., 800., 7.3, 4.1, z25ref )
      else
            z25Ref = -1.
      endif

c     Z2.5 scaling
      if ( z25 .ge. 0. .and. z25ref .ge. 0.) then
        temp1 = alog ( ( z25*1000.0 + 50.) /(z25ref+50.) )
      else
        temp1 = 0.
      endif
      if ( z25 .gt. 0. ) then
        if ( region .eq. 1 ) then
           part(38) =  temp1
        elseif ( region .eq. 2 ) then
        if ( temp1 .gt. 0. ) then
           part(39) =  temp1-0.
        else
           part(39) = 0.
        endif
        elseif ( region .eq. 3 ) then
           part(40) =  temp1
        elseif ( region .eq. 4 ) then
          if ( temp1 .gt. -2. ) then
            part(41) =  temp1
          else
c            part(41) = 0.0
            part(41) = -2.0
          endif
        elseif ( region .eq. 5 ) then
          part(42) =  temp1
        elseif ( region .eq. 6 ) then
          part(43) = temp1
        elseif ( region .eq. 7 ) then
          part(44) =  temp1
        endif
      endif

c     Add mag scaling difference for interface and slab
      part(45) = (c1_slab(region)-7.5) * evType
      if ( evType .eq. 1 .and. mag .le. c1 ) then
c      if ( evType .eq. 1 ) then
         part(45) =  mag - c1 + part(45)
      endif

c     comptue median
      lnY = NL_soil
      do iParam=1,45
        lnY = lnY + theta(iParam) * part(iParam)
      enddo

C     Apply Alaska or Cascadia adjustment is requested.
      if (Region .eq. 1 ) then
         ACadjfac = AKfacT
      elseif (Region .eq. 2 ) then
         ACadjfac = CasfacT
      else
         ACadjfac = 0.0
      endif


      term1 = theta(1)*part(1) + theta(2)*part(2) + theta(3)*part(3)
     1            + theta(6)*part(6)

      term2 = theta(4)*part(4) +  theta(5)*part(5) + theta(13)*part(13)

      term3 = theta(10)*part(10) + theta(7)*part(7) + theta(14)*part(14)
      term4 = theta(45)*part(45)
      term5 = theta(8)*part(8) + theta(11)*part(11)
      term6 = theta(12)*part(12) + NL_soil
      term1c = theta(15)*part(15) + theta(16)*part(16)
      term6a = 0.
      do k=17,23
        term6a = term6a + theta(k)*part(k)
      enddo
      term1a = 0.
      do k=24,30
        term1a = term1a + theta(k)*part(k)
      enddo
      term1b = 0.
      do k=31,37
        term1b = term1b + theta(k)*part(k)
      enddo
      term7 = 0.
      do k=38,44
        term7 = term7 + theta(k)*part(k)
      enddo

      lnY1 = term1 + term2 + term3 + term4 + term5 + term6 + term6a + term7
     1      + term1a + term1b + term1c

C     Apply Epistemic model only for Global version
      if (Region .eq. 8) then
         cepi = e1T + e2T*(Rrup/100.0) + e3T*(Rrup/100.0)*(Rrup/100.0)
         if (epiflag .eq. -1) then
            lnY = lnY - cepi
            lnY1 = lnY1 - cepi
         elseif (epiflag .eq. 1) then
            lnY = lnY + cepi
            lnY1 = lnY1 + cepi
         endif
      endif

c      if (specT .eq. 0.0 .and. vs30 .eq. 760.0) then
c          write (*,'( i5,30f10.4))')
c     1     region, mag, ZTOR, evType, rrup, vs30, z25, z25ref,
c     1      rockpga, specT, lnY, exp(lnY1),
c     1      term1, term1a , term1b , term1c, term2, term3, term4, term5,
c     1      term6, term6a, term7
c
c      endif


c      if (iprint .eq. 1 ) write (33,'( i5,30f10.4))')
c     1     region, mag, ZTOR, evType, rrup, vs30, z25,
c     1      rockpga, lnY,
c     1      term1, term2, term3, term4, term5, term6, term6a, term7,
c     1      term1a , term1b , term1c, z25ref, temp1,
c     1      alog ( ( z25 + 50.) /(z25ref+50.) )

c       regName1 = regName(region)

c     Sigma models

c     set coefficents
      d0 = 0.47
      T1_phi2 = 0.03
      T2_Phi2 = 0.075
      T3_phi2 = 0.20
      T4_phi2 = 1.0
      T1_phi3 = 0.03
      T2_Phi3 = 0.075
      T3_phi3 = 0.10
      T4_phi3 = 0.3
      d3_phi2 = 0.109
      d4_phi2 = 0.062
      d5_phi2 = 0.470
      d3_phi3 = 0.242
      d4_phi3 = 0.0
      d5_phi3 = 0.0
      alpha_phi3 = 0.42

C     Set alpha for phi2 (eq 5.6)
      if (rrup .le. 250. ) then
        alpha_phi2 = 1.
      else if ( rrup .lt. 450. ) then
        alpha_phi2 = 1 - 0.0036*(Rrup-250.)
      else
        alpha_phi2 = 0.28
      endif

c     tau model
      tau_LIN = d0
      tau_LIN_PGA = d0

c     phi1 model
      phi1_sq_100 = d1_T
      phi1_sq_PGA_100 = d1(1)
      if ( rrup .lt. 150. ) then
        phi1_SQ = d1_T
        phi1_SQ_PGA = d1(1)
      elseif ( Rrup .lt. 450. ) then
        phi1_SQ = d1_T + d2_T * (Rrup-150)/300.
        phi1_SQ_PGA = d1(1) + d2(1) * (Rrup-150)/300.
      else
        phi1_SQ = d1_T + d2_T
        phi1_SQ_PGA = d1(1) + d2(1)
      endif

c     phi2 model
c     A_phi term (eq 5.4)
      A_phi2_100 = d3_phi2
      if ( Rrup .lt. 225. ) then
        A_phi2 = d3_phi2
      elseif ( Rrup .lt. 450. ) then
        A_phi2 = d3_phi2 + d4_phi2 * (Rrup-225)/225. + d5_phi2 * ((Rrup-225)/225.)**2
      else
        A_phi2 = d3_phi2 + d4_phi2 + d5_phi2
      endif

c     f2 term for phi2 model (eq 5.3)
      if ( specT .lt. T1_phi2 ) then
        f2 = 1 - alpha_phi2
      elseif ( specT .lt. T2_phi2 ) then
        f2 = 1 - alpha_phi2 *alog(specT/T2_phi2)/alog(T1_phi2/T2_phi2)
      elseif ( specT .lt. T3_phi2 ) then
        f2 = 1.0
      elseif ( specT .lt. T4_phi2 ) then
        f2 = alog(specT/T4_phi2)/alog(T3_phi2/T4_phi2)
      else
        f2 = 0.
      endif
      f2_PGA = 1 - alpha_phi2

c     Compute added variance term, phi2
c      phi2_SQ_add = A_phi2 * f2

c     f2 for phi3 (same for as eq 5.3, but different coeff)
      if ( specT .lt. T1_phi3 ) then
        f3 = 1 - alpha_phi3
      elseif ( specT .lt. T2_phi3 ) then
        f3 = 1 - alpha_phi3 *alog(specT/T2_phi3)/alog(T1_phi3/T2_phi3)
      elseif ( specT .lt. T3_phi3 ) then
        f3 = 1.0
      elseif ( specT .lt. T4_phi3 ) then
        f3 = alog(specT/T4_phi3)/alog(T3_phi3/T4_phi3)
      else
        f3 = 0.
      endif
      A_phi3 = d3_phi3
      f3_PGA = 1 - alpha_phi3

c      phi3_SQ_add = A_phi3 * f3

c     compute region-specific phi
      if ( region .eq. 3) then
        phi_LIN = sqrt( phi1_SQ + A_phi3*f3 )
        phi_LIN_PGA = sqrt( phi1_SQ_PGA + A_phi3*f3_PGA )
        phi_S2S = sqrt( phi1_sq_100 + A_phi3*f3 - 0.165)
        phi_S2S_PGA = sqrt( phi1_SQ_PGA_100 + A_phi3*f3_PGA - 0.165)
      elseif ( region .eq. 4 .or. region .eq. 6 ) then
        phi_LIN = sqrt( phi1_SQ + A_phi2*f2 + A_phi3*f3)
        phi_LIN_PGA = sqrt( phi1_SQ_PGA + A_phi2*f2_PGA + A_phi3*f3_PGA)
        phi_S2S = sqrt( phi1_SQ_100 + A_phi2_100*f2 + A_phi3*f3 - 0.18)
        phi_S2S_PGA = sqrt( phi1_SQ_PGA_100 + A_phi2_100*f2 + A_phi3*f3_PGA - 0.18)
      else
        phi_LIN = sqrt(phi1_SQ)
        phi_LIN_PGA = sqrt(phi1_SQ)
        phi_S2S = sqrt( phi1_SQ_100 - 0.165)
        phi_S2S_PGA = sqrt( phi1_SQ_PGA_100 - 0.165)
      endif

c     compute linear single-station sigma
      PhiSS_LIN = sqrt( phi_LIN**2 - phi_S2S**2 )
      PhiSS_LIN_PGA = sqrt( phi_LIN_PGA**2 - phi_S2S**2 )

c     NL site effects on phi and tau

c     eq 5.9
      If ( vs30 .ge. VLIN_T ) then
        partial_f_PGA = 0.
      else
        partial_f_PGA = b_soil_T * rockPGA *
     1       ( -1./(rockPGA+c) + 1. / (rockPGA+c*(vs30/VLIN_T)**n) )
      endif

c     eq 5.7
      phi_AMP = 0.3
      phi_B_PGA = sqrt( phi_LIN_PGA**2 - phi_AMP**2)
      phi_B = sqrt( phi_LIN**2 - phi_AMP**2)

c     eq 5.8
      phiSQ_NL = phi_LIN**2 + partial_f_PGA**2 * phi_B**2
     1           + 2. * partial_f_PGA * phi_B_PGA * phi_B * rhoWT

c     eq 5.10
      tauSQ_NL = tau_LIN**2 + partial_f_PGA**2 * tau_LIN_PGA**2
     1           + 2. * partial_f_PGA * tau_LIN_PGA * tau_LIN * rhoBT

      phi = sqrt(phiSQ_NL)
      tau = sqrt(tauSQ_NL)
      sigma = sqrt (phiSQ_NL + tauSQ_NL)

c     NL effects on phiSS and sigmaSS
c     eq 5.7
      phi_AMP = 0.3
      phiSS_B_PGA = sqrt( phiSS_LIN_PGA**2 - phi_AMP**2)
      phiSS_B = sqrt( phiSS_LIN**2 - phi_AMP**2)

c     eq 5.8
      phiSS_SQ_NL = phiSS_LIN**2 + partial_f_PGA**2 * phiSS_B**2
     1           + 2. * partial_f_PGA * phiSS_B_PGA * phiSS_B * rhoWT

      phiSS = sqrt(phiSS_SQ_NL)
      sigmaSS = sqrt (phiSS_SQ_NL + tauSQ_NL)


c     Convert units spectral acceleration in gal
      lnY = lnY + 6.89

      return
      end

c -------------------------------------------------------------------

      subroutine S35_z25_interp ( vs30, x1, x2, y1, y2, y)
      real vs30, x1, x2, y1, y2, y, x, x1Log, x2Log

      x = alog(vs30)
      x1Log = alog(x1)
      x2Log = alog(x2)
      if ( x .lt. x1Log ) then
        y = y1
      elseif ( x .gt. x2Log ) then
        y = y2
      else
        y = (x-x1Log)/(x2Log-x1Log) * (y2-y1) + y1
      endif
      y = exp(y)

      return
      end
