c ----------------------------------------------------------------------
      subroutine S35_PSHAB2019 ( mag, Ftype, rRup, vs30, z25, lnY, sigma, phi, tau,
     2                     specT, period2, iflag, depth, Rhypo, region, mbInter, mbSlab, pnwbflag )

C     Model Version: December 16, 2020

      implicit none
      integer MAXPER
      parameter (MAXPER=26)
      integer count1, count2, iflag, nPer, i1, i, region, pnwbflag

      real Period(MAXPER), c1(MAXPER), c1Slab(MAXPER), c4(MAXPER), c5(MAXPER), C6(MAXPER),
     1     f4(MAXPER), f5(MAXPER), Vc(MAXPER),
     2     Alaska_a0(MAXPER), CAM_a0(MAXPER),
     3     Japan_a0(MAXPER), SA_a0(MAXPER), Taiwan_a0(MAXPER), Global_a0(MAXPER),
     4     Global_c0(MAXPER), Alaska_c0(MAXPER), CAM_c0(MAXPER),
     5     Taiwan_C0(MAXPER), Cascadia_c0(MAXPER), Japan_Pac_c0(MAXPER), Japan_Phi_c0(MAXPER), SAN_c0(MAXPER), SAS_c0(MAXPER),
     6     Global_c0s(MAXPER), Alaska_c0s(MAXPER),  Aleutian_c0s(MAXPER), CAM_c0s(MAXPER), Japan_c0s(MAXPER),
     5     SAN_c0s(MAXPER), SAS_c0s(MAXPER), Taiwan_C0s(MAXPER), Cascadia_c0s(MAXPER),
     6     Global_c(MAXPER), Alaska_c(MAXPER), Cascadia_c(MAXPER), Japan_c(MAXPER),
     7     SA_c(MAXPER), Taiwan_c(MAXPER), d(MAXPER), m(MAXPER), db(MAXPER),
     8     Japan_e1(MAXPER), Japan_e2(MAXPER), Japan_e3(MAXPER),
     9     Cas_e1(MAXPER), Cas_e2(MAXPER), Cas_e3(MAXPER), SeaBasinD(MAXPER), BasinD(MAXPER)
      real Japan_c1(MAXPER), Taiwan_c1(MAXPER)
      real Aleutian_c0(MAXPER), Aleutian_c0T

      real c4slab(MAXPER), c5slab(MAXPER), c6slab(MAXPER), Alaska_a0Slab(MAXPER), Cascadia_a0Slab(MAXPER),
     1     CAM_a0Slab(MAXPER), Japan_a0Slab(MAXPER), SA_a0Slab(MAXPER), Taiwan_a0Slab(MAXPER), Global_a0Slab(MAXPER)

      real sigtau(MAXPER), sigphi1(MAXPER), sigphi2(MAXPER), sigphiv(MAXPER), tau, phi
      real c1T, c1SlabT, c4T, c5T, c6T, f4T, f5T, VcT, sigPhi1T, sigphi2T, sigPhivT, sigTauT, a0T, c0T, cT
      real Global_a0T, Alaska_a0T, CAM_a0T, Japan_a0T, SA_a0T, Taiwan_a0T
      real Global_c0T, Alaska_c0T, CAM_c0T, Taiwan_c0T, Cascadia_c0T, Japan_Pac_c0T, Japan_Phi_c0T, SAN_c0T, SAS_c0T
      real Global_c0sT, Alaska_c0sT, Aleutian_c0sT, CAM_c0sT, Japan_c0sT, SAN_c0sT, SAS_c0sT, Taiwan_c0sT, Cascadia_c0sT
      real Global_cT, Alaska_cT, Cascadia_cT, Japan_cT, SA_cT, Taiwan_cT, dT, mT, dbT
      real Japan_e1T, Japan_e2T, Japan_e3T, Cas_e1T, Cas_e2T, Cas_e3T
      real SeaBasinDT, BasinDT
      real Japan_c1T, Taiwan_c1T
      real c4slabT, c5SlabT, c6SlabT
      real Alaska_a0SlabT, Cascadia_a0SlabT, CAM_a0SlabT,
     1     Japan_a0SlabT, SA_a0SlabT, Taiwan_a0SlabT, Global_a0SlabT
      real a0Pga, c0Pga, fpPga, fmPga, fdPga, x

      real sigma, lnY, pgaRock, vs30, rRup, Rhypo, mag, depth, Ftype
      real period2, specT, z25
      real b4, f1, f3, Vb, Vref, mbInter, mbslab
      real Fm, Fp, Fd, Fs, Flin, Fnl, f2, db1, h, R, Rref, muz25, deltaz25, fb, cas_e3TBasin, cas_e2TBasin
      real phirup, phivs, phivar, V1

      data  period  / 0.0, -1.0, 0.01, 0.02, 0.025, 0.03, 0.04, 0.05, 0.075, 0.1, 0.15,
     1                0.2, 0.25, 0.3, 0.4, 0.5, 0.75, 1.0, 1.5, 2.0, 2.5, 3.0, 4.0, 5.0, 7.5, 10.0  /

C     Model Constants - Updated (12/16/20) revised C6 for Interface

      Data Global_c0 / 4.082, 8.097, 3.714, 3.762, 3.859, 4.014, 4.223, 4.456, 4.742, 4.952, 5.08,
     1                 5.035, 4.859, 4.583, 4.18, 3.752, 3.085, 2.644, 2.046, 1.556, 1.167, 0.92,
     2                 0.595, 0.465, 0.078, 0.046 /
      Data Alaska_c0 / 4.458796298, 9.283796298, 4.094796298, 4.132796298, 4.246796298, 4.386796298,
     1                 4.553796298, 4.745796298, 4.972796298, 5.160796298, 5.285796298, 5.277796298,
     2                 5.154796298, 4.910796298, 4.548796298, 4.168796298, 3.510796298, 3.067796298,
     3                 2.513796298, 2.061796298, 1.709796298, 1.456796298, 1.207796298, 1.131796298,
     4                 0.758796298, 0.708796298 /
      Data Aleutian_c0 / 3.652796298, 8.374796298, 3.288796298, 3.338796298, 3.392796298, 3.535796298,
     1                   3.747796298, 3.959796298, 4.231796298, 4.471796298, 4.665796298, 4.661796298,
     2                   4.503796298, 4.276796298, 3.919796298, 3.486796298, 2.710796298, 2.238796298,
     3                   1.451796298, 0.906796298, 0.392796298, 0.099796298, -0.356203702, -0.601203702,
     4                  -1.137203702, -1.290203702 /
      Data Cascadia_c0 / 3.856, 7.728, 3.488, 3.536, 3.633, 3.788, 3.997, 4.23, 4.516, 4.726, 4.848,
     1                   4.798, 4.618, 4.34, 3.935, 3.505, 2.837, 2.396, 1.799, 1.31, 0.922, 0.675,
     2                   0.352, 0.223, -0.162, -0.193 /
      Data CAM_c0 / 2.875899908, 7.046899908, 2.564899908, 2.636899908, 2.731899908, 2.890899908,
     1              3.075899908, 3.287899908, 3.560899908, 3.788899908, 3.945899908, 3.943899908,
     2              3.800899908, 3.491899908, 3.128899908, 2.640899908, 1.987899908, 1.553899908,
     3              0.990899908, 0.534899908, 0.186899908, -0.087100092, -0.353100092, -0.491100092,
     4             -0.837100092, -0.864100092 /
      Data Japan_Pac_c0 / 5.373125851, 8.772125851, 5.022125851, 5.066125851, 5.140125851, 5.317125851,
     1                    5.564125851, 5.843125851, 6.146125851, 6.346125851, 6.425125851, 6.288125851,
     2                    5.972125851, 5.582125851, 5.091125851, 4.680125851, 3.906125851, 3.481125851,
     3                    2.870125851, 2.507125851, 2.160125851, 1.969125851, 1.675125851, 1.601125851,
     4                    1.270125851, 1.364125851 /
      Data Japan_Phi_c0 / 4.309125851, 7.579125851, 3.901125851, 3.935125851, 4.094125851, 4.278125851,
     1                    4.531125851, 4.816125851, 5.126125851, 5.333125851, 5.420125851, 5.289125851,
     2                    4.979125851, 4.592125851, 4.089125851, 3.571125851, 2.844125851, 2.371125851,
     3                    1.779125851, 1.293125851, 0.895125851, 0.607125851, 0.303125851, 0.183125851,
     4                   -0.143874149, -0.195874149 /
      Data SAN_c0 / 5.064671414, 8.528671414, 4.673671414, 4.694671414, 4.779671414, 4.935671414,
     1              5.182671414, 5.457671414, 5.788671414, 5.998671414, 6.103671414, 6.013671414,
     2              5.849671414, 5.603671414, 5.151671414, 4.719671414, 3.995671414, 3.512671414,
     3              2.875671414, 2.327671414, 1.950671414, 1.766671414, 1.524671414, 1.483671414,
     4              1.175671414, 1.271671414 /
      Data SAS_c0 / 5.198671414, 8.679671414, 4.807671414, 4.827671414, 4.911671414, 5.066671414,
     1              5.312671414, 5.586671414, 5.917671414, 6.126671414, 6.230671414, 6.140671414,
     2              5.974671414, 5.728671414, 5.277671414, 4.848671414, 4.129671414, 3.653671414,
     3              3.023671414, 2.481671414, 2.111671414, 1.932671414, 1.698671414, 1.665671414,
     4              1.366671414, 1.462671414 /
      Data Taiwan_c0 / 3.032846279, 7.559846279, 2.636846279, 2.698846279, 2.800846279, 2.926846279,
     1                 3.069846279, 3.236846279, 3.446846279, 3.643846279, 3.798846279, 3.827846279,
     2                 3.765846279, 3.602846279, 3.343846279, 3.028846279, 2.499846279, 2.140846279,
     3                 1.645846279, 1.217846279, 0.871846279, 0.596846279, 0.268846279, 0.014846279,
     4                -0.446153721, -0.473153721 /

      data Global_c0s / 9.907, 13.194, 9.962, 10.099, 10.181, 10.311, 10.588, 10.824, 11.084,
     1                 11.232, 11.311, 11.055, 10.803, 10.669, 10.116, 9.579, 8.837, 8.067,
     2                  6.829, 5.871, 5.2, 4.83, 4.173, 3.833, 3.132, 2.72 /
      data Cascadia_c0s / 9.6, 12.874, 9.802, 9.933, 10.009, 10.133, 10.404, 10.634, 10.888, 11.03,
     1                   11.103, 10.841, 10.583, 10.443, 9.884, 9.341, 8.593, 7.817, 6.573, 5.609,
     2                    4.932, 4.556, 3.893, 3.547, 2.84, 2.422 /
      data Alaska_c0s / 9.404, 12.79, 9.451, 9.587, 9.667, 9.808, 10.086, 10.379, 10.65,
     1                 10.816, 10.883, 10.633, 10.322, 10.116, 9.561, 8.973, 8.246, 7.507,
     2                  6.213, 5.206, 4.594, 4.206, 3.517, 3.142, 2.391, 2.031 /
      data Aleutian_c0s / 9.912, 13.6, 9.954, 10.086, 10.172, 10.302, 10.602, 10.862, 11.184,
     1                   11.304, 11.402, 11.183, 10.965, 10.87, 10.411, 9.901, 9.335, 8.68,
     2                    7.581, 6.671, 6.047, 5.667, 4.97, 4.592, 3.65, 2.95 /
      data CAM_c0s / 9.58, 12.81, 9.612, 9.771, 9.85, 9.993, 10.317, 10.563, 10.785, 10.841,
     1              10.809, 10.519, 10.268, 10.134, 9.598, 9.097, 8.324, 7.557, 6.35, 5.434,
     2               4.773, 4.441, 3.849, 3.502, 2.821, 2.408 /
      data Japan_c0s / 10.145, 13.248, 10.162, 10.306, 10.387, 10.498, 10.744, 10.981, 11.25,
     1                 11.466, 11.619, 11.351, 11.063, 10.878, 10.296, 9.711, 8.934, 8.164,
     2                  6.896, 5.935, 5.234, 4.849, 4.074, 3.814, 3.152, 2.791 /
      data SAN_c0s / 9.254, 12.754, 9.293, 9.403, 9.481, 9.592, 9.834, 10.027, 10.265, 10.467,
     1              10.566, 10.33, 10.124, 10.077, 9.539, 9.03, 8.258, 7.467, 6.22, 5.261, 4.567,
     2               4.176, 3.495, 3.038, 2.368, 1.939 /
      data SAS_c0s / 9.991, 12.927, 9.994, 10.152, 10.292, 10.459, 10.818, 11.102, 11.424, 11.49,
     1              11.32, 10.927, 10.555, 10.328, 9.639, 9.03, 8.258, 7.417, 6.18, 5.161, 4.517,
     2               4.076, 3.445, 3.038, 2.368, 1.939 /
      data Taiwan_c0s / 10.071, 13.516, 10.174, 10.273, 10.329, 10.451, 10.678, 10.86, 11.093,
     1                  11.283, 11.503, 11.32, 11.147, 11.079, 10.547, 10.049, 9.327, 8.504,
     2                   7.204, 6.227, 5.517, 5.157, 4.55, 4.229, 3.554, 3.166 /

C     Geometrical Spreading
      data c1 / -1.662, -1.661, -1.587, -1.593, -1.607, -1.63, -1.657, -1.687, -1.715,
     1          -1.737, -1.745, -1.732, -1.696, -1.643, -1.58, -1.519, -1.44, -1.419, -1.4,
     2          -1.391, -1.394, -1.416, -1.452, -1.504, -1.569, -1.676 /
      data c1slab / -2.543, -2.422, -2.554, -2.566, -2.578, -2.594, -2.629, -2.649, -2.65,
     1              -2.647, -2.634, -2.583, -2.539, -2.528, -2.452, -2.384, -2.338, -2.267,
     2              -2.166, -2.077, -2.015, -2.012, -1.989, -1.998, -2.019, -2.047 /

C     Anelastic Attenuation
      data Global_a0 / -0.00657, -0.00395, -0.00657, -0.00657, -0.00657, -0.00657, -0.00657,
     1                 -0.00657, -0.00657, -0.00657, -0.00657, -0.00657, -0.00657, -0.00657,
     2                 -0.00657, -0.00657, -0.00635, -0.0058, -0.00505, -0.00429, -0.00369,
     3                 -0.00321, -0.00244, -0.0016, -0.000766, 0.0 /
      data Alaska_a0 / -0.00541, -0.00404, -0.00541, -0.00541, -0.00541, -0.00541, -0.00541,
     1                 -0.00541, -0.00541, -0.00541, -0.00541, -0.00541, -0.00541, -0.00541,
     2                 -0.00541, -0.00541, -0.00478, -0.00415, -0.00342, -0.0029, -0.0025,
     3                 -0.00217, -0.00165, -0.00125, -0.000519, 0.0 /
      data CAM_a0 / -0.00387, -0.00153, -0.00387, -0.00387, -0.00387, -0.00387, -0.00387,
     1              -0.00387, -0.00387, -0.00387, -0.00387, -0.00387, -0.00387, -0.00387,
     2              -0.00387, -0.00387, -0.00342, -0.00297, -0.00245, -0.00208, -0.00179,
     3              -0.00156, -0.00118, -0.000895, -0.000371, 0.0 /
      data Japan_a0 / -0.00862, -0.00239, -0.00862, -0.00862, -0.00862, -0.00862, -0.00862,
     1                -0.00862, -0.00862, -0.00862, -0.00862, -0.00862, -0.00862, -0.00862,
     2                -0.00862, -0.00862, -0.00763, -0.00663, -0.00546, -0.00463, -0.00399,
     3                -0.00347, -0.00264, -0.002, -0.000828, 0.0 /
      data SA_a0 / -0.00397, -0.000311, -0.00397, -0.00397, -0.00397, -0.00397, -0.00397,
     1             -0.00397, -0.00397, -0.00397, -0.00397, -0.00397, -0.00397, -0.00397,
     2             -0.00397, -0.00397, -0.00351, -0.00305, -0.00252, -0.00214, -0.00184,
     3             -0.0016, -0.00122, -0.000919, -0.000382, 0.0 /
      data Taiwan_a0 / -0.00787, -0.00514, -0.00787, -0.00787, -0.00787, -0.00787, -0.00787,
     1                 -0.00787, -0.00787, -0.00787, -0.00787, -0.00787, -0.00787, -0.00787,
     2                 -0.00787, -0.00787, -0.0068, -0.00605, -0.00498, -0.00423, -0.00364,
     3                 -0.00316, -0.00241, -0.00182, -0.000755, 0.0 /

      data Global_a0slab / -0.00255, -0.0019, -0.00255, -0.00255, -0.00255, -0.00255, -0.00255,
     1                     -0.00255, -0.00255, -0.00255, -0.00255, -0.00255, -0.00255, -0.00255,
     2                     -0.00255, -0.00255, -0.00211, -0.00187, -0.00154, -0.00131, -0.00113,
     3                     -0.000979, -0.000745, -0.000564, -0.000234, 0.0 /
      data Alaska_a0slab / -0.00227, -0.00238, -0.00219, -0.00219, -0.00219, -0.00219, -0.00219,
     1                     -0.00219, -0.00219, -0.00219, -0.00219, -0.00219, -0.00219, -0.00219,
     2                     -0.00219, -0.00219, -0.00189, -0.00168, -0.00139, -0.00118, -0.00101,
     3                     -0.00088, -0.00067, -0.000507, -0.00021, 0.0 /
      data Cascadia_a0slab / -0.00354, -0.00109, -0.00401, -0.00401, -0.00401, -0.00401, -0.00401,
     1                       -0.00401, -0.00401, -0.00401, -0.00401, -0.00401, -0.00401, -0.00401,
     2                       -0.00401, -0.00401, -0.00347, -0.00309, -0.00254, -0.00216, -0.00186,
     3                       -0.00161, -0.00123, -0.000929, -0.000385, 0.0 /
      data CAM_a0slab / -0.00238, -0.00192, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217,
     1                  -0.00217, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217,
     2                  -0.00217, -0.00217, -0.00188, -0.00167, -0.00138, -0.00117, -0.00101,
     3                  -0.000873, -0.000664, -0.000503, -0.000209, 0.0 /
      data Japan_a0slab / -0.00335, -0.00215, -0.00311, -0.00311, -0.00311, -0.00311, -0.00311,
     1                    -0.00311, -0.00311, -0.00311, -0.00311, -0.00311, -0.00311, -0.00311,
     2                    -0.00311, -0.00311, -0.00269, -0.00239, -0.00197, -0.00167, -0.00144,
     3                    -0.00125, -0.000952, -0.00072, -0.000299, 0.0 /
      data SA_a0slab / -0.00238, -0.00192, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217,
     1                 -0.00217, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217, -0.00217,
     2                 -0.00217, -0.00217, -0.00188, -0.00167, -0.00138, -0.00117, -0.00101,
     3                 -0.000873, -0.000664, -0.000503, -0.000209, 0.0 /
      data Taiwan_a0slab / -0.00362, -0.00366, -0.00355, -0.00355, -0.00355, -0.00355, -0.00355,
     1                     -0.00355, -0.00355, -0.00355, -0.00355, -0.00355, -0.00355, -0.00355,
     2                     -0.00355, -0.00355, -0.00307, -0.00273, -0.00225, -0.00191, -0.00164,
     3                     -0.00143, -0.00109, -0.000822, -0.000341, 0.0 /

C     Source Scaling
      data c4 / 1.246, 1.336, 1.246, 1.227, 1.221, 1.215, 1.207, 1.201, 1.19, 1.182, 1.171,
     1          1.163, 1.156, 1.151, 1.143, 1.143, 1.217, 1.27, 1.344, 1.396, 1.437, 1.47,
     2          1.523, 1.564, 1.638, 1.69 /
      data c5 / -0.021, -0.039, -0.021, -0.021, -0.021, -0.021, -0.021, -0.021, -0.021,
     1          -0.021, -0.021, -0.021, -0.021, -0.021, -0.022, -0.023, -0.026, -0.028,
     2          -0.031, -0.034, -0.036, -0.038, -0.044, -0.048, -0.059, -0.067 /

      data c6 / 1.128, 1.336, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.162, 1.163,
     1          1.156, 1.151, 1.143, 1.143, 1.217, 1.240, 1.237, 1.232, 1.227, 1.223, 1.216, 1.21,
     2          1.2, 1.194 /
c      data c6 /1.128, 1.844, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.128, 1.162, 1.187,
c     1         1.204, 1.215, 1.227, 1.234, 1.24, 1.24, 1.237, 1.232, 1.227, 1.223, 1.216, 1.21,
c     2         1.2, 1.194 /

      data c4slab / 1.84, 1.84, 1.84, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884,
     1              1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.884, 1.949,
     2              2.031, 2.131, 2.185, 2.35 /
      data c5slab / -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.05, -0.06,
     1              -0.068, -0.075, -0.082, -0.091, -0.1, -0.115, -0.134, -0.154, -0.154, -0.154,
     2              -0.154, -0.154, -0.154, -0.154, -0.154 /
      data c6slab / 0.4, 0.8, 0.4, 0.415, 0.43, 0.445, 0.46, 0.475, 0.49, 0.505, 0.52, 0.535, 0.55,
     1              0.565, 0.58, 0.595, 0.61, 0.625, 0.64, 0.655, 0.67, 0.685, 0.7, 0.715, 0.73, 0.745 /

      data d / 0.3004, 0.2693, 0.2839, 0.2854, 0.2891, 0.2932, 0.3004, 0.3048, 0.2992, 0.2854, 0.2814,
     1         0.291, 0.2758, 0.2719, 0.2539, 0.2482, 0.2227, 0.1969, 0.1452, 0.06, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data m / 0.0314, 0.0252, 0.0296, 0.0298, 0.0302, 0.0306, 0.0313, 0.0316, 0.0321, 0.032, 0.0325, 0.0306,
     1         0.0306, 0.0323, 0.0302, 0.0295, 0.0266, 0.0231, 0.0118, 0.007, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data db / 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0,
     1          67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 67.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /

C     Linear Site Amplification, Fs
      data Vc / 1350.0, 850.0, 1300.0, 1225.0, 1200.0, 1200.0, 1200.0, 1225.0, 1350.0, 1450.0,
     1          1500.0, 1425.0, 1350.0, 1250.0, 1150.0, 1025.0, 900.0, 800.0, 760.0,
     2          760.0, 760.0, 760.0, 760.0, 760.0, 760.0, 760.0  /

      data Japan_c1 / -0.586, -0.738, -0.604, -0.593, -0.569, -0.539, -0.468, -0.403, -0.325,
     1                -0.264, -0.25, -0.288, -0.36, -0.455, -0.617, -0.757, -0.966, -0.986,
     2                -0.966, -0.901, -0.822, -0.751, -0.68, -0.592, -0.494, -0.395 /
      data Taiwan_c1 / -0.44, -0.454, -0.44, -0.458, -0.454, -0.455, -0.453, -0.452, -0.456, -0.468,
     1                 -0.484, -0.498, -0.511, -0.514, -0.51, -0.506, -0.5, -0.49, -0.486, -0.475,
     2                 -0.453, -0.428, -0.396, -0.353, -0.311, -0.261 /
      data Global_c / -0.498, -0.601, -0.498, -0.478, -0.464, -0.446, -0.431, -0.42, -0.442,
     1                -0.485, -0.546, -0.612, -0.688, -0.748, -0.802, -0.845, -0.911, -0.926,
     2                -0.888, -0.808, -0.743, -0.669, -0.585, -0.506, -0.418, -0.321 /
      data Alaska_c / -0.785, -1.031, -0.803, -0.785, -0.745, -0.69, -0.636, -0.594, -0.586,
     1                -0.629, -0.729, -0.867, -1.011, -1.133, -1.238, -1.321, -1.383, -1.414,
     2                -1.43, -1.421, -1.391, -1.343, -1.297, -1.233, -1.147, -1.06 /
      data Cascadia_c / -0.572, -0.671, -0.571, -0.575, -0.573, -0.565, -0.546, -0.519, -0.497,
     1                  -0.486, -0.499, -0.533, -0.592, -0.681, -0.772, -0.838, -0.922, -0.932,
     2                  -0.814, -0.725, -0.632, -0.57, -0.489, -0.421, -0.357, -0.302 /
      data Japan_c / -0.586, -0.738, -0.604, -0.593, -0.579, -0.561, -0.508, -0.461, -0.452,
     1               -0.498, -0.568, -0.667, -0.781, -0.867, -0.947, -1.003, -1.052, -1.028,
     2               -0.971, -0.901, -0.822, -0.751, -0.68, -0.592, -0.52, -0.395 /
      data SA_c / -0.333, -0.681, -0.333, -0.345, -0.362, -0.38, -0.403, -0.427, -0.458, -0.49,
     1            -0.536, -0.584, -0.654, -0.725, -0.801, -0.863, -0.942, -0.96, -0.942, -0.891,
     2            -0.842, -0.787, -0.706, -0.621, -0.52, -0.42 /
      data Taiwan_c / -0.44, -0.59, -0.44, -0.458, -0.459, -0.464, -0.466, -0.468, -0.473, -0.482,
     1                -0.499, -0.522, -0.555, -0.596, -0.643, -0.689, -0.745, -0.777, -0.79,
     2                -0.765, -0.724, -0.675, -0.613, -0.536, -0.444, -0.352 /

C     Nonlinear Site Amplification, Fnl
      data f4 / -0.44169, -0.31763, -0.4859, -0.4859, -0.4859, -0.4908, -0.49569, -0.49823,
     1          -0.49724, -0.49471, -0.48583, -0.47383, -0.47696, -0.4845, -0.48105, -0.46492,
     2          -0.43439, -0.38484, -0.31318, -0.25, -0.19, -0.14, -0.07, -0.03,
     3          -0.005, 0.000 /
      data f5 / -0.0052, -0.0052, -0.0052, -0.00518, -0.00515, -0.00511, -0.00505, -0.00497,
     1          -0.00489, -0.00478, -0.0046, -0.00434, -0.00402, -0.0037, -0.00342, -0.00322,
     2          -0.00312, -0.0031, -0.0031, -0.0031, -0.0031, -0.0031, -0.0031, -0.0031,
     3          -0.0031, -0.0031 /

C     Basin Terms
      data Japan_e1 / 0.0, -0.137, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.05, 0.1, 0.164, 0.164,
     1                0.08, 0.0, -0.13, -0.2, -0.401, -0.488, -0.578, -0.645, -0.678, -0.772,
     2               -0.699, -0.642, -0.524, -0.327 /
      data Japan_e2 / 0.0, 0.137, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, -0.043, -0.085, -0.139, -0.139,
     1               -0.08, 0.0, 0.113, 0.176, 0.284, 0.346, 0.48, 0.579, 0.609, 0.635, 0.709,
     2                0.63, 0.306, 0.182 /
      data Japan_e3 / 1.0, 0.091, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, -0.025, -0.05, -0.082, -0.082,
     1               -0.053, 1.0, 0.087, 0.118, 0.167, 0.203, 0.24, 0.254, 0.267, 0.265, 0.259,
     2                0.215, 0.175, 0.121 /
      data Cas_e1 / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.1, 0.3, 0.333, 0.29, 0.177, 0.1, 0.0,
     1              0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /
      data Cas_e2 / 0.0, 0.115, 0, 0, 0, 0, 0, -0.1, -0.34, -0.377, -0.29, -0.192, -0.035, 0.0,
     1              0.05, 0.1, 0.2, 0.245, 0.32, 0.37, 0.4, 0.43, 0.44, 0.45, 0.406, 0.345 /
      data Cas_e3 / 1.0, 0.068, 1.0, 1.0, 1.0, 1.0, 1.0, -0.063, -0.2, -0.222, -0.193, -0.148,
     1             -0.054, 1.0, 0.2, 0.2, 0.125, 0.153, 0.2, 0.239, 0.264, 0.287, 0.303,
     2              0.321, 0.312, 0.265 /
      data BasinD / 0.0, -0.115, 0.0, 0.0, 0.0, 0.0, 0.0, -0.05, -0.075, -0.081, -0.091,
     1             -0.092, 0.0, 0.0, 0.0, 0.0, -0.2, -0.245, -0.32, -0.28, -0.313, -0.355,
     2             -0.417, -0.45, -0.35, -0.331 /
      data SeaBasinD / 0.0, 0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.078, 0.075, 0.064, 0.075, 0.0,
     1                 0.0, 0.0, 0.0, 0.012, 0.037, 0.064, 0.14, 0.19, 0.165, 0.163, 0.132,
     2                 0.15, 0.117 /

C     Sigma Model
      data sigtau / 0.48, 0.477, 0.476, 0.482, 0.49, 0.5, 0.515, 0.528, 0.53, 0.524, 0.51,
     1              0.501, 0.492, 0.492, 0.492, 0.492, 0.492, 0.492, 0.492, 0.492, 0.492,
     2              0.492, 0.492, 0.492, 0.492, 0.492 /
      data sigphi1 / 0.396, 0.348, 0.397, 0.401, 0.405, 0.413, 0.439, 0.473, 0.529, 0.517, 0.457,
     1               0.432, 0.45, 0.436, 0.433, 0.428, 0.448, 0.43, 0.406, 0.393, 0.381, 0.367,
     2               0.33, 0.298, 0.254, 0.231 /
      data sigphi2 / 0.565, 0.288, 0.56, 0.563, 0.575, 0.589, 0.616, 0.653, 0.722, 0.712,
     1               0.644, 0.64, 0.633, 0.584, 0.556, 0.51, 0.471, 0.43, 0.406, 0.393, 0.381,
     2               0.367, 0.33, 0.298, 0.254, 0.231 /
      data sigphiv / -0.18, -0.179, -0.18, -0.181, -0.183, -0.188, -0.205, -0.23, -0.262,
     1               -0.239, -0.185, -0.138, -0.185, -0.158, -0.19, -0.186, -0.177, -0.166,
     2               -0.111, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 /

C     Constant parameters
      b4 = 0.1
      f1 = 0.0
      f3 = 0.05
      Vb = 200.0
      Vref = 760.0
      db1 = 20.0
      V1 = 270.0

C Find the requested spectral period and corresponding coefficients
      nPer = 26

C First check for the PGA case
      if (specT .eq. 0.0) then
         i1=1
         period2 = period(i1)
         c1T = c1(i1)
         c1slabT = c1slab(i1)
         c4T = c4(i1)
         c5T = c5(i1)
         c6T = c6(i1)
         c4slabT = c4slab(i1)
         c5slabT = c5slab(i1)
         c6slabT = c6slab(i1)
         f4T = f4(i1)
         f5T = f5(i1)
         VcT = Vc(i1)
         dT = d(i1)
         mT = m(i1)
         dbT = db(i1)

         global_a0T = Global_a0(i1)
         Alaska_a0T = Alaska_a0(i1)
         CAM_a0T = CAM_a0(i1)
         Japan_a0T = Japan_a0(i1)
         SA_a0T = SA_a0(i1)
         Taiwan_a0T = Taiwan_a0(i1)

         global_a0slabT = Global_a0slab(i1)
         Alaska_a0slabT = Alaska_a0slab(i1)
         Cascadia_a0slabT = Cascadia_a0slab(i1)
         CAM_a0slabT = CAM_a0slab(i1)
         Japan_a0slabT = Japan_a0slab(i1)
         SA_a0slabT = SA_a0slab(i1)
         Taiwan_a0slabT = Taiwan_a0slab(i1)

         global_c0T = Global_c0(i1)
         Alaska_c0T = Alaska_c0(i1)
         Aleutian_c0T = Aleutian_c0(i1)
         Cascadia_c0T = Cascadia_c0(i1)
         CAM_c0T = CAM_c0(i1)
         Japan_Pac_c0T = Japan_Pac_c0(i1)
         Japan_Phi_c0T = Japan_Phi_c0(i1)
         SAN_c0T = SAN_c0(i1)
         SAS_c0T = SAS_c0(i1)
         Taiwan_c0T = Taiwan_c0(i1)

         global_c0sT = Global_c0s(i1)
         Cascadia_c0sT = Cascadia_c0s(i1)
         Alaska_c0sT = Alaska_c0s(i1)
         Aleutian_c0sT = Aleutian_c0s(i1)
         CAM_c0sT = CAM_c0s(i1)
         Japan_c0sT = Japan_c0s(i1)
         SAN_c0sT = SAN_c0s(i1)
         SAS_c0sT = SAS_c0s(i1)
         Taiwan_c0sT = Taiwan_c0s(i1)

         global_cT = Global_c(i1)
         Alaska_cT = Alaska_c(i1)
         Cascadia_cT = Cascadia_c(i1)
         Japan_cT = Japan_c(i1)
         Japan_c1T = Japan_c1(i1)
         SA_cT = SA_c(i1)
         Taiwan_cT = Taiwan_c(i1)
         Taiwan_c1T = Taiwan_c1(i1)

         Japan_e1T = Japan_e1(i1)
         Japan_e2T = Japan_e2(i1)
         Japan_e3T = Japan_e3(i1)
         Cas_e1T = Cas_e1(i1)
         Cas_e2T = Cas_e2(i1)
         Cas_e3T = Cas_e3(i1)
         SeaBasinDT = SeaBasinD(i1)
         BasinDT = BasinD(i1)

         sigPhi1T = sigphi1(i1)
         sigPhi2T = sigphi2(i1)
         sigPhivT = sigphiv(i1)
         sigtauT = sigtau(i1)

         goto 1011

      elseif (specT .eq. -1.0) then
         i1=2
         period2 = period(i1)
         c1T = c1(i1)
         c1SlabT = c1Slab(i1)
         c4T = c4(i1)
         c5T = c5(i1)
         c6T = c6(i1)
         c4slabT = c4slab(i1)
         c5slabT = c5slab(i1)
         c6slabT = c6slab(i1)
         f4T = f4(i1)
         f5T = f5(i1)
         VcT = Vc(i1)
         dT = d(i1)
         mT = m(i1)
         dbT = db(i1)

         global_a0T = Global_a0(i1)
         Alaska_a0T = Alaska_a0(i1)
         CAM_a0T = CAM_a0(i1)
         Japan_a0T = Japan_a0(i1)
         SA_a0T = SA_a0(i1)
         Taiwan_a0T = Taiwan_a0(i1)

         global_a0slabT = Global_a0slab(i1)
         Alaska_a0slabT = Alaska_a0slab(i1)
         Cascadia_a0slabT = Cascadia_a0slab(i1)
         CAM_a0slabT = CAM_a0slab(i1)
         Japan_a0slabT = Japan_a0slab(i1)
         SA_a0slabT = SA_a0slab(i1)
         Taiwan_a0slabT = Taiwan_a0slab(i1)

         global_c0T = Global_c0(i1)
         Alaska_c0T = Alaska_c0(i1)
         Aleutian_c0T = Aleutian_c0(i1)
         Cascadia_c0T = Cascadia_c0(i1)
         CAM_c0T = CAM_c0(i1)
         Japan_Pac_c0T = Japan_Pac_c0(i1)
         Japan_Phi_c0T = Japan_Phi_c0(i1)
         SAN_c0T = SAN_c0(i1)
         SAS_c0T = SAN_c0(i1)
         Taiwan_c0T = Taiwan_c0(i1)

         global_c0sT = Global_c0s(i1)
         Cascadia_c0sT = Cascadia_c0s(i1)
         Alaska_c0sT = Alaska_c0s(i1)
         Aleutian_c0sT = Aleutian_c0s(i1)
         CAM_c0sT = CAM_c0s(i1)
         Japan_c0sT = Japan_c0s(i1)
         SAN_c0sT = SAN_c0s(i1)
         SAS_c0sT = SAS_c0s(i1)
         Taiwan_c0sT = Taiwan_c0s(i1)

         global_cT = Global_c(i1)
         Alaska_cT = Alaska_c(i1)
         Cascadia_cT = Cascadia_c(i1)
         Japan_cT = Japan_c(i1)
         Japan_c1T = Japan_c1(i1)
         SA_cT = SA_c(i1)
         Taiwan_cT = Taiwan_c(i1)
         Taiwan_c1T = Taiwan_c1(i1)

         Japan_e1T = Japan_e1(i1)
         Japan_e2T = Japan_e2(i1)
         Japan_e3T = Japan_e3(i1)
         Cas_e1T = Cas_e1(i1)
         Cas_e2T = Cas_e2(i1)
         Cas_e3T = Cas_e3(i1)
         SeaBasinDT = SeaBasinD(i1)
         BasinDT = BasinD(i1)

         sigPhi1T = sigphi1(i1)
         sigPhi2T = sigphi2(i1)
         sigPhivT = sigphiv(i1)
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
      write (*,*) 'PSHAB (2020) Horizontal'
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
 1020 call S24_interp (period(count1),period(count2),c1(count1),c1(count2),
     +                   specT,c1T,iflag)
      call S24_interp (period(count1),period(count2),c1slab(count1),c1slab(count2),
     +                   specT,c1slabT,iflag)
      call S24_interp (period(count1),period(count2),c4(count1),c4(count2),
     +                   specT,c4T,iflag)
      call S24_interp (period(count1),period(count2),c5(count1),c5(count2),
     +                   specT,c5T,iflag)
      call S24_interp (period(count1),period(count2),c6(count1),c6(count2),
     +                   specT,c6T,iflag)
      call S24_interp (period(count1),period(count2),c4slab(count1),c4slab(count2),
     +                   specT,c4slabT,iflag)
      call S24_interp (period(count1),period(count2),c5slab(count1),c5slab(count2),
     +                   specT,c5slabT,iflag)
      call S24_interp (period(count1),period(count2),c6slab(count1),c6slab(count2),
     +                   specT,c6slabT,iflag)
      call S24_interp (period(count1),period(count2),f4(count1),f4(count2),
     +                   specT,f4T,iflag)
      call S24_interp (period(count1),period(count2),f5(count1),f5(count2),
     +                   specT,f5T,iflag)
      call S24_interp (period(count1),period(count2),Vc(count1),Vc(count2),
     +                   specT,VcT,iflag)
      call S24_interp (period(count1),period(count2),Global_a0(count1),Global_a0(count2),
     +                   specT,Global_a0T,iflag)
      call S24_interp (period(count1),period(count2),Alaska_a0(count1),Alaska_a0(count2),
     +                   specT,Alaska_a0T,iflag)
      call S24_interp (period(count1),period(count2),CAM_a0(count1),CAM_a0(count2),
     +                   specT,CAM_a0T,iflag)
      call S24_interp (period(count1),period(count2),Japan_a0(count1),Japan_a0(count2),
     +                   specT,Japan_a0T,iflag)
      call S24_interp (period(count1),period(count2),SA_a0(count1),SA_a0(count2),
     +                   specT,SA_a0T,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_a0(count1),Taiwan_a0(count2),
     +                   specT,Taiwan_a0T,iflag)
      call S24_interp (period(count1),period(count2),Global_a0slab(count1),Global_a0slab(count2),
     +                   specT,Global_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),Alaska_a0slab(count1),Alaska_a0slab(count2),
     +                   specT,Alaska_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),Cascadia_a0slab(count1),Cascadia_a0slab(count2),
     +                   specT,Cascadia_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),CAM_a0slab(count1),CAM_a0slab(count2),
     +                   specT,CAM_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),Japan_a0slab(count1),Japan_a0slab(count2),
     +                   specT,Japan_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),SA_a0slab(count1),SA_a0slab(count2),
     +                   specT,SA_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_a0slab(count1),Taiwan_a0slab(count2),
     +                   specT,Taiwan_a0slabT,iflag)
      call S24_interp (period(count1),period(count2),Global_c0(count1),Global_c0(count2),
     +                   specT,Global_c0T,iflag)
      call S24_interp (period(count1),period(count2),Alaska_c0(count1),Alaska_c0(count2),
     +                   specT,Alaska_c0T,iflag)
      call S24_interp (period(count1),period(count2),Aleutian_c0(count1),Aleutian_c0(count2),
     +                   specT,Aleutian_c0T,iflag)
      call S24_interp (period(count1),period(count2),Cascadia_c0(count1),Cascadia_c0(count2),
     +                   specT,Cascadia_c0T,iflag)
      call S24_interp (period(count1),period(count2),CAM_c0(count1),CAM_c0(count2),
     +                   specT,CAM_c0T,iflag)
      call S24_interp (period(count1),period(count2),Japan_Pac_c0(count1),Japan_Pac_c0(count2),
     +                   specT,Japan_Pac_c0T,iflag)
      call S24_interp (period(count1),period(count2),Japan_Phi_c0(count1),Japan_Phi_c0(count2),
     +                   specT,Japan_Phi_c0T,iflag)
      call S24_interp (period(count1),period(count2),SAN_c0(count1),SAN_c0(count2),
     +                   specT,SAN_c0T,iflag)
      call S24_interp (period(count1),period(count2),SAS_c0(count1),SAS_c0(count2),
     +                   specT,SAS_c0T,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_c0(count1),Taiwan_c0(count2),
     +                   specT,Taiwan_c0T,iflag)
      call S24_interp (period(count1),period(count2),Global_c0s(count1),Global_c0s(count2),
     +                   specT,Global_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Cascadia_c0s(count1),Cascadia_c0s(count2),
     +                   specT,Cascadia_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Alaska_c0s(count1),Alaska_c0s(count2),
     +                   specT,Alaska_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Aleutian_c0s(count1),Aleutian_c0s(count2),
     +                   specT,Aleutian_c0sT,iflag)
      call S24_interp (period(count1),period(count2),CAM_c0s(count1),CAM_c0s(count2),
     +                   specT,CAM_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Japan_c0s(count1),Japan_c0s(count2),
     +                   specT,Japan_c0sT,iflag)
      call S24_interp (period(count1),period(count2),SAN_c0s(count1),SAN_c0s(count2),
     +                   specT,SAN_c0sT,iflag)
      call S24_interp (period(count1),period(count2),SAS_c0s(count1),SAS_c0s(count2),
     +                   specT,SAS_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_c0s(count1),Taiwan_c0s(count2),
     +                   specT,Taiwan_c0sT,iflag)
      call S24_interp (period(count1),period(count2),Global_c(count1),Global_c(count2),
     +                   specT,Global_cT,iflag)
      call S24_interp (period(count1),period(count2),Alaska_c(count1),Alaska_c(count2),
     +                   specT,Alaska_cT,iflag)
      call S24_interp (period(count1),period(count2),Cascadia_c(count1),Cascadia_c(count2),
     +                   specT,Cascadia_cT,iflag)
      call S24_interp (period(count1),period(count2),Japan_c(count1),Japan_c(count2),
     +                   specT,Japan_cT,iflag)
      call S24_interp (period(count1),period(count2),Japan_c1(count1),Japan_c1(count2),
     +                   specT,Japan_c1T,iflag)
      call S24_interp (period(count1),period(count2),SA_c(count1),SA_c(count2),
     +                   specT,SA_cT,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_c(count1),Taiwan_c(count2),
     +                   specT,Taiwan_cT,iflag)
      call S24_interp (period(count1),period(count2),Taiwan_c1(count1),Taiwan_c1(count2),
     +                   specT,Taiwan_c1T,iflag)
      call S24_interp (period(count1),period(count2),d(count1),d(count2),
     +                   specT,dT,iflag)
      call S24_interp (period(count1),period(count2),m(count1),m(count2),
     +                   specT,mT,iflag)
      call S24_interp (period(count1),period(count2),db(count1),db(count2),
     +                   specT,dbT,iflag)
      call S24_interp (period(count1),period(count2),Japan_e1(count1),japan_e1(count2),
     +                   specT,Japan_e1T,iflag)
      call S24_interp (period(count1),period(count2),Japan_e2(count1),japan_e2(count2),
     +                   specT,Japan_e2T,iflag)
      call S24_interp (period(count1),period(count2),Japan_e3(count1),japan_e3(count2),
     +                   specT,Japan_e3T,iflag)
      call S24_interp (period(count1),period(count2),Cas_e1(count1),Cas_e1(count2),
     +                   specT,Cas_e1T,iflag)
      call S24_interp (period(count1),period(count2),Cas_e2(count1),Cas_e2(count2),
     +                   specT,Cas_e2T,iflag)
      call S24_interp (period(count1),period(count2),CAS_e3(count1),Cas_e3(count2),
     +                   specT,Cas_e3T,iflag)
      call S24_interp (period(count1),period(count2),SeaBasinD(count1),SeaBasinD(count2),
     +                   specT,SeaBasinDT,iflag)
      call S24_interp (period(count1),period(count2),BasinD(count1),BasinD(count2),
     +                   specT,BasinDT,iflag)
      call S24_interp (period(count1),period(count2),sigPhi1(count1),sigPhi1(count2),
     +                   specT,sigPhi1T,iflag)
      call S24_interp (period(count1),period(count2),sigPhi2(count1),sigPhi2(count2),
     +                   specT,sigPhi2T,iflag)
      call S24_interp (period(count1),period(count2),sigPhiv(count1),sigPhiv(count2),
     +                   specT,sigPhivT,iflag)
      call S24_interp (period(count1),period(count2),sigtau(count1),sigtau(count2),
     +                   specT,sigtauT,iflag)

 1011 period2 = specT

C     Set the regional terms
c     0=Global
c     1=Alaska
c     2=Aleutian (a0=Alaska)
c     3=Cascadia (a0=Global(Interface))
c     4=Central America / Mexico (Global Linear site term)
c     5=Japan - Pacific Plate
c     6=Japan - Philippine Plate
c     7=Northern South America
c     8=Southern South America
c     9=Taiwan

      if (region .eq. 0) then
         if (ftype .eq. 0.0) then
            c0T = Global_c0T
            c0Pga = Global_c0(1)
            a0T = Global_a0T
            a0Pga = Global_a0(1)
         else
            c0T = Global_c0sT
            c0Pga = Global_c0s(1)
            a0T = Global_a0slabT
            a0Pga = Global_a0slab(1)
         endif
         cT = Global_cT
      elseif (region .eq. 1) then
         if (ftype .eq. 0.0) then
            c0T = Alaska_c0T
            c0Pga = Alaska_c0(1)
            a0T = Alaska_a0T
            a0Pga = Alaska_a0(1)
         else
            c0T = Alaska_c0sT
            c0Pga = Alaska_c0s(1)
            a0T = Alaska_a0slabT
            a0Pga = Alaska_a0slab(1)
         endif
         cT = Alaska_cT
      elseif (region .eq. 2) then
         if (ftype .eq. 0.0) then
            c0T = Aleutian_c0T
            c0Pga = Aleutian_c0(1)
            a0T = Alaska_a0T
            a0Pga = Alaska_a0(1)
         else
            c0T = Aleutian_c0sT
            c0Pga = Aleutian_c0s(1)
            a0T = Alaska_a0slabT
            a0Pga = Alaska_a0slab(1)
         endif
         cT = Alaska_cT
      elseif (region .eq. 3) then
         if (ftype .eq. 0.0) then
            c0T = Cascadia_c0T
            c0Pga = Cascadia_c0(1)
            a0T = Global_a0T
            a0Pga = Global_a0(1)
         else
            c0T = Cascadia_c0sT
            c0Pga = Cascadia_c0s(1)
            a0T = Cascadia_a0SlabT
            a0Pga = Cascadia_a0Slab(1)
         endif
         cT = Cascadia_cT
      elseif (region .eq. 4) then
         if (ftype .eq. 0.0) then
            c0T = CAM_c0T
            c0Pga = CAM_c0(1)
            a0T = CAM_a0T
            a0Pga = CAM_a0(1)
         else
            c0T = CAM_c0sT
            c0Pga = CAM_c0s(1)
            a0T = CAM_a0slabT
            a0Pga = CAM_a0slab(1)
         endif
         cT = Global_cT
      elseif (region .eq. 5) then
         if (ftype .eq. 0.0) then
            c0T = Japan_Pac_c0T
            c0Pga = Japan_Pac_c0(1)
            a0T = Japan_a0T
            a0Pga = Japan_a0(1)
         else
            c0T = Japan_c0sT
            c0Pga = Japan_c0s(1)
            a0T = Japan_a0slabT
            a0Pga = Japan_a0slab(1)
         endif
         cT = Japan_cT
      elseif (region .eq. 6) then
         if (ftype .eq. 0.0) then
            c0T = Japan_Phi_c0T
            c0Pga = Japan_Phi_c0(1)
            a0T = Japan_a0T
            a0Pga = Japan_a0(1)
         else
            c0T = Japan_c0sT
            c0Pga = Japan_c0s(1)
            a0T = Japan_a0slabT
            a0Pga = Japan_a0slab(1)
         endif
         cT = Japan_cT
      elseif (region .eq. 7) then
         if (ftype .eq. 0.0) then
            c0T = SAN_c0T
            c0Pga = SAN_c0(1)
            a0T = SA_a0T
            a0Pga = SA_a0(1)
         else
            c0T = SAN_c0sT
            c0Pga = SAN_c0s(1)
            a0T = SA_a0slabT
            a0Pga = SA_a0slab(1)
         endif
         cT = SA_cT
      elseif (region .eq. 8) then
         if (ftype .eq. 0.0) then
            c0T = SAS_c0T
            c0Pga = SAS_c0(1)
            a0T = SA_a0T
            a0Pga = SA_a0(1)
         else
            c0T = SAS_c0sT
            c0Pga = SAS_c0s(1)
            a0T = SA_a0slabT
            a0Pga = SA_a0slab(1)
         endif
         cT = SA_cT
      elseif (region .eq. 9) then
         if (ftype .eq. 0.0) then
            c0T = Taiwan_c0T
            c0Pga = Taiwan_c0(1)
            a0T = Taiwan_a0T
            a0Pga = Taiwan_a0(1)
         else
            c0T = Taiwan_c0sT
            c0Pga = Taiwan_c0s(1)
            a0T = Taiwan_a0slabT
            a0Pga = Taiwan_a0slab(1)
         endif
         cT = Taiwan_cT
      endif

      if (ftype .eq. 0.0) then
         h = 10.0**(-0.82+0.252*mag)
      else
C     New Slab Model for H
          if (mag .ge. mbslab) then
             h = 35.0
          else
             h = 10.0**((1.05/(mbslab-4.0))*(mag-mbslab) + 1.544)
          endif
      endif
      R = sqrt(Rrup*Rrup + h*h)
      Rref = sqrt(1.0+h*h)

C     Compute Fp term
      if (Ftype .eq. 0.0) then
         Fp = c1T*alog(R) + b4*mag*alog(R/Rref) + a0T*R
         FpPga = c1(1)*alog(R) + b4*mag*alog(R/Rref) + a0Pga*R
      else
         Fp = c1slabT*alog(R) + b4*mag*alog(R/Rref) + a0T*R
         FpPga = c1slab(1)*alog(R) + b4*mag*alog(R/Rref) + a0Pga*R
      endif

C     Compute the Fm term
      if (ftype .eq. 0.0) then
         if (mag .le. mbinter) then
            Fm = c4T*(mag-mbinter) + c5T*(mag-mbinter)**2.0
            FmPga = c4(1)*(mag-mbinter) + c5(1)*(mag-mbinter)**2.0
         else
            Fm = c6T*(mag-mbinter)
            FmPga = c6(1)*(mag-mbinter)
         endif
      else
         if (mag .le. mbslab) then
            Fm = c4slabT*(mag-mbslab) + c5slabT*(mag-mbslab)**2.0
            FmPga = c4slab(1)*(mag-mbslab) + c5slab(1)*(mag-mbslab)**2.0
         else
            Fm = c6slabT*(mag-mbslab)
            FmPga = c6slab(1)*(mag-mbslab)
         endif
      endif

C     Compute the Fd term
      if (Ftype .eq. 0.0) then
         Fd = 0.0
         FdPga = 0.0
      else
         if (depth .le. 20.0) then
            Fd = mT*(20.0 - dbT) + dT
            FdPga = m(1)*(20.0 - db(1)) + d(1)
         elseif (depth .gt. dbT) then
            Fd = dT
            FdPga = d(1)
         else
            Fd = mT*(depth-dbT) + dT
            FdPga = m(1)*(depth-db(1)) + d(1)
         endif
      endif

C     Compute PGARock

      pgarock = exp(c0Pga + FpPga + FmPga + FdPga)

C     Now compute the ground motion for given Vs30 value.

C     Compute the linear Flin term.
      if (Vs30 .le. V1) then
         if (region .eq. 5)then
            Flin = Japan_c1T*alog(Vs30/V1) + cT*alog(V1/Vref)
         elseif (region .eq. 8) then
            Flin = Taiwan_c1T*alog(Vs30/V1) + cT*alog(V1/Vref)
         else
            Flin = cT*alog(Vs30/V1) + cT*alog(V1/Vref)
         endif
      elseif (Vs30 .gt. VcT) then
         Flin = cT*alog(VcT/Vref)
      else
         Flin = cT*alog(Vs30/Vref)
      endif

C     Now Compute the Non-linear term (Fnl)
      f2 = f4T*(exp(f5T*(min(vs30,760.0)-200)) - exp(f5T*(760-200)))
c      if (specT .ge. 3.0) then
c         Fnl = 0.0
c      else
         Fnl = f1 + f2*alog((pgarock+f3)/f3)
c      endif

      Fs = Flin + Fnl

C     Compute the basin term
C     Cascadia Basin Terms
c      z25 = z25
      if (region .eq. 3) then
          x = (alog10(vs30) - alog10(500.0) ) / (0.42*sqrt(2.0))
          muz25 = 10**(3.75-0.74*(1+erf(x)))
          deltaz25 = alog(z25*1000.0) - alog(muz25)
C     Adjust the e3 term based on the PNW Basin Flag
C     Note values of 0 and 1 are for previous models not currently recommended.
          if (pnwbflag .eq. 0) then
             cas_e3Tbasin = cas_e3T + basinDT
             cas_e2Tbasin = cas_e2T + basinDT
          elseif (pnwbflag .eq. 1) then
             cas_e3Tbasin = cas_e3T + SeabasinDT
             cas_e2Tbasin = cas_e2T + SeabasinDT
          else
             cas_e3Tbasin = cas_e3T
             cas_e2Tbasin = cas_e2T
          endif

          if (deltaz25 .le. (Cas_e1T/Cas_e3Tbasin) ) then
             fb = Cas_e1T
          elseif (deltaz25 .ge. (cas_e2Tbasin/Cas_e3Tbasin) ) then
             fb = cas_e2Tbasin
          else
             fb = Cas_e3TBasin*deltaz25
          endif

C     Japan Basin Terms
      elseif (region .eq. 5 .or. region .eq. 6) then
          x = (alog10(vs30) - alog10(500.0) ) / (0.33*sqrt(2.0))
          muz25 = 10**(3.05-0.8*(1+erf(x)))
          deltaz25 = alog(z25*1000.0) - alog(muz25)

          if (deltaz25 .le. (Japan_e1T/Japan_e3T) ) then
             fb = Japan_e1T
          elseif (deltaz25 .ge. (Japan_e2T/Japan_e3T) ) then
             fb = Japan_e2T
          else
             fb = Japan_e3T*deltaz25
          endif
      else
         fb = 0.0
      endif
C     Compute the site specific ground motion
      lnY = c0T + Fp + Fm + Fd + Fs + Fb

c     Convert units spectral acceleration in gal
      if (spect .ne. -1.0) then
         lnY = lnY + 6.89
      endif

C     Compute sigma values
      if (rRup .le. 200.0) then
         phirup = sigphi1T
      elseif (rRup .ge. 500.0) then
         phirup = sigphi2T
      else
         phirup = ((sigphi2T - sigphi1T)/0.9163)*alog(rRup/200.0) + sigphi1T
      endif

      if (Vs30 .le. 200.0) then
         phivs = sigphivT*(alog(500.0/max(200.0,min(500.0,rRup)))/alog(500.0/200.0))
      elseif (Vs30 .ge. 500.0) then
         phivs = 0.0
      else
         phivs = sigphivT*(alog(500.0/Vs30)/alog(500.0/200.0))*(alog(500.0/(max(200.0,min(500.0,rRup))))/(alog(500.0/200.0)))
      endif

      phivar = phirup + phiVs
      phi = sqrt(phivar)

      tau = sigtauT

      sigma = sqrt(phi*phi + tau*tau)

      return
      end
