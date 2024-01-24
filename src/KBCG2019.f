     subroutine S35_KBCG2019 ( mag, Ftype, rRup, vs30, z25, lnSa, sigma, phi, tau,
     2                     specT, period2, iflag, depth, disthypo, iRegion, mbInter, mbSlab, ztor, CasBas, Z10 )

      implicit none
      integer MAXPER
      parameter (MAXPER=23)
      integer count1, count2, iflag, nPer, i1, i, iRegion, CasBas

      real Period(MAXPER), sigphi(MAXPER), sigtau(MAXPER), theta1a_Global(MAXPER), theta2(MAXPER),
     1     theta2a(MAXPER), theta3(MAXPER), theta4(MAXPER), theta4a(MAXPER), theta5(MAXPER),
     2     theta9(MAXPER), theta9a(MAXPER), delta_zb_if(MAXPER), delta_zb_slab(MAXPER),
     3     nft_1(MAXPER), nft_2(MAXPER), k1(MAXPER), k2(MAXPER), theta1_reg_AL(MAXPER),
     4     theta1_reg_CAS(MAXPER), theta1_reg_CAM(MAXPER), theta1_reg_Ja(MAXPER), theta1_reg_NZ(MAXPER),
     5     theta1_reg_SA(MAXPER), theta1_reg_TW(MAXPER), theta1_global(MAXPER), theta7_reg_AL(MAXPER),
     6     theta7_reg_Cas(MAXPER), theta7_reg_CAM(MAXPER), theta7_reg_Ja(MAXPER), theta7_reg_NZ(MAXPER),
     7     theta7_reg_SA(MAXPER), theta7_reg_Tw(MAXPER), theta7_global(MAXPER), theta62_reg_AL(MAXPER),
     8     theta62_reg_Cas(MAXPER), theta62_reg_CAM(MAXPER), theta62_reg_Ja(MAXPER), theta62_reg_NZ(MAXPER),
     9     theta62_reg_SA(MAXPER), theta62_reg_Tw(MAXPER), theta6_global(MAXPER)
      real sigphiT, sigtauT, theta2T, theta2aT, theta3T, theta4T, theta4aT, theta5T, theta9T,
     1     theta9aT, delta_zb_ifT, delta_zb_slabT, nft_1T, nft_2T, k1T, k2T, theta1_reg_AlT,
     2     theta1_reg_casT, theta1_reg_CAMT, theta1_reg_JaT, theta1_reg_NZT, theta1_reg_SAT,
     3     theta1_reg_TwT, theta1_globalT, theta7_reg_AlT, theta7_reg_CasT, theta7_reg_CAMT, theta7_reg_JaT,
     4     theta7_reg_NZT, theta7_reg_SAT, theta7_reg_TwT, theta7_globalT, theta62_reg_AlT,
     5     theta62_reg_CasT, theta62_reg_CAMT, theta62_reg_JaT, theta62_reg_NZT, theta62_reg_SAT,
     6     theta62_reg_TwT, theta6_globalT
      real theta1a_reg_AL(MAXPER),theta1a_reg_CAS(MAXPER), theta1a_reg_CAM(MAXPER), theta1a_reg_Ja(MAXPER),
     2     theta1a_reg_NZ(MAXPER), theta1a_reg_SA(MAXPER), theta1a_reg_TW(MAXPER)
      real theta1a_globalT, theta1a_reg_ALT, theta1a_reg_CAST, theta1a_reg_CAMT, theta1a_reg_jaT,
     1     theta1a_reg_NZT, theta1a_reg_SAT, theta1a_reg_TWT
      real CasBasinInter(MAXPER), CasBasinSlope(MAXPER), JapBasinInter(MAXPER), JapBasinSlope(MAXPER)
      real CasBasinInterT, CasBasinSlopeT, JapBasinInterT, JapBasinSlopeT, deltaZ25, predZ25, Z10, deltaZ10, predZ10
      real TaiBasinInter(MAXPER), TaiBasinSlope(MAXPER), TaiBasinInterT, TaiBasinSlopeT
      real NZBasinInter(MAXPER), NZBasinSlope(MAXPER), NZBasinInterT, NZBasinSlopeT
      real dmbInter(MAXPER), mbInterPGA, dmbInterT, mbInter0, CasSeaResid(MAXPER), CasSeaResidT

      real c, n, theta10, deltam, deltaz, zbif, zbslab, mref, zifref, zslabref, delta_mb_if, delta_mb_slab
      real delta_mb_BCH
      real theta1RegT, theta6RegT, theta7RegT
      real h, hpga, ztor
      real sigma, lnSa, pgaRock, vs30, rRup, disthypo, mag, depth, Ftype
      real period2, specT, z25, phi, tau, mbinter, mbslab
      real fconst, fmag, fgeom, fdepth, fatten, fsite
      real fconstPga, fmagPGa, fgeomPga, fdepthPGA, fattenPGA, fsitePGA
      real theta6RegPGA, theta7RegPGA, theta1RegPGA, fbasin
      real fsiteRockPGA, fbasinPGA, LnPGA


      data Period /  0.0, -1.0, 0.01, 0.02, 0.03, 0.05, 0.075, 0.1, 0.15, 0.2, 0.25, 0.3, 0.4, 0.5, 0.75,
     1               1.0, 1.5, 2.0, 3.0, 4.0, 5.0, 7.5, 10.0  /
      data k1  / 865.0, 400.0, 865.0, 865.0, 908.0, 1054.0, 1086.0, 1032.0, 878.0, 748.0, 654.0, 587.0,
     1           503.0, 457.0, 410.0, 400.0, 400.0, 400.0, 400.0, 400.0, 400.0, 400.0, 400.0 /
      data k2  / -1.186, -1.955, -1.186, -1.219, -1.273, -1.346, -1.471, -1.624, -1.931, -2.188, -2.381,
     1           -2.518, -2.657, -2.669, -2.401, -1.955, -1.025, -0.299, 0.0, 0.0, 0.0, 0.0, 0.0 /

C     Coefficients 9/02/2021 - Updated Alaska Coefficients Smoothed Long Period
      data theta1_global / 3.715306652, 6.43354268, 3.575653364, 3.716488614, 4.019203939, 4.544768777,
     1                     4.920943189, 5.073489777, 5.025990527, 4.769660602, 4.441347564,
     2                     4.095960014, 3.433449152, 2.846435008, 1.701639568, 0.890491783,
     3                    -0.175096723, -0.854226686, -1.707596773, -2.257679636, -2.663615911,
     4                    -3.368096986, -3.837778998 /
      data theta1a_global / 4.789080359, 9.43573316, 4.559571984, 4.808206646, 5.167197596, 5.640302621,
     1                      5.848237896, 5.830854259, 5.531066721, 5.132677534, 4.737976746,
     2                      4.374731034, 3.757219946, 3.265243755, 2.398836593, 1.829616288,
     3                      1.090154171, 0.586593296, -0.146183991, -0.711213054, -1.182577041,
     4                     -2.100674191, -2.768999266 /
      data theta2 / -2.460896366, -2.217436, -2.428916739, -2.459748358, -2.513834312, -2.589450354,
     1              -2.619896357, -2.608322791, -2.531737908, -2.434298892, -2.338216333,
     2              -2.250204787, -2.102934572, -1.989991219, -1.811384538, -1.719992958,
     3              -1.652688213, -1.646034912, -1.676629343, -1.709830071, -1.732636218,
     4              -1.750019169, -1.735295193 /
      data theta2a / -2.438099383, -2.6695061, -2.395028055, -2.443224205, -2.496444857, -2.541066633,
     1               -2.525555518, -2.477878863, -2.361283158, -2.252963091, -2.16215032,
     2               -2.088032245, -1.979317168, -1.907871091, -1.818965748, -1.791480543,
     3               -1.79780401, -1.825030176, -1.875875935, -1.908284477, -1.925829475,
     4               -1.933397388, -1.918143528 /
      data theta3 / 0.10393058, 0.1278845, 0.104318059, 0.104711329, 0.104531274, 0.10337605, 0.101545988,
     1              0.099788169, 0.096885665, 0.094745404, 0.093177224, 0.092031873, 0.090583773,
     2              0.089848329, 0.089517549, 0.090095535, 0.091896751, 0.09368432, 0.096501829,
     3              0.098438255, 0.099773762, 0.101593855, 0.102312143 /
      data theta4 / 0.952187401, 1.01448335, 0.945373983, 0.958823587, 0.973096474, 0.991235338, 1.000331099,
     1              1.001912667, 0.997173516, 0.990941872, 0.986668856, 0.985056106, 0.989150137,
     2              1.000679554, 1.047320756, 1.103813757, 1.215946727, 1.313705228, 1.463119514,
     3              1.566045928, 1.637882578, 1.738643843, 1.780438096 /
      data theta4a / 1.10501155, 1.30757813, 1.108606759, 1.116235958, 1.109268454, 1.090513308, 1.078532394,
     1               1.078729359, 1.099427554, 1.13154674, 1.166600358, 1.201257777, 1.26504902,
     2               1.320126168, 1.426015163, 1.500926099, 1.60075645, 1.666319535, 1.752683857,
     3               1.811761179, 1.857447102, 1.941357713, 2.00091285 /
      data theta5 / 0.128150984, 0.123716, 0.129146009, 0.132406953, 0.134842835, 0.13730139, 0.137915277,
     1              0.137330854, 0.134973442, 0.132355343, 0.12994531, 0.12784598, 0.124505287,
     2              0.122092052, 0.118613921, 0.117157008, 0.116773256, 0.117650902, 0.120191283,
     3              0.122611682, 0.124654241, 0.128327776, 0.130642589 /
      data theta6_global / -0.002634759, -0.0013477, -0.002699424, -0.002614211, -0.002548149, -0.002550059,
     1                     -0.002672773, -0.002820768, -0.003084744, -0.003273108, -0.003413419,
     2                     -0.003508116, -0.003620243, -0.003627501, -0.003470123, -0.003218289,
     3                     -0.002747866, -0.002339633, -0.001756502, -0.001376542, -0.001123589,
     4                     -0.000826115, -0.000728431 /
      data theta7_global / 0.88767875, 1.687675, 0.892437982, 0.930261469, 1.01275289, 1.1880131, 1.372273896,
     1                     1.531423527, 1.800940611, 2.015482111, 2.182527609, 2.307342334, 2.444039384,
     2                     2.451673734, 2.096480325, 1.516381724, 0.459844405, -0.176024266, -0.592415429,
     3                    -0.585389016, -0.512670766, -0.431586954, -0.428490629 /
      data theta9 / 0.025480655, 0.0161514, 0.025058198, 0.027178223, 0.029343849, 0.031911432, 0.032822575,
     1              0.032447898, 0.030225714, 0.027520137, 0.024917221, 0.02257519, 0.01872305,
     2              0.015822693, 0.011351668, 0.009168065, 0.00781531, 0.008069539, 0.009820042,
     3              0.011723614, 0.013386835, 0.016398519, 0.01828198 /
      data theta9a / 0.022385169, 0.01552129, 0.021963597, 0.023277106, 0.02453943, 0.02575995, 0.02579158,
     1               0.025080709, 0.022995598, 0.02084772, 0.018906568, 0.017206788, 0.014446286,
     2               0.012344736, 0.008820911, 0.006644533, 0.004020303, 0.002398108, 0.000278753,
     3              -0.001183113, -0.002310934, -0.004286542, -0.005541411 /
c      data theta1_reg_Al / 3.324582852, 6.192513888, 3.111678527, 3.272985702, 3.599643065, 4.153519077,
c     1                     4.558982077, 4.744258602, 4.768031065, 4.579911977, 4.311011865, 4.015117027,
c     2                     3.424768965, 2.881877487, 1.77365556, 0.947723395, -0.188551198, -0.934816148,
c     3                    -1.861303948, -2.41549166, -2.78204016, -3.299650373, -3.545156523 /
      data theta1_reg_Al / 3.324582852, 6.192513888, 3.111678527, 3.272985702, 3.599643065, 4.153519077,
     1                     4.558982077, 4.744258602, 4.768031065, 4.579911977, 4.311011865, 4.015117027,
     2                     3.424768965, 2.881877487, 1.77365556, 0.947723395, -0.188551198, -0.934816148,
     3                    -1.861303948, -2.41549166, -2.78204016, -3.376621108, -3.812917708 /
      data theta1_reg_Cas / 3.600304874, 6.43341768, 3.480334924, 3.601501862, 3.881527524, 4.387812562,
     1                      4.768796999, 4.937742037, 4.927519437, 4.702098799, 4.396801049,
     2                      4.068026237, 3.425683137, 2.848400136, 1.70893745, 0.894849762,
     3                     -0.177633126, -0.859883288, -1.712325988, -2.258375988, -2.660212813,
     4                     -3.357720901, -3.824347713 /
      data theta1_reg_CAM / 3.696420974, 6.41128435, 3.521664099, 3.684086811, 4.009646711, 4.549220574,
     1                      4.916524961, 5.053960299, 4.981228686, 4.710481311, 4.375634036,
     2                      4.028587899, 3.370173599, 2.791079115, 1.666433496, 0.869768161,
     3                     -0.180839526, -0.854612751, -1.706495114, -2.257421039, -2.663634851,
     4                     -3.365218976, -3.829128839 /
      data theta1_reg_Ja / 4.008987257, 6.50527566, 3.786327782, 3.966415432, 4.346256007, 4.964188257,
     1                     5.36193617, 5.488463607, 5.350477145, 5.005995045, 4.606384295,
     2                     4.206322057, 3.472405045, 2.847101424, 1.673874536, 0.868523382,
     3                    -0.172629318, -0.83651918, -1.684797118, -2.243241693, -2.659023743,
     4                    -3.375555293, -3.83850338 /
      data theta1_reg_NZ / 3.943337888, 6.46045166, 3.810325438, 3.912658438, 4.214684, 4.772026638,
     1                     5.180892763, 5.346631513, 5.2890084, 5.002106925, 4.6387195,
     2                     4.259727838, 3.5410181, 2.9129436, 1.712003021, 0.880914761,
     3                    -0.186117187, -0.8541594, -1.68967475, -2.233627612, -2.640906075,
     4                    -3.360428112, -3.845874587 /
      data theta1_reg_SA / 4.051210372, 6.43999505, 3.935535147, 4.057270272, 4.354312135, 4.88884236,
     1                     5.274643422, 5.425392947, 5.350816772, 5.05429396, 4.68392416,
     2                     4.29905291, 3.570140297, 2.93325554, 1.71546997, 0.873661997,
     3                    -0.203168403, -0.87287104, -1.703078565, -2.239111678, -2.638954865,
     4                    -3.344667653, -3.821734003 /
      data theta1_reg_Tw / 3.446871055, 6.43448706, 3.31199428, 3.43677063, 3.698230605, 4.16120153,
     1                     4.51271993, 4.676007205, 4.68984193, 4.504351367, 4.23980503,
     2                     3.947873555, 3.364050705, 2.827443326, 1.739913367, 0.94144859,
     3                    -0.135476695, -0.83215377, -1.70617387, -2.261116483, -2.664491845,
     4                    -3.355595383, -3.816492708 /
c      data theta1a_reg_Al / 4.149829797, 9.8146774, 3.923380172, 4.24921056, 4.60123381, 4.99344756,
c     1                      5.120876872, 5.069682685, 4.792279285, 4.470693522, 4.168180172, 3.896536697,
c     2                      3.439476451, 3.071433322, 2.388354905, 1.892909355, 1.167362272, 0.626671222,
c     3                     -0.164460478, -0.724734053, -1.139277078, -1.791549815, -2.13024199 /
      data theta1a_reg_Al / 4.149829797, 9.8146774, 3.923380172, 4.24921056, 4.60123381, 4.99344756,
     1                      5.120876872, 5.069682685, 4.792279285, 4.470693522, 4.168180172, 3.896536697,
     2                      3.439476451, 3.071433322, 2.388354905, 1.892909355, 1.167362272, 0.626671222,
     3                     -0.164460478, -0.724734053, -1.139277078, -2.098557165, -2.735575178 /
      data theta1a_reg_Cas / 4.486160214, 9.44157083, 4.309173564, 4.492272227, 4.780066177, 5.191630102,
     1                       5.409688439, 5.437210652, 5.242301202, 4.932354902, 4.604524402,
     2                       4.290405089, 3.734440359, 3.274077642, 2.429917247, 1.855767138,
     3                       1.097427152, 0.582327377, -0.154870361, -0.714677586, -1.179599661,
     4                      -2.089046048, -2.759944648 /
      data theta1a_reg_CAM / 5.041801411, 9.40842905, 4.809797423, 5.138580211, 5.547287961, 6.041135236,
     1                       6.214868648, 6.146773936, 5.751216648, 5.279849461, 4.832425536,
     2                       4.431417661, 3.767458256, 3.251794002, 2.368038756, 1.801356947,
     3                       1.074441061, 0.579301661, -0.148164002, -0.713874664, -1.186648539,
     4                      -2.103672102, -2.764279914 /
      data theta1a_reg_Ja / 5.36094074, 9.56171671, 5.048204203, 5.35605789, 5.823778528, 6.424629515,
     1                      6.65407559, 6.587999378, 6.135932665, 5.591939103, 5.07930914,
     2                      4.624838865, 3.885418432, 3.324406957, 2.394374697, 1.819634315,
     3                      1.09961974, 0.610200003, -0.12041156, -0.696791947, -1.179522347,
     4                     -2.106138247, -2.756891322 /
      data theta1a_reg_NZ / 4.822427116, 9.44242678, 4.653637454, 4.860414466, 5.176631041, 5.618999054,
     1                      5.835589729, 5.839812216, 5.579030904, 5.203979691, 4.820203141,
     2                      4.459910304, 3.835826743, 3.330662296, 2.430571835, 1.837887843,
     3                      1.077221991, 0.570044391, -0.154812746, -0.710636734, -1.176416246,
     4                     -2.094423559, -2.773091609 /
      data theta1a_reg_SA / 5.079648822, 9.36631119, 4.840730747, 5.154175935, 5.57760931, 6.114167272,
     1                      6.32265626, 6.26824361, 5.86644186, 5.371816747, 4.898129985,
     2                      4.472628647, 3.769547817, 3.22733893, 2.313242356, 1.74213861,
     3                      1.029526247, 0.55235046, -0.15173344, -0.708760028, -1.181004115,
     4                     -2.10961869, -2.784359065 /
      data theta1a_reg_Tw / 4.391529844, 9.33738787, 4.203531294, 4.388374744, 4.666983819, 5.054938069,
     1                      5.254962794, 5.277204731, 5.092471831, 4.803260306, 4.497810356,
     2                      4.204567281, 3.682760571, 3.246846383, 2.434937705, 1.870986279,
     3                      1.110181869, 0.585690969, -0.166118681, -0.732027631, -1.197657881,
     4                     -2.099842756, -2.761835231 /
c      data theta7_reg_AL / 0.551140468, 1.246646108, 0.572924573, 0.606050262, 0.715872694, 0.986759236,
c     1                     1.244194485, 1.399564893, 1.549306356, 1.62948428, 1.699971678, 1.76786809,
c     2                     1.871947148, 1.903775085, 1.675204801, 1.214579429, 0.299504686, -0.29013837,
c     3                    -0.718257827, -0.739439677, -0.682618715, -0.623527877, -0.66026114 /
      data theta7_reg_AL / 0.551140468, 1.246646108, 0.572924573, 0.606050262, 0.715872694, 0.986759236,
     1                     1.244194485, 1.399564893, 1.549306356, 1.62948428, 1.699971678, 1.76786809,
     2                     1.871947148, 1.903775085, 1.675204801, 1.214579429, 0.299504686, -0.29013837,
     3                    -0.718257827, -0.739439677, -0.682618715, -0.631117395, -0.569526682 /
      data theta7_reg_Cas / 0.662527069, 1.55887653, 0.667082832, 0.707218024, 0.782766296, 0.959353513,
     1                      1.183366699, 1.382117651, 1.682451037, 1.889590743, 2.041945092,
     2                      2.156154854, 2.287970004, 2.302687729, 1.969235669, 1.400361498,
     3                      0.356549952, -0.262818326, -0.638106383, -0.592149796, -0.487937496,
     4                     -0.364708696, -0.362123483 /
      data theta7_reg_CAM / 1.147446734, 1.85177447, 1.109680891, 1.161897312, 1.24741899, 1.368367591,
     1                      1.494059219, 1.643536028, 1.973240894, 2.261112938, 2.476115899,
     2                      2.621750749, 2.748278586, 2.718123161, 2.279570523, 1.664982299,
     3                      0.59274564, -0.063796126, -0.544892764, -0.580628139, -0.519162314,
     4                     -0.410008426, -0.387004114 /
      data theta7_reg_Ja / 0.86972574, 1.53952407, 0.870923483, 0.912943812, 1.011507346, 1.257663811,
     1                     1.49028266, 1.630928725, 1.786976198, 1.896749447, 1.995613121,
     2                     2.081211992, 2.186512067, 2.187920655, 1.83732347, 1.264853747,
     3                     0.224215841, -0.39993712, -0.802764383, -0.780815983, -0.68758182,
     4                    -0.549707683, -0.505965695 /
      data theta7_reg_NZ / 1.005782506, 1.78608459, 1.012431679, 1.056041081, 1.141592746, 1.296723954,
     1                     1.462358148, 1.625128475, 1.924311811, 2.15745282, 2.32477949,
     2                     2.438066215, 2.540737377, 2.519785177, 2.140886776, 1.572665626,
     3                     0.544469013, -0.08281136, -0.49804516, -0.483198185, -0.401492923,
     4                    -0.332415348, -0.387987785 /
      data theta7_reg_SA / 1.131304474, 2.098709, 1.147077322, 1.208208624, 1.276473281, 1.367020497,
     1                     1.469249031, 1.606935822, 1.945485117, 2.274055939, 2.542924494,
     2                     2.742512956, 2.955971819, 2.980301156, 2.590644506, 1.980834172,
     3                     0.913346769, 0.274633447, -0.174358569, -0.205313894, -0.159049369,
     4                    -0.113042244, -0.143450381 /
      data theta7_reg_Tw / 0.933688039, 1.67248556, 0.910859521, 0.953422087, 1.033921126, 1.187023224,
     1                     1.35787035, 1.520788001, 1.816593859, 2.057558467, 2.242567296,
     2                     2.376440146, 2.511422896, 2.502295571, 2.089379765, 1.460664727,
     3                     0.34695872, -0.315465991, -0.749482754, -0.741057329, -0.659579766,
     4                    -0.551928766, -0.531306454 /
c      data theta62_reg_AL / -0.002727249, -0.001656003, -0.002760841, -0.002737411, -0.002736235, -0.002810708,
c     1                      -0.002912096, -0.003009239, -0.00314302, -0.003235036, -0.003288512, -0.003332162,
c     2                      -0.003360836, -0.003320488, -0.003111385, -0.002887742, -0.002461284, -0.002101573,
c     3                      -0.001584346, -0.001266269, -0.001058123, -0.000816203, -0.000766118 /
      data theta62_reg_AL / -0.002727249, -0.001656003, -0.002760841, -0.002737411, -0.002736235, -0.002810708,
     1                      -0.002912096, -0.003009239, -0.00314302, -0.003235036, -0.003288512, -0.003332162,
     2                      -0.003360836, -0.003320488, -0.003111385, -0.002887742, -0.002461284, -0.002101573,
     3                      -0.001584346, -0.001266269, -0.001058123, -0.000868331, -0.00083004 /
      data theta62_reg_Cas / -0.00378872, -0.0013097, -0.003919403, -0.003787575, -0.003683177, -0.003680303,
     1                       -0.003883777, -0.004138913, -0.004589887, -0.004916118, -0.005140047,
     2                       -0.005276242, -0.005410199, -0.005393397, -0.005048778, -0.004564926,
     3                       -0.003666813, -0.002920969, -0.001909979, -0.001295891, -0.000913294,
     4                       -0.000494664, -0.000390228 /
      data theta62_reg_CAM / -0.001378838, -0.0009827, -0.00132529, -0.001329181, -0.00137577, -0.001486615,
     1                       -0.001608964, -0.00170333, -0.001825801, -0.001891814, -0.001937343,
     2                       -0.001959577, -0.001983679, -0.001975796, -0.001915739, -0.001837589,
     3                       -0.001701139, -0.001591565, -0.00140805, -0.00127133, -0.001167597,
     4                       -0.001003264, -0.000899137 /
      data theta62_reg_Ja / -0.00281163, -0.0019607, -0.002915696, -0.002888919, -0.002837462, -0.002794009,
     1                      -0.002831023, -0.00288636, -0.00304475, -0.003173064, -0.003288383,
     2                      -0.003360069, -0.0034541, -0.003481557, -0.003410605, -0.003229835,
     3                      -0.002861705, -0.0025148, -0.001984637, -0.001630897, -0.001413493,
     4                      -0.001218557, -0.00125276 /
      data theta62_reg_NZ / -0.001984352, -0.0005497, -0.00193905, -0.001895321, -0.001910774, -0.002021342,
     1                      -0.002193522, -0.002347337, -0.002575118, -0.002722229, -0.002818411,
     2                      -0.002881902, -0.002936009, -0.002924349, -0.002787277, -0.002607331,
     3                      -0.002268416, -0.001981616, -0.001552311, -0.001265052, -0.001061875,
     4                      -0.000774037, -0.000634906 /
      data theta62_reg_SA / -0.00209537, -0.0011235, -0.002071798, -0.002033545, -0.002045657, -0.002130904,
     1                      -0.002262679, -0.002366816, -0.002506983, -0.002564728, -0.002590677,
     2                      -0.002587561, -0.002556041, -0.002491535, -0.002310983, -0.002133191,
     3                      -0.001875352, -0.001684623, -0.001415576, -0.001219844, -0.001080541,
     4                      -0.000875424, -0.00073729 /
      data theta62_reg_Tw / -0.003046717, -0.0018548, -0.003128106, -0.002916352, -0.002687556, -0.002453776,
     1                      -0.002469507, -0.002641826, -0.003111805, -0.003563224, -0.003945293,
     2                      -0.004249917, -0.004668449, -0.004877669, -0.004934195, -0.00467427,
     3                      -0.003939296, -0.003234659, -0.00218645, -0.001522874, -0.001110153,
     4                      -0.000666067, -0.000611445 /
      data delta_zb_if / 16.81605756, 12.5660947, 16.8238131, 16.66218908, 17.18166098, 18.32074494, 18.92369876,
     1                   18.75864934, 17.286575, 15.32167741, 13.37493716, 11.61349725, 8.768199935,
     2                    6.72811905, 3.964759788, 2.99839585, 2.958589638, 3.467766463, 3.92726955,
     3                    3.370655838, 2.129626625, -2.2543041, -6.949083475 /
      data delta_zb_slab / -15.84125345, -13.359066, -16.12506073, -16.28403215, -16.31449001, -16.30017631,
     1                     -16.24007111, -16.14727661, -15.87766608, -15.52663778, -15.12641286,
     2                     -14.69940528, -13.81827288, -12.94797861, -10.95322734, -9.255852251,
     3                      -6.60057452, -4.646262708, -1.994229308, -0.305374608, 0.840704092,
     4                       2.472428092, 3.239206467 /
      data nft_1 / 0.894588882, 0.93637922, 0.896423521, 0.897168407, 0.896405171, 0.893480171, 0.889433915,
     1             0.885756638, 0.879903372, 0.875691026, 0.872635815, 0.870398829, 0.867543383,
     2             0.866017459, 0.864982593, 0.86559616, 0.868035809, 0.870533621, 0.874445533,
     3             0.877064259, 0.87880376, 0.881004891, 0.881692145 /
      data nft_2 / 0.198851305, 0.21094258, 0.199091802, 0.199150631, 0.198909735, 0.198209877, 0.197375325,
     1             0.19669797, 0.195765446, 0.195220279, 0.194916495, 0.194765678, 0.194726462,
     2             0.194876022, 0.195519746, 0.196226914, 0.197458581, 0.198405476, 0.199694436,
     3             0.200494411, 0.201025627, 0.201732235, 0.20200423 /
      data sigphi / 0.595755278, 0.51148649, 0.599595609, 0.59241506, 0.604430227, 0.638018891, 0.666119594,
     1              0.678368904, 0.677739961, 0.664654218, 0.649527824, 0.635787238, 0.615314866,
     2              0.603262958, 0.594773331, 0.59897004, 0.611546271, 0.617348448, 0.609290796,
     3              0.587788163, 0.562101633, 0.500922328, 0.453378144 /
      data sigtau / 0.488744744, 0.45098461, 0.494229731, 0.505458398, 0.511667679, 0.515792855, 0.514872686,
     1              0.511974691, 0.505515371, 0.500076345, 0.495846384, 0.492609188, 0.488214611,
     2              0.485606983, 0.482858615, 0.482329313, 0.482849552, 0.483535832, 0.48402177,
     3              0.483600245, 0.482729987, 0.480025032, 0.477483008 /

C     Cascadia Basin and Seattle Basin Coefficients updated 11/1/19
      data  CasBasinInter / -0.033754301, -0.012278, -0.03437367, -0.034662015, -0.032014189, -0.028009288,
     1                      -0.033581978, -0.025227652, -0.032857328, -0.034587858, -0.031240354,
     2                      -0.027222135, -0.045714985, -0.031599507, -0.032778512, -0.029057663,
     3                      -0.023875663, -0.039217868, 0.00042061, 0.018831799, 0.016382687,
     4                       0.012890241, 0.015757278 /
      data  CasBasinSlope / 0.020800512, 0.11844209, 0.020403813, 0.018233466, 0.014046186, -0.016393706,
     1                     -0.060272212, -0.077067179, -0.051386265, -0.016655504, 0.035523765,
     2                      0.051800012, 0.11065903, 0.129281574, 0.184907514, 0.173915665,
     3                      0.201715295, 0.230981824, 0.210505666, 0.244947111, 0.261637485,
     4                      0.220665408, 0.186336738 /
      data  CasSeaResid / -0.126368667, 0.118442089, -0.126722682, -0.128641453, -0.132860752, -0.17904093,
     1                    -0.261658424, -0.276774462, -0.24668039, -0.174197802, -0.08799574,
     2                    -0.046549387, 0.023017763, 0.09922914, 0.20957875, 0.236757351, 0.286400113,
     3                     0.351803208531182, 0.417364971913835, 0.448002389, 0.521325059, 0.485046352, 0.40273411 /
      data  JapBasinInter / -0.000552945, -0.0162883, 0.000252659, -0.000199039, 0.001563091, 0.009631227,
     1                       0.013762593, 0.010235154, -0.009497614, -0.013801794, -0.008879054,
     2                      -0.010823018, -0.01393108, -0.019981112, -0.0241875, -0.002670865,
     3                       0.004608325, -0.014419342, 0.017050267, 0.052368336, 0.010241569,
     4                      -0.005722435, -0.012161281 /
      data  JapBasinSlope / -0.027347239, 0.10123977, -0.028776069, -0.030456671, -0.03626794, -0.057726275,
     1                      -0.076256191, -0.068037275, -0.050343659, -0.035030582, -0.022592393,
     2                      -0.005371836, 0.030155088, 0.074733416, 0.141358586, 0.18875034,
     3                       0.249536336, 0.280580414, 0.305661189, 0.307163658, 0.292740348,
     4                       0.248739909, 0.214443383 /

      data  TaiBasinInter / 0.013176757, 0.0116407, 0.012098583, 0.011978376, 0.012910306, 0.010570091,
     1                      0.013508881, 0.01025206, 0.015651805, 0.014435965, 0.021983556,
     2                      0.019669811, 0.016888863, 0.018284057, 0.006109487, 0.015144663,
     3                      0.022357196, 0.013477586, 0.025343721, 0.030810534, 0.012982342,
     4                      0.013252225, -0.002628502 /
      data  TaiBasinSlope / 0.047719326, 0.078625, 0.046885023, 0.047268369, 0.047962287, 0.032748082,
     1                      0.040801587, 0.045775043, 0.040260644, 0.042212758, 0.050554267,
     2                      0.053314924, 0.051926286, 0.065895191, 0.085522692, 0.110850338,
     3                      0.15125173, 0.168365385, 0.157175193, 0.13208657, 0.135284696,
     4                      0.125746568, 0.112043062 /

      data NZBasinInter / -0.029314763, 0.00788926, -0.02830015, -0.028717664, -0.032177337, -0.044733843,
     1                    -0.043511236, -0.047968738, -0.039382031, -0.025000918, -0.022148713,
     2                    -0.010104446, 0.007677064, 0.016125544, 0.012059183, 0.030448367,
     3                     0.043018473, 0.046788465, 0.058672133, 0.042133993, 0.044754822,
     4                     0.03714183, 0.020092163 /
      data NZBasinSlope / -0.083452599, -0.0111422, -0.08319884, -0.082965508, -0.083556642, -0.081382979,
     1                    -0.097523701, -0.124030048, -0.118887899, -0.089642052, -0.11874965,
     2                    -0.084007842, -0.045937946, -0.019815379, 0.004010888, 0.044832102,
     3                     0.094098855, 0.112241422, 0.12463767, 0.136546104, 0.159273263,
     4                     0.126676539, 0.109094018 /

C     Mag scaling adjustment for Japan and South America Regions only
      data dmbInter / 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,
     1               -0.147628099, -0.252371901, -0.4, -0.4, -0.4, -0.4, -0.4 /

C     Constant parameters

      delta_mb_if = 0.0
      delta_mb_slab = 0.0
      delta_mb_BCH = 0.0
      c = 1.88
      n = 1.18
      theta10 = 0.0
      deltam = 0.1
      deltaz = 1.0
      zbif = 30.0
      zbslab = 80.0

      mref = 6.0
      Zifref = 15.0
      zslabref = 50.0
      mbInter0 = mbInter
      mbInterPGA = mbInter

C Find the requested spectral period and corresponding coefficients
      nPer = 23

C First check for the PGA case
      if (specT .eq. 0.0) then
         i1=1
         period2 = period(i1)
         sigphiT = sigphi(i1)
         sigtauT = sigtau(i1)
         theta2T = theta2(i1)
         theta2aT = theta2a(i1)
         theta3T = theta3(i1)
         theta4T = theta4(i1)
         theta4aT = theta4a(i1)
         theta5T = theta5(i1)
         theta9T = theta9(i1)
         theta9aT = theta9a(i1)
         delta_zb_ifT = delta_zb_if(i1)
         delta_zb_slabT = delta_zb_slab(i1)
         nft_1T = nft_1(i1)
         nft_2T = nft_2(i1)
         k1T = k1(i1)
         k2T = k2(i1)
         theta1_reg_AlT = theta1_reg_Al(i1)
         theta1_reg_CasT = theta1_reg_Cas(i1)
         theta1_reg_CAMT = theta1_reg_CAM(i1)
         theta1_reg_JaT = theta1_reg_Ja(i1)
         theta1_reg_NZT = theta1_reg_NZ(i1)
         theta1_reg_SAT = theta1_reg_SA(i1)
         theta1_reg_TwT = theta1_reg_Tw(i1)
         theta1_GlobalT = theta1_global(i1)

         theta1a_globalT = theta1a_global(i1)
         theta1a_reg_ALT = theta1a_reg_AL(i1)
         theta1a_reg_CasT = theta1a_reg_Cas(i1)
         theta1a_reg_CAMT = theta1a_reg_CAM(i1)
         theta1a_reg_JAT = theta1a_reg_Ja(i1)
         theta1a_reg_NZT = theta1a_reg_NZ(i1)
         theta1a_reg_SAT = theta1a_reg_SA(i1)
         theta1a_reg_TwT = theta1a_reg_Tw(i1)

         theta7_reg_AlT = theta7_reg_Al(i1)
         theta7_reg_CasT = theta7_reg_Cas(i1)
         theta7_reg_CAMT = theta7_reg_CAM(i1)
         theta7_reg_JaT = theta7_reg_Ja(i1)
         theta7_reg_NZT = theta7_reg_NZ(i1)
         theta7_reg_SAT = theta7_reg_SA(i1)
         theta7_reg_TwT = theta7_reg_Tw(i1)
         theta7_GlobalT = theta7_global(i1)

         theta62_reg_AlT = theta62_reg_Al(i1)
         theta62_reg_CasT = theta62_reg_Cas(i1)
         theta62_reg_CAMT = theta62_reg_CAM(i1)
         theta62_reg_JaT = theta62_reg_Ja(i1)
         theta62_reg_NZT = theta62_reg_NZ(i1)
         theta62_reg_SAT = theta62_reg_SA(i1)
         theta62_reg_TwT = theta62_reg_Tw(i1)
         theta6_GlobalT = theta6_global(i1)

         CasBasinInterT = CasBasinInter(i1)
         CasBasinSlopeT = CasBasinSlope(i1)
         CasSeaResidT = CasSeaResid(i1)
         JapBasinInterT = JapBasinInter(i1)
         JapBasinSlopeT = JapBasinSlope(i1)
         TaiBasinInterT = TaiBasinInter(i1)
         TaiBasinSlopeT = TaiBasinSlope(i1)
         NZBasinInterT = NZBasinInter(i1)
         NZBasinSlopeT = NZBasinSlope(i1)

         dmbInterT = 0.0

         goto 1011
      elseif (specT .eq. -1.0) then
         i1=2
         period2 = period(i1)
         sigphiT = sigphi(i1)
         sigtauT = sigtau(i1)
         theta2T = theta2(i1)
         theta2aT = theta2a(i1)
         theta3T = theta3(i1)
         theta4T = theta4(i1)
         theta4aT = theta4a(i1)
         theta5T = theta5(i1)
         theta9T = theta9(i1)
         theta9aT = theta9a(i1)
         delta_zb_ifT = delta_zb_if(i1)
         delta_zb_slabT = delta_zb_slab(i1)
         nft_1T = nft_1(i1)
         nft_2T = nft_2(i1)
         k1T = k1(i1)
         k2T = k2(i1)
         theta1_reg_AlT = theta1_reg_Al(i1)
         theta1_reg_CasT = theta1_reg_Cas(i1)
         theta1_reg_CAMT = theta1_reg_CAM(i1)
         theta1_reg_JaT = theta1_reg_Ja(i1)
         theta1_reg_NZT = theta1_reg_NZ(i1)
         theta1_reg_SAT = theta1_reg_SA(i1)
         theta1_reg_TwT = theta1_reg_Tw(i1)
         theta1_GlobalT = theta1_global(i1)

         theta1a_globalT = theta1a_global(i1)
         theta1a_reg_ALT = theta1a_reg_AL(i1)
         theta1a_reg_CasT = theta1a_reg_Cas(i1)
         theta1a_reg_CAMT = theta1a_reg_CAM(i1)
         theta1a_reg_JAT = theta1a_reg_Ja(i1)
         theta1a_reg_NZT = theta1a_reg_NZ(i1)
         theta1a_reg_SAT = theta1a_reg_SA(i1)
         theta1a_reg_TwT = theta1a_reg_Tw(i1)

         theta7_reg_AlT = theta7_reg_Al(i1)
         theta7_reg_CasT = theta7_reg_Cas(i1)
         theta7_reg_CAMT = theta7_reg_CAM(i1)
         theta7_reg_JaT = theta7_reg_Ja(i1)
         theta7_reg_NZT = theta7_reg_NZ(i1)
         theta7_reg_SAT = theta7_reg_SA(i1)
         theta7_reg_TwT = theta7_reg_Tw(i1)
         theta7_GlobalT = theta7_global(i1)

         theta62_reg_AlT = theta62_reg_Al(i1)
         theta62_reg_CasT = theta62_reg_Cas(i1)
         theta62_reg_CAMT = theta62_reg_CAM(i1)
         theta62_reg_JaT = theta62_reg_Ja(i1)
         theta62_reg_NZT = theta62_reg_NZ(i1)
         theta62_reg_SAT = theta62_reg_SA(i1)
         theta62_reg_TwT = theta62_reg_Tw(i1)
         theta6_GlobalT = theta6_global(i1)

         CasBasinInterT = CasBasinInter(i1)
         CasBasinSlopeT = CasBasinSlope(i1)
         CasSeaResidT = CasSeaResid(i1)
         JapBasinInterT = JapBasinInter(i1)
         JapBasinSlopeT = JapBasinSlope(i1)
         TaiBasinInterT = TaiBasinInter(i1)
         TaiBasinSlopeT = TaiBasinSlope(i1)
         NZBasinInterT = NZBasinInter(i1)
         NZBasinSlopeT = NZBasinSlope(i1)

         dmbInterT = 0.0

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
      write (*,*) 'KBCG (2019) Horizontal'
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
      call S24_interp (period(count1),period(count2),theta2(count1),theta2(count2),
     +                   specT,theta2T,iflag)
      call S24_interp (period(count1),period(count2),theta2a(count1),theta2a(count2),
     +                   specT,theta2aT,iflag)
      call S24_interp (period(count1),period(count2),theta3(count1),theta3(count2),
     +                   specT,theta3T,iflag)
      call S24_interp (period(count1),period(count2),theta4(count1),theta4(count2),
     +                   specT,theta4T,iflag)
      call S24_interp (period(count1),period(count2),theta4a(count1),theta4a(count2),
     +                   specT,theta4aT,iflag)
      call S24_interp (period(count1),period(count2),theta5(count1),theta5(count2),
     +                   specT,theta5T,iflag)
      call S24_interp (period(count1),period(count2),theta9(count1),theta9(count2),
     +                   specT,theta9T,iflag)
      call S24_interp (period(count1),period(count2),theta9a(count1),theta9a(count2),
     +                   specT,theta9aT,iflag)
      call S24_interp (period(count1),period(count2),delta_zb_if(count1),delta_zb_if(count2),
     +                   specT,delta_zb_ifT,iflag)
      call S24_interp (period(count1),period(count2),delta_zb_slab(count1),delta_zb_slab(count2),
     +                   specT,delta_zb_slabT,iflag)
      call S24_interp (period(count1),period(count2),nft_1(count1),nft_1(count2),
     +                   specT,nft_1T,iflag)
      call S24_interp (period(count1),period(count2),nft_2(count1),nft_2(count2),
     +                   specT,nft_2T,iflag)
      call S24_interp (period(count1),period(count2),k1(count1),k1(count2),
     +                   specT,k1T,iflag)
      call S24_interp (period(count1),period(count2),k2(count1),k2(count2),
     +                   specT,k2T,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_AL(count1),theta1_reg_AL(count2),
     +                   specT,theta1_reg_ALT,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_CAS(count1),theta1_reg_CAS(count2),
     +                   specT,theta1_reg_CAST,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_CAM(count1),theta1_reg_CAM(count2),
     +                   specT,theta1_reg_CAMT,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_Ja(count1),theta1_reg_Ja(count2),
     +                   specT,theta1_reg_JaT,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_NZ(count1),theta1_reg_NZ(count2),
     +                   specT,theta1_reg_NZT,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_SA(count1),theta1_reg_SA(count2),
     +                   specT,theta1_reg_SAT,iflag)
      call S24_interp (period(count1),period(count2),theta1_reg_Tw(count1),theta1_reg_Tw(count2),
     +                   specT,theta1_reg_TwT,iflag)
      call S24_interp (period(count1),period(count2),theta1_global(count1),theta1_global(count2),
     +                   specT,theta1_globalT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_global(count1),theta1a_global(count2),
     +                   specT,theta1a_GlobalT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_AL(count1),theta1a_reg_AL(count2),
     +                   specT,theta1a_reg_ALT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_Cas(count1),theta1a_reg_Cas(count2),
     +                   specT,theta1a_reg_CasT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_CAM(count1),theta1a_reg_CAM(count2),
     +                   specT,theta1a_reg_CAMT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_Ja(count1),theta1a_reg_Ja(count2),
     +                   specT,theta1a_reg_JaT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_NZ(count1),theta1a_reg_NZ(count2),
     +                   specT,theta1a_reg_NZT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_SA(count1),theta1a_reg_SA(count2),
     +                   specT,theta1a_reg_SAT,iflag)
      call S24_interp (period(count1),period(count2),theta1a_reg_Tw(count1),theta1a_reg_Tw(count2),
     +                   specT,theta1a_reg_TwT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_AL(count1),theta7_reg_AL(count2),
     +                   specT,theta7_reg_ALT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_CAS(count1),theta7_reg_CAS(count2),
     +                   specT,theta7_reg_CAST,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_CAM(count1),theta7_reg_CAM(count2),
     +                   specT,theta7_reg_CAMT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_Ja(count1),theta7_reg_Ja(count2),
     +                   specT,theta7_reg_JaT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_NZ(count1),theta7_reg_NZ(count2),
     +                   specT,theta7_reg_NZT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_SA(count1),theta7_reg_SA(count2),
     +                   specT,theta7_reg_SAT,iflag)
      call S24_interp (period(count1),period(count2),theta7_reg_Tw(count1),theta7_reg_Tw(count2),
     +                   specT,theta7_reg_TwT,iflag)
      call S24_interp (period(count1),period(count2),theta7_global(count1),theta7_global(count2),
     +                   specT,theta7_globalT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_AL(count1),theta62_reg_AL(count2),
     +                   specT,theta62_reg_ALT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_CAS(count1),theta62_reg_CAS(count2),
     +                   specT,theta62_reg_CAST,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_CAM(count1),theta62_reg_CAM(count2),
     +                   specT,theta62_reg_CAMT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_Ja(count1),theta62_reg_Ja(count2),
     +                   specT,theta62_reg_JaT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_NZ(count1),theta62_reg_NZ(count2),
     +                   specT,theta62_reg_NZT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_SA(count1),theta62_reg_SA(count2),
     +                   specT,theta62_reg_SAT,iflag)
      call S24_interp (period(count1),period(count2),theta62_reg_Tw(count1),theta62_reg_Tw(count2),
     +                   specT,theta62_reg_TwT,iflag)
      call S24_interp (period(count1),period(count2),theta6_global(count1),theta6_global(count2),
     +                   specT,theta6_globalT,iflag)
      call S24_interp (period(count1),period(count2),CasBasinInter(count1),CasBasinInter(count2),
     +                   specT,CasBasinInterT,iflag)
      call S24_interp (period(count1),period(count2),CasBasinSlope(count1),CasBasinSlope(count2),
     +                   specT,CasBasinSlopeT,iflag)
      call S24_interp (period(count1),period(count2),CasSeaResid(count1),CasSeaResid(count2),
     +                   specT,CasSeaResidT,iflag)
      call S24_interp (period(count1),period(count2),JapBasinInter(count1),JapBasinInter(count2),
     +                   specT,JapBasinInterT,iflag)
      call S24_interp (period(count1),period(count2),JapBasinSlope(count1),JapBasinSlope(count2),
     +                   specT,JapBasinSlopeT,iflag)
      call S24_interp (period(count1),period(count2),TaiBasinInter(count1),TaiBasinInter(count2),
     +                   specT,TaiBasinInterT,iflag)
      call S24_interp (period(count1),period(count2),TaiBasinSlope(count1),TaiBasinSlope(count2),
     +                   specT,TaiBasinSlopeT,iflag)
      call S24_interp (period(count1),period(count2),NZBasinInter(count1),NZBasinInter(count2),
     +                   specT,NZBasinInterT,iflag)
      call S24_interp (period(count1),period(count2),NZBasinSlope(count1),NZBasinSlope(count2),
     +                   specT,NZBasinSlopeT,iflag)
      call S24_interp (period(count1),period(count2),dmbInter(count1),dmbInter(count2),
     +                   specT,dmbInterT,iflag)

 1011 period2 = specT

C     Set the regional terms
c     0=Global
c     1=Alaska
c     2=Cascadia
c     3=Central America
c     4=Japan
c     5=New Zealand
c     6=South America
c     7=Taiwan

      if (iRegion .eq. 0) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_globalT
            theta1RegPGA = theta1_global(1)
         else
            theta1RegT = theta1a_globalT
            theta1RegPGA = theta1a_global(1)
         endif
         theta6RegT = theta6_globalT
         theta7RegT = theta7_globalT
         theta6RegPGA = theta6_global(1)
         theta7RegPGA = theta7_global(1)
      elseif (iRegion .eq. 1) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_ALT
            theta1RegPGA = theta1_reg_AL(1)
         else
            theta1RegT = theta1a_reg_ALT
            theta1RegPGA = theta1a_reg_AL(1)
         endif
         theta6RegT = theta62_reg_ALT
         theta7RegT = theta7_reg_ALT
         theta6RegPGA = theta62_reg_AL(1)
         theta7RegPGA = theta7_reg_AL(1)
      elseif (iRegion .eq. 2) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_CasT
            theta1RegPGA = theta1_reg_Cas(1)
         else
            theta1RegT = theta1a_reg_CasT
            theta1RegPGA = theta1a_reg_Cas(1)
         endif
         theta6RegT = theta62_reg_CasT
         theta7RegT = theta7_reg_CasT
         theta6RegPGA = theta62_reg_Cas(1)
         theta7RegPGA = theta7_reg_Cas(1)
      elseif (iRegion .eq. 3) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_CAMT
            theta1RegPGA = theta1_reg_CAM(1)
         else
            theta1RegT = theta1a_reg_CAMT
            theta1RegPGA = theta1a_reg_CAM(1)
         endif
         theta6RegT = theta62_reg_CAMT
         theta7RegT = theta7_reg_CAMT
         theta6RegPGA = theta62_reg_CAM(1)
         theta7RegPGA = theta7_reg_CAM(1)
      elseif (iRegion .eq. 4) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_JaT
            theta1RegPGA = theta1_reg_Ja(1)
         else
            theta1RegT = theta1a_reg_JaT
            theta1RegPGA = theta1a_reg_Ja(1)
         endif
         theta6RegT = theta62_reg_JaT
         theta7RegT = theta7_reg_JaT
         theta6RegPGA = theta62_reg_Ja(1)
         theta7RegPGA = theta7_reg_Ja(1)
      elseif (iRegion .eq. 5) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_NZT
            theta1RegPGA = theta1_reg_NZ(1)
         else
            theta1RegT = theta1a_reg_NZT
            theta1RegPGA = theta1a_reg_NZ(1)
         endif
         theta6RegT = theta62_reg_NZT
         theta7RegT = theta7_reg_NZT
         theta6RegPGA = theta62_reg_NZ(1)
         theta7RegPGA = theta7_reg_NZ(1)
      elseif (iRegion .eq. 6) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_SAT
            theta1RegPGA = theta1_reg_SA(1)
         else
            theta1RegT = theta1a_reg_SAT
            theta1RegPGA = theta1a_reg_SA(1)
         endif
         theta6RegT = theta62_reg_SAT
         theta7RegT = theta7_reg_SAT
         theta6RegPGA = theta62_reg_SA(1)
         theta7RegPGA = theta7_reg_SA(1)
      elseif (iRegion .eq. 7) then
         if (ftype .eq. 0.0) then
            theta1RegT = theta1_reg_TwT
            theta1RegPGA = theta1_reg_Tw(1)
         else
            theta1RegT = theta1a_reg_TwT
            theta1RegPGA = theta1a_reg_Tw(1)
         endif
         theta6RegT = theta62_reg_TwT
         theta7RegT = theta7_reg_TwT
         theta6RegPGA = theta62_reg_Tw(1)
         theta7RegPGA = theta7_reg_Tw(1)
      endif

C     Adjust mbinterface term for Japan and South America
      if (iRegion .eq. 4 .or. iRegion .eq. 6) then
         if (ftype .eq. 0.0) then
            mbInter = mbInter0 + dmbInterT
         endif
      else
         if (ftype .eq. 0.0) then
            mbInter = mbInter0
         endif
      endif

C     Compute the base model including PGARock

      h = 10**(nft_1T+nft_2T*(mag-6.0))
      hpga = 10**(nft_1(1)+nft_2(1)*(mag-6.0))

C     Constant term
      fconst = theta1RegT
      fconstpga = theta1RegPGA

C     f(mag) term
      if (ftype .eq. 0.0) then
         call S35_KBCGLH (mag,mbInter,theta4T*(mbInter-Mref),theta4T,theta5T,deltam,fmag)
         call S35_KBCGLH (mag,mbInter,theta4(1)*(mbInter-Mref),theta4(1),theta5(1),deltam,fmagpga)
      else
         call S35_KBCGLH (mag,mbSlab,theta4aT*(mbslab-Mref),theta4aT,theta5T,deltam,fmag)
         call S35_KBCGLH (mag,mbSlab,theta4a(1)*(mbslab-Mref),theta4a(1),theta5(1),deltam,fmagpga)
      endif

C     f(geom) term
      if (ftype .eq. 0.0) then
         fgeom = (theta2T + theta3T*mag)*alog(Rrup + h)
         fgeompga = (theta2(1) + theta3(1)*mag)*alog(Rrup + hpga)
      else
         fgeom = (theta2aT + theta3T*mag)*alog(Rrup + h)
         fgeompga = (theta2a(1) + theta3(1)*mag)*alog(Rrup + hpga)
      endif

C     f(depth) term
      if (ftype .eq. 0.0) then
         call S35_KBCGLH (ztor,(zbif+delta_zb_ifT),theta9T*(zbif+delta_zb_ifT-Zifref),theta9T,theta10,deltaz,fdepth)
         call S35_KBCGLH (ztor,(zbif+delta_zb_ifT),theta9(1)*(zbif+delta_zb_ifT-Zifref),theta9(1),theta10,deltaz,fdepthpga)
      else
         call S35_KBCGLH (ztor,(zbslab+delta_zb_slabT),theta9aT*(zbslab+delta_zb_slabT-Zslabref),theta9aT,theta10,deltaz,fdepth)
         call S35_KBCGLH (ztor,(zbslab+delta_zb_slabT),theta9a(1)*(zbslab+delta_zb_slabT-Zslabref),theta9a(1),
     +                    theta10,deltaz,fdepthpga)
      endif

C     f(atten) term
C     Currently assumes that R2=Rrup (i.e., does not cross arc region)
      fatten = Rrup*theta6RegT
      fattenpga = Rrup*theta6RegPGA

C     f(site) term
C     For PGA Vs=1100m/s
      fsiteRockpga = (theta7RegPGA + k2(1)*n)*alog(1100.0/k1(1))

C     Compute PGARock
      pgarock = exp(fconstpga + fmagpga + fgeompga + fdepthpga + fattenpga + fsiteRockPga)

C     Now compute full site term
      if (vs30 .gt. k1T) then
         fsite = (theta7RegT + k2T*n)*alog(Vs30/k1T)
         fsitePGA = (theta7RegPGA + k2(1)*n)*alog(Vs30/k1(1))
      else
         fsite =theta7RegT*alog(vs30/k1T) + k2T*(alog(PGArock + c*(vs30/k1T)**n) - alog(PGARock + c))
         fsitePGA =theta7RegPGA*alog(vs30/k1(1)) + k2(1)*(alog(PGArock + c*(vs30/k1(1))**n) - alog(PGARock + c))
      endif

C     Compute the basin term - only for Cascadia, Japan, Taiwan, and New Zealand
      if (iRegion .eq. 2) then
         predZ25 = 8.2940 + (2.3026 - 8.2940) * Exp((alog(Vs30) - 6.3969)/0.2708)/(1 + Exp((alog(Vs30) - 6.3969)/0.2708))
         deltaZ25 = alog(Z25*1000.0) - predZ25
C     Add in Seattle Basin Residual if requested
         if (CasBas .eq. 1) then
            fbasin = CasSeaResidT
            fbasinPGA = CasSeaResid(1)
C     Add in Cascadia non-Seattle Basin Factor not to exceed Seattle Basin Factors
         elseif (CasBas .eq. 2) then
            if ((CasBasinInterT + CasBasinSlopeT * deltaz25) .gt. 0.0) then
               if ( (CasBasinInterT + CasBasinSlopeT * deltaz25) .gt. CasSeaResidT) then
                  fbasin = CasSeaResidT
                  fbasinPGA = CasSeaResid(1)
               else
                  fbasin = CasBasinInterT + CasBasinSlopeT * deltaz25
                  fbasinPGA = CasBasinInter(1) + CasBasinSlope(1) * deltaz25
               endif

           elseif ( (CasBasinInterT + CasBasinSlopeT * deltaz25) .le. 0.0) then
               if ( CasSeaResidT .gt. 0.0) then
                  fbasin = CasBasinInterT + CasBasinSlopeT * deltaz25
                  fbasinPGA = CasBasinInter(1) + CasBasinSlope(1) * deltaz25
               elseif ( (CasBasinInterT + CasBasinSlopeT * deltaz25) .lt. CasSeaResidT ) then
                  fbasin = CasSeaResidT
                  fbasinPGA = CasSeaResid(1)
               else
                  fbasin = CasBasinInterT + CasBasinSlopeT * deltaz25
                  fbasinPGA = CasBasinInter(1) + CasBasinSlope(1) * deltaz25
               endif
           endif
         endif

      elseif (iRegion .eq. 4) then
         predZ25 = 7.6894 + (2.3026 - 7.6894) * Exp((alog(Vs30) - 6.3092)/0.7529)/(1 + Exp((alog(Vs30) - 6.3092)/0.7529))
         deltaZ25 = alog(Z25*1000.0) - predZ25
         fbasin = JapBasinInterT + JapBasinSlopeT * deltaz25
         fbasinPGA = JapBasinInter(1) + JapBasinSlope(1) * deltaz25
      elseif (iRegion .eq. 7) then
         predZ10 = 6.3056 + (2.3026 - 6.3056) * Exp((alog(Vs30) - 6.1105)/0.4367)/(1 + Exp((alog(Vs30) - 6.1105)/0.4367))
         deltaZ10 = alog(Z10*1000.0) - predZ10
         fbasin = TaiBasinInterT + TaiBasinSlopeT * deltaz10
         fbasinPGA = TaiBasinInter(1) + TaiBasinSlope(1) * deltaz10
      elseif (iRegion .eq. 5) then
         predZ10 = 6.8598 + (2.3026 - 6.8598) * Exp((alog(Vs30) - 5.7457)/0.9156)/(1 + Exp((alog(Vs30) - 5.7457)/0.9156))
         deltaZ10 = alog(Z10*1000.0) - predZ10
         fbasin = NZBasinInterT + NZBasinSlopeT * deltaz10
         fbasinPGA = NZBasinInter(1) + NZBasinSlope(1) * deltaz10
      else
         fbasin = 0.0
         fbasinPGA = 0.0
      endif

C     Compute the site specific ground motion
      lnSA = fconst + fmag + fgeom + fdepth + fatten + fsite + fbasin
      lnPGA = fconstPGA + fmagPGA + fgeomPGA + fdepthPGA + fattenPGA + fsitePGA + fbasinPGA

C     Check that PSA > PGA for T < 0.1sec
C     In cases where PSA < PGA, then set PSA = PGA
      if (specT .lt. 0.1) then
         if (lnSA .lt. lnPGA ) then
            lnSA = lnPGA
         endif
      endif

c     Convert units spectral acceleration in gal
      lnSa = lnSa + 6.89

C     Set sigma values to return
      sigma = sqrt(sigPhiT*sigPhiT + sigTauT*sigTauT)
      phi = sigPhiT
      tau = sigTauT

      return
      end

c ----------------------------------------------------------------------
      subroutine S35_kbcgLH (x, x0, a, b0, b1, delta, LH)

C     Kuehn et al. (2019) LH Function

      real x, x0, a, b0, b1, delta, LH

C     For numerical stability for deep slab events check that exp((x-x0)/delta
C         is less than 85.0. If greater set equal to 85.0. These large
C         values do not change the ground motions since depth scaling is constant.
      if ((x - x0)/delta .gt. 85.0) then
         LH = a + b0*(85.0) + (b1 - b0)*delta*alog(1.0 + exp(85.0))
      else
         LH = a + b0*(x - x0) + (b1 - b0)*delta*alog(1.0 + exp((x - x0)/delta))
      endif

      return
      end