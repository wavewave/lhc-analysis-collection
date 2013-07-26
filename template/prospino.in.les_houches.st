# SOFTSUSY1.8.4
# B.C. Allanach, Comput. Phys. Commun. 143 (2002) 305-331, hep-ph/0104145
Block SPINFO         # Program information
     1   SOFTSUSY    # spectrum calculator
     2   1.8.4       # version number
Block MODSEL  # Select model
     1    1   # sugra
Block SMINPUTS   # Standard Model inputs
     1    1.27934000e+02   # alpha_em^(-1)(MZ) SM MSbar
     2    1.16637000e-05   # G_Fermi
     3    1.17200000e-01   # alpha_s(MZ)MSbar
     4    9.11876000e+01   # MZ(pole)
     5    4.25000000e+00   # Mb(mb)
     6    1.74300000e+02   # Mtop(pole)
     7    1.77700000e+00   # Mtau(pole)
Block MINPAR  # SUSY breaking input parameters
     3    1.00000000e+01   # tanb
     4    1.00000000e+00   # sign(mu)
     1    1.00000000e+02   # m0
     2    2.50000000e+02   # m12
     5   -1.00000000e+02   # A0
# Low energy data in SOFTSUSY: MIXING=1 TOLERANCE=1.00000000e-03
# mgut=2.55129987e+16 GeV
Block MASS   # Mass spectrum
#PDG code      mass              particle
        24     8.02463984e+01   # MW
        25     1.10636832e+02   # h0
        35     4.00874604e+02   # H0
        36     4.00506272e+02   # A0
        37     2.00000000e+02   # H+
   1000001     $msquark$  # ~d_L
   1000002     $msquark$  # ~u_L
   1000003     $msquark$  # ~s_L
   1000004     $msquark$  # ~c_L
   1000005     $msbottom$  # ~b_1
   1000006     $mstop$     # ~t_1
   1000011     $mslepton$   # ~e_L
   1000012     $mslepton$   # ~nue_L
   1000013     $mslepton$   # ~mu_L
   1000014     $mslepton$   # ~numu_L
   1000015     $mstau$   # ~stau_1
   1000016     $mstau$   # ~nu_tau_L
   1000021     $mgluino$  # ~g
   1000022     $mneutralino$  # ~neutralino(1)
   1000023     $mneuttwo$   # ~neutralino(2)
   1000024     $mchargino$  # ~chargino(1)
   1000025     $mneuttwo$   # ~neutralino(3)
   1000035     $mneuttwo$   # ~neutralino(4)
   1000037     $mcharginotwo$  # ~chargino(2)
   2000001     $msquark$   # ~d_R
   2000002     $msquark$   # ~u_R
   2000003     $msquark$   # ~s_R
   2000004     $msquark$   # ~c_R
   2000005     $msbottom$   # ~b_2
   2000006     $mstop$   # ~t_2
   2000011     $mslepton$   # ~e_R
   2000013     $mslepton$   # ~mu_R
   2000015     $mstau$   # ~stau_2
# Higgs mixing
Block alpha     # Effective Higgs mixing parameter
          -1.13716828e-01   # alpha
Block stopmix  # stop mixing matrix
  1  1     5.37975094e-01   # O_{11}
  1  2     8.42960733e-01   # O_{12}
  2  1     8.42960733e-01   # O_{21}
  2  2    -5.37975094e-01   # O_{22}
Block sbotmix  # sbottom mixing matrix
  1  1     9.47346882e-01   # O_{11}
  1  2     3.20209128e-01   # O_{12}
  2  1    -3.20209128e-01   # O_{21}
  2  2     9.47346882e-01   # O_{22}
Block staumix  # stau mixing matrix
  1  1     2.78399839e-01   # O_{11}
  1  2     9.60465267e-01   # O_{12}
  2  1     9.60465267e-01   # O_{21}
  2  2    -2.78399839e-01   # O_{22}
Block nmix  # neutralino mixing matrix
  1  1     9.86102610e-01   # N_{1,1}
  1  2    -5.46971979e-02   # N_{1,2}
  1  3     1.47526998e-01   # N_{1,3}
  1  4    -5.33445802e-02   # N_{1,4}
  2  1     1.01818619e-01   # N_{2,1}
  2  2     9.43310250e-01   # N_{2,2}
  2  3    -2.73948058e-01   # N_{2,3}
  2  4     1.57325147e-01   # N_{2,4}
  3  1    -6.06211640e-02   # N_{3,1}
  3  2     9.00367885e-02   # N_{3,2}
  3  3     6.95440071e-01   # N_{3,3}
  3  4     7.10339045e-01   # N_{3,4}
  4  1    -1.16446066e-01   # N_{4,1}
  4  2     3.14749686e-01   # N_{4,2}
  4  3     6.47727839e-01   # N_{4,3}
  4  4    -6.83974850e-01   # N_{4,4}
Block Umix  # chargino U mixing matrix 
  1  1     9.16207706e-01   # U_{1,1}
  1  2    -4.00703680e-01   # U_{1,2}
  2  1     4.00703680e-01   # U_{2,1}
  2  2     9.16207706e-01   # U_{2,2}
Block Vmix  # chargino V mixing matrix 
  1  1     9.72887524e-01   # V_{1,1}
  1  2    -2.31278762e-01   # V_{1,2}
  2  1     2.31278762e-01   # V_{2,1}
  2  2     9.72887524e-01   # V_{2,2}
Block gauge Q= 4.64649125e+02  
     1     3.60872342e-01   # g'(Q)MSSM DRbar
     2     6.46479280e-01   # g(Q)MSSM DRbar
     3     1.09623002e+00   # g3(Q)MSSM DRbar
Block yu Q= 4.64649125e+02  
  3  3     8.88194465e-01   # Yt(Q)MSSM DRbar
Block yd Q= 4.64649125e+02  
  3  3     1.40135884e-01   # Yb(Q)MSSM DRbar
Block ye Q= 4.64649125e+02  
  3  3     9.97405356e-02   # Ytau(Q)MSSM DRbar
Block hmix Q= 4.64649125e+02  # Higgs mixing parameters
     1     3.58660361e+02   # mu(Q)MSSM DRbar
     2     3.00000000e+01   # tan beta(Q)MSSM DRbar
     3     2.44923506e+02   # higgs vev(Q)MSSM DRbar
     4     1.69697051e+04   # mA^2(Q)MSSM DRbar
Block msoft Q=4.64649125e+02 # MSSM DRbar SUSY breaking parameters
     1     1.01353084e+02   # M_1(Q)
     2     1.91513233e+02   # M_2(Q)
     3     5.86951218e+02   # M_3(Q)
    31     1.99111011e+02   # meL(Q)
    32     1.99108212e+02   # mmuL(Q)
    33     1.98291304e+02   # mtauL(Q)
    34     1.38808102e+02   # meR(Q)
    35     1.38800070e+02   # mmuR(Q)
    36     1.36441129e+02   # mtauR(Q)
    41     5.51249170e+02   # mqL1(Q)
    42     5.51173571e+02   # mqL2(Q)
    43     4.99839787e+02   # mqL3(Q)
    44     5.29285249e+02   # muR(Q)
    45     5.29130286e+02   # mcR(Q)
    46     4.19025924e+02   # mtR(Q)
    47     5.26529352e+02   # mdR(Q)
    48     5.26527024e+02   # msR(Q)
    49     5.23183913e+02   # mbR(Q)
Block au Q= 4.64649125e+02  
  1  1    -6.68742117e+02   # Au(Q)MSSM DRbar
  2  2    -6.34974581e+02   # Ac(Q)MSSM DRbar
  3  3    -5.04995511e+02   # At(Q)MSSM DRbar
Block ad Q= 4.64649125e+02  
  1  1    -8.65541613e+02   # Ad(Q)MSSM DRbar
  2  2    -8.65439235e+02   # As(Q)MSSM DRbar
  3  3    -7.97992485e+02   # Ab(Q)MSSM DRbar
Block ae Q= 4.64649125e+02  
  1  1    -2.58042681e+02   # Ae(Q)MSSM DRbar
  2  2    -2.58036833e+02   # Amu(Q)MSSM DRbar
  3  3    -2.56328558e+02   # Atau(Q)MSSM DRbar
