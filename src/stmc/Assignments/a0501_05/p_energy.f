      program p_energy ! Copyright, Bernd Berg, June 25 2002.
C Purpose: Check of the Potts model energy conventions.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,nd=2,nq=10)

      e0s_c=-2.6324555d00
      del_e0s=1.3921d00
      e0s_d=e0s_c+half*del_e0s
      e0s_o=e0s_c-half*del_e0s
      write(iuo,'(" e0s_0,e0s_d:",2F12.4)') e0s_d,e0s_o

      act_d=0.4328d00
      act_o=0.8638d00
      e0s_d=nd*two*((one/nq)-act_d)
      e0s_o=nd*two*((one/nq)-act_o)
      write(iuo,'(" e0s_0,e0s_d:",2F12.4)') e0s_d,e0s_o

      act_d=0.4328d00
      act_o=0.8638d00
      e0s_d=nd*two*(-act_d)
      e0s_o=nd*two*(-act_o)
      write(iuo,'(" e0s_0,e0s_d:",2F12.4)') e0s_d,e0s_o

      stop
      end

