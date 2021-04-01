      program p_beta_c
c Critical temperatures of 2d q-state potts models.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6)
      q=two
      beta_c=half*log(one+sqrt(q))
      write(iuo,'(" q,beta_c:",F6.0,F18.12)') q,beta_c
      stop
      end
