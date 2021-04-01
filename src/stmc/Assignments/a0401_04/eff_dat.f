      program eff_dat ! effective numbers of data
C Corrected estimates:
C 2**14 data:
      write(6,'(/," K=14:")')
      del_min=(1.0-0.919)*3.06/2
      del_max=(1.093-1.0)*3.06/2
      tau_min=3.06-del_min
      tau_max=3.06+del_max
      write(6,'(" Old: tau_min,tau_max:",2F10.3)') tau_min,tau_max
      write(6,'("      del_min,del_max:",2F10.3)') del_min,del_max
      del_min=(1.0-0.909)*3.06/2
      del_max=(1.103-1.0)*3.06/2
      tau_min=3.06-del_min
      tau_max=3.06+del_max
      write(6,'(" New: tau_min,tau_max:",2F10.3)') tau_min,tau_max
      write(6,'("      del_min,del_max:",2F10.3)') del_min,del_max
C Numbers needed:
      write(6,'(/," 2**17/4 =",I8)') 2**15
      write(6,'(  " 2**21/4 =",I8)') 2**19
C K=17:
      write(6,'(/," K=17:")')
      del_min=(1.0-0.955)*3.63/2
      del_max=(1.047-1.0)*3.63/2
      tau_min=3.63-del_min
      tau_max=3.63+del_max
      write(6,'(" New: tau_min,tau_max:",2F10.3)') tau_min,tau_max
      write(6,'("      del_min,del_max:",2F10.3)') del_min,del_max
C K=21:
      write(6,'(/," K=21:")')
      del_min=(1.0-0.978)*3.89/2
      del_max=(1.022-1.0)*3.89/2
      tau_min=3.89-del_min
      tau_max=3.89+del_max
      write(6,'(" New: tau_min,tau_max:",2F10.3)') tau_min,tau_max
      write(6,'("      del_min,del_max:",2F10.3)') del_min,del_max
      stop
      end 
