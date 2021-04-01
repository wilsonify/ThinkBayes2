      program efficiency
C The Metropolis runs have 4 times the heat bath (hb) statistics.
C Efficiency = tau_int(Metro)/tau_int(hb)*CPU(Metro)/(4*CPU(hb)).
      parameter(iuo=6)
      beta=0.22165 ! =bc
      eff=(98.15/61.7)*16556/(4*5574) ! second
      write(iuo,'("beta,efficiency hb/metro:",F10.5,F9.2)') beta,eff
      beta=0.3
      eff=(5.55/2.12)*16430/(4*5576) ! seconds
      write(iuo,'("beta,efficiency hb/metro:",F10.5,F9.2)') beta,eff
      beta=0.4
      eff=(3.78/1.30)*273/(4*90) ! minutes
      write(iuo,'("beta,efficiency hb/metro:",F10.5,F9.2)') beta,eff
      beta=0.5
      eff=(3.306/1.114)*272/(4*90) ! minutes
      write(iuo,'("beta,efficiency hb/metro:",F10.5,F9.2)') beta,eff
      stop
      end
