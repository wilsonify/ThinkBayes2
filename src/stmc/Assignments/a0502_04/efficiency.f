      program efficiency ! Heat bath / EDS
      parameter(iuo=6)
c
      write(iuo,'(/," Laptop 1.1 GHz cygwin g77:")')
      beta=0.22165 ! =bc
      eff=(61.7/120.5)*(11.1/63.1)/0.3243
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.3
      eff=(2.12/4.25)*(10.3/64.2)/0.0833
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.4
      eff=(1.30/2.70)*(10.0/63.0)/0.0201
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.5
      eff=(1.11/2.38)*(10.0/63.0)/0.00543
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.6
      eff=(1.05/2.25)*(10.0/63.0)/0.00156
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
c
      write(iuo,'(/," Lnx20 400 MHz Suse Linux g77:")')
      beta=0.22165 ! =bc
      eff=(61.7/120.5)*(33.7/253)/0.3243
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.3
      eff=(2.12/4.25)*(31.6/252)/0.0833
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.4
      eff=(1.30/2.70)*(31.3/244)/0.0202
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.5
      eff=(1.11/2.38)*(31.3/235)/0.00543
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      beta=0.6
      eff=(1.05/2.25)*(31.3/235)/0.00156
      write(iuo,'("beta,efficiency EDS/hb:",F10.5,F9.2)') beta,eff
      stop
      end
