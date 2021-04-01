      program p_f_as ! Copyright, Bernd Berg, June 25 2002.
C Potts model, beta large asymptotic free energy.
      include '../../ForLib/implicit.sta'
      character*2 cq
      include '../../ForLib/constants.par'
      parameter(iuo=6,iud0=10,nd=2,nq=02,nl=20, n=nl**nd)
      parameter(nbeta=200,beta_min=half,beta_max=one)

      write(cq,'(i2.2)') nq
      open(iud0,file="f_as"//cq//".d",form='formatted',status='unknown')
      do ibeta=0,nbeta
        beta=beta_min+ibeta*(beta_max/nbeta)
        f_as=-two*nd+(two*nd)/nq-log(one*nq)/(beta*n)
        write(iud0,'(2F15.6)') beta,f_as
      end do
      close(iud0)

      stop
      end

