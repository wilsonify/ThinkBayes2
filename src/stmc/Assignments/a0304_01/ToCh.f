      program ToCh
C Combinations of energy values from Tobochnik and Chester,
C                 Phys. Rev. B 20 (1979): Tables I and II.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      parameter(iuo=6,n=2)
      dimension e_s(n),e_e(n),w(n)
      data w/one,one/
      e_s(1)=-0.5470d0
      e_e(1)=+0.0007d0
      e_s(2)=-0.5485d0
      e_e(2)=+0.0005d0
      call steb2(n,e_s,e_e,w,em,eme)
      write(iuo,'(" beta=0.5: em =",F12.6," +/-",F12.6)') em,eme
      e_s(1)=-1.3188d0
      e_e(1)=+0.0023d0
      e_s(2)=-1.3237d0
      e_e(2)=+0.0019d0
      call steb2(n,e_s,e_e,w,em,eme)
      write(iuo,'(" beta=1:   em =",F12.6," +/-",F12.6)') em,eme
      stop
      end

      include '../../ForLib/steb2.f'
