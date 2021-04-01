      subroutine XY_ran(sta,ns)
C Copyright, Bernd Berg, Feb 10, 2002.
C Assigns random (i.e. beta=0) values to the states sta(*,is).
      include 'implicit.sta'
      include 'constants.par'
      dimension sta(2,ns)
      do is=1,ns
        phi=tpi*rmafun()
        sta(1,is)=cos(phi)
        sta(2,is)=sin(phi)
      end do
      return
      end
