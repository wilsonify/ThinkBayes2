      subroutine XY_ran1(sta,ns)
C Copyright, Bernd Berg, Feb 10 2002.
C Assigns random (i.e. beta=0) values 0,..,nq-1 to the states ista(is).
      include 'implicit.sta'
      include 'constants.par'
      dimension sta(ns)
      do is=1,ns
        sta(is)=tpi*rmafun()
      end do
      return
      end
