      subroutine On_ran(sta,n,ns)
C Copyright, Bernd Berg, Feb 10 2002.
C Assigns random (i.e. beta=0) values to the states ista(is).
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension sta(n,ns)
      do is=1,ns
1       sum=zero
          do i=1,n
            sta(i,is)=two*(rmafun()-half)
            sum=sum+sta(i,is)**2
          end do
        if(sum>one) go to 1
        fn=one/sqrt(sum)
        do i=1,n
          sta(i,is)=fn*sta(i,is)
        end do
      end do
      return
      end
