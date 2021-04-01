      SUBROUTINE READ_STEB0(nrpt,iud,norm,nlink,ha,ham,hae,act)
C Copyright, Bernd Berg, Jan 13, 2002.
C Reads nrpt times the array ha() from unit iud and calculates the
C       histogram mean values ham() and their error bars hae().
C Further, the nrpt action variable averages are calculated.
C norm=0:  no normalization of the ha histogram, otherwise normalization.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      dimension ha(0:nlink),ham(0:nlink),hae(0:nlink),act(nrpt)
C
      call razero(ham,0,nlink)
      do irpt=1,nrpt
        read(iud) ha
	if(norm.ne.0) then ! Normalization of ha() to hasum-nlink.
          hasum=zero
          do ilink=0,nlink
            hasum=hasum+ha(ilink)
          end do
          factor=nlink/hasum
          do ilink=0,nlink ! Normalization of ha() to hasum=nlink.
            ha(ilink)=factor*ha(ilink)
          end do
        end if
        act(irpt)=potts_actm(nlink,ha) ! Mean action variable.
        do ilink=0,nlink
          ham(ilink)=ham(ilink)+ha(ilink)
        end do
      end do
      do ilink=0,nlink
        ham(ilink)=ham(ilink)/nrpt
      end do
C
      do irpt=1,nrpt
        backspace iud
      end do
C
      call razero(hae,0,nlink)
      do irpt=1,nrpt
        read(iud) ha
        do ilink=0,nlink
          hae(ilink)=hae(ilink)+(ha(ilink)-ham(ilink))**2
        end do
      end do
      do ilink=0,nlink
        hae(ilink)=sqrt(hae(ilink)/(nrpt*(nrpt-1)))
      end do
C
      return
      end
