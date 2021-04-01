      program Hvalue
C Copyright, Bernd Berg, Oct 13, 2001.
C Heat Bath algorithm: MC production runs for energy (action) histograms.
      include '../../ForLib/implicit.sta'
      include '../../ForLib/constants.par'
      include 'potts.par'
      H=half*log((nqm1*one))
      write(6,'(" H    =",F20.10)') H
      actm=half**2+nqm1*(half/nqm1)**2
      write(6,'(" actm =",F20.10)') actm
      stop
      end
