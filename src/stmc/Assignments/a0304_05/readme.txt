
A.

MC calculation of the  previous assignent (a0304_04) using xy_ts.f:
(Use lat.p60, lat.d60 and mc.p60.)

em = -1.3211085382 +/- 0.0011951242

B.

L=10 lattice at beta=10.0: (use lat.p10, lat.d10 and mc.p10)

xy_ts0.f: em = -1.9498457443 +/- 0.0000328435
xy_ts.f:  em = -1.9498455162 +/- 0.0000173800

To further investigate this situation, additional runs on the L=10
lattice wer performed: (use mc.p10l, l=large).

With the same statistics as the above beta=10.0 run: (use mc.p20)

beta=20.0, xy_ts0.f: em = -1.7841034855 +/- 0.0000197996
beta=20.0, xy_ts.f:  em = -1.9750823386 +/- 0.0000099811

The program domain.f prepares the plot of the domain wall configuration
after equibrilation with xy_ts0.f. To be plotted with domain.plt.

C.

 10 times the previous statistics: Gauss Q=0.47 from

 bias.f:
 beta,nlink,nd,nla:   1.0000000000      7200  2     60   60
 nequi,nrpt,nmeas:          100000        32     20000
 actm =   0.6610356932 +/-   0.0002066875
 em =    -1.3220713864 +/-   0.0004133750

 xy_ts.f:
 beta,nlink,nd,nla:   1.0000000000      7200  2     60   60
 nequi,nrpt,nmeas:          100000        32     20000
 actm =   0.6612417780 +/-   0.0001942304
 em =    -1.3224835561 +/-   0.0003884609

 100 times the previous statistics: Gauss Q=0.??? from

 bias.f:
 beta,nlink,nd,nla:   1.0000000000      7200  2     60   60
 nequi,nrpt,nmeas:          200000        32    200000
 actm =   0.6609905413 +/-   0.0000726337
 em =    -1.3219810826 +/-   0.0001452673
