
Store permanently needed data sets a fln.D.

Get rid of created data sets with rm *.d.

1. Choose the appropriate subl*.f subroutine in the
   include statements at the end of lfit.f.

2. Copy the data set to fort.10.

3. Compile and run lfit.f

lfit.f   Needs:   Choice of subl*.f (include) and fort.10 data.
         Creates: data.d, plot.d, lfit.d1, lfit.d2, ellipse.d1
                  and ellipse.d2.

         Gnuplots: fit.plt     plots data.d and plot.d.
                   lfit.plt    plots lfit.d1 and lfit.d2.
                   ellipse.plt plots ellipse.d1 and ellipse.d2.


subl_linear.f     Linear fit ilustrated with brandt.D data.
subl_1ox.f        Linear fit in 1/x illustrated with berg.D data.
subl_power.f      Power law a1*x**a2 converted to linear fit in 
                  ln(x) illustrated with bhanot.D data.
subl_exp.f        Exponetial law a1*exp(-a2*x) converted to linear fit.
