
on_ts.f      Simple Metropolis code for the O(n) model (inefficient).

sphere.f     Volume occupied by the nd-dimensional unit sphere in
             the nd-dimensional cube.
xy_ts.f   Metropolis code with self-adjusting acceptance rate to 1/2.
          Storage: X, Y in sta(2,ns).
          
xy_ts0.f  Faster version of xy_ts.f for the disoredered region where
          the acceptance rate of random proposals is >= 1/2.
          Storage: X, Y in sta(2,ns).
         
xy_ts1.f  Gives results identical with xy_ts0.f. Uses only one
          angle. Smaller Storage sta(ns), but slower code.
