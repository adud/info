set xlabel "<d_{np}> (veh/case)" offset 0,-1,0
set ylabel "<d_p> (veh/case)" offset 1,0,0
set zlabel "<J_{np}> (veh/tps)" offset 3,1,3
set xyplane at 0
set view 70,50
set key inside right top

splot "localabs.dat" using 4:1:6 ls 1
replot "localdyn.dat" using 4:1:6 ls 2

