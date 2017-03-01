set xlabel "<d_{np}> (veh/case)"
set ylabel "<d_p> (veh/case)"
set zlabel "<J_{np}> (veh/tps)"
set xyplane at 0

splot "localabs.dat" using 4:1:6 ls 1
replot "localabs2.dat" using 4:1:6 ls 2

