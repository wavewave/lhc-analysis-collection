set xlabel "$xvar$ Mass (GeV)"
set ylabel "$yvar$ Mass (GeV)"
set xrange [$xmin$:$xmax$] 
set yrange [$ymin$:$ymax$]
set zrange [-12:8]
unset key 
unset surface
set contour base
set cntrparam level incremental -20, 20, 20
unset surface
set table '$datname$_contour.dat'
splot '$datname$_mesh.dat' using 1:2:(log (\$3)) with lines lw 1.5 lc rgb "#202020"



