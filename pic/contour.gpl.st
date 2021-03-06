set xlabel "Gluino Mass (GeV)"
set ylabel "Squark Mass (GeV)"
set xrange [$xmin$:$xmax$] 
set yrange [$ymin$:$ymax$]
set zrange [-12:8]
unset key 
unset surface
set contour base
set cntrparam level incremental -20, 20, 20
unset surface
set table '$modelname$$mass$_0lep_sqsg_8TeV_contour.dat'
splot '$modelname$$mass$_sqsg_8TeV_0lep.dat' using 1:2:(log (\$3)) with lines lw 1.5 lc rgb "#202020"



