set terminal pngcairo dashed
set output '$modelname$$mass$_0lep_sqsg_8TeV.png'

set xlabel "Gluino Mass (GeV)"
set ylabel "Squark Mass (GeV)"

set xrange [$xmin$:$xmax$]
set yrange [$ymin$:$ymax$]
#set zrange [-14:8]
set log cb 
set key 

unset surface
set view map 
set palette rgbformulae 33,13,10

plot '$modelname$$mass$_sqsg_8TeV_0lep.dat' u 1:2:(\$3) with image, '$modelname$$mass$_0lep_sqsg_8TeV_contour.dat' u 1:2 w l lt -1 lc rgb "black" lw 1.5 title "$modelalias$" , 'simplifiedsusy$mass$_0lep_sqsg_8TeV_contour.dat' u 1:2 w l lt 2 lc rgb "black" lw 1.5 title "Simplified"




