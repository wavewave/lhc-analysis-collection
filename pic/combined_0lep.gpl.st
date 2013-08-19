set terminal pngcairo dashed
set output '$figurefilename$.png'

set xlabel "$xvar$ Mass (GeV)"
set ylabel "$yvar$ Mass (GeV)"

set xrange [$xmin$:$xmax$]
set yrange [$ymin$:$ymax$]
set cbrange [1e-6:1e2]
set logscale cb  
set cbtics (1e-6,1e-5, 1e-4, 1e-3, 1e-2, 1e-1, 1, 10, 100) 
set key 

unset surface
set view map 

theta(x) = x<0 ? 0 : 1

logval(x) = (log(x+0.00001)-log(1e-4)) / (log(1e3) - log(1e-4))
interpolate(x,y0,x0,y1,x1) = (x-x0)*(y1-y0)/(x1-x0)+y0

base(x) = (x <= -0.75) ? 0 : ( x <= -0.25 ? interpolate(x,0.0,-0.75,1.0,-0.25) : (x <= 0.25 ? 1.0 : (x <= 0.75 ? interpolate(x,1.0,0.25,0.0,0.75) : 0.0 )))

r(x)= base(x-0.55)*theta(x-0.01)+1.0*theta(0.01-x)
g(x)= 0.5*base(x-0)*theta(x-0.01)+0.2+1.0*theta(0.01-x)
b(x)= base(x+0.15)*theta(x-0.01)+1.0*theta(0.01-x)

set palette model RGB functions r(gray),g(gray),b(gray)

plot '$datnameA$_mesh.dat' u 1:2:(\$3) with image title "", '$datnameA$_contour.dat' u 1:2 w l lt -1 lc rgb "black" lw 1.5 title "$modelaliasA$" , '$datnameB$_contour.dat' u 1:2 w l lt 2 lc rgb "black" lw 1.5 title "$modelaliasB$"




