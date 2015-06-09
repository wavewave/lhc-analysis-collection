set term png 
set output "partonxsec.png"




plot "xsecptnMA400.0GA11.82.dat" using 1:2 lt 1 lc 2 with lines,  \
     "myresultqcd.dat" using 1:2 lt 1 lc rgb "black" with lines
