set term png 
set output "partonxsec.png"




plot "xsecptnMH400.0GH2.97.dat" using 1:2 lt 1 lc 2 with lines,  \
     "xsecptnMH500.0GH11.1.dat" using 1:2 lt 1 lc 3 with lines,  \
     "myresultqcd.dat" using 1:2 lt 1 lc rgb "black" with lines
