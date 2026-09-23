if (ext eq "png") { set terminal pngcairo size 1350,570 font "Sans,17" } else { set terminal pdfcairo size 9,3.8 font "Sans,9" }
set output "bm25_b_effect.".ext
set encoding utf8
set border 3 lw 1.2 lc rgb "#444444"
set tics nomirror scale 0.6 textcolor rgb "#333333"
set grid ytics lc rgb "#dddddd" lw 1
set key textcolor rgb "#333333"
$m << EOD
0 0.2434 0.2508 0.2508 0.2722
0.75 0.2689 0.2861 0.2917 0.2722
1 0.2707 0.2851 0.2886 0.2722
EOD
$n << EOD
0 0.2980 0.3067 0.3100 0.3279
0.75 0.3317 0.3481 0.3538 0.3279
1 0.3315 0.3452 0.3510 0.3279
EOD
set multiplot layout 1,2 title "Efeito de b no BM25 (stopwords+stemming)"
set xlabel "b"
set xrange [-0.1:1.1]
set xtics (0, 0.75, 1)
set key bottom right
set yrange [*:*]
set title "MAP"
plot $m using 1:2 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#4c78a8" title "k1=0.5", \
     $m using 1:3 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#f58518" title "k1=1.2", \
     $m using 1:4 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#54a24b" title "k1=2", \
     $m using 1:5 with lines dt 2 lw 2 lc rgb "#444444" title "vetorial"
set title "NDCG\\@10"
plot $n using 1:2 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#4c78a8" title "k1=0.5", \
     $n using 1:3 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#f58518" title "k1=1.2", \
     $n using 1:4 with linespoints pt 7 ps 1.2 lw 2 lc rgb "#54a24b" title "k1=2", \
     $n using 1:5 with lines dt 2 lw 2 lc rgb "#444444" title "vetorial"
unset multiplot
