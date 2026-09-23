if (ext eq "png") { set terminal pngcairo size 1050,900 font "Sans,17" } else { set terminal pdfcairo size 7,6 font "Sans,9" }
set output "bm25_grid_map.".ext
set encoding utf8
set border 3 lw 1.2 lc rgb "#444444"
set tics nomirror scale 0.6 textcolor rgb "#333333"
set grid ytics lc rgb "#dddddd" lw 1
set key textcolor rgb "#333333"
set multiplot layout 2,2 title "MAP do BM25 por (k1, b)"
unset grid
unset key
set tics scale 0
set xrange [-0.5:2.5]
set yrange [-0.5:2.5]
set xtics ("0" 0, "0.75" 1, "1" 2)
set ytics ("0.5" 0, "1.2" 1, "2.0" 2)
set xlabel "b"
set ylabel "k_1"
set cbrange [0.21701:0.29172]
set palette defined (0 "#f2f7fb", 1 "#1f4e79")
$g0 << EOD
0.2170 0.2404 0.2447
0.2236 0.2544 0.2524
0.2228 0.2604 0.2588
EOD
set title "raw"
plot $g0 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.25436?0xffffff:0x000000) with labels tc rgb variable notitle
$g1 << EOD
0.2223 0.2431 0.2425
0.2302 0.2559 0.2527
0.2294 0.2606 0.2577
EOD
set title "stop"
plot $g1 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.25436?0xffffff:0x000000) with labels tc rgb variable notitle
$g2 << EOD
0.2369 0.2693 0.2730
0.2409 0.2834 0.2813
0.2417 0.2881 0.2866
EOD
set title "stem"
plot $g2 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.25436?0xffffff:0x000000) with labels tc rgb variable notitle
$g3 << EOD
0.2434 0.2689 0.2707
0.2508 0.2861 0.2851
0.2508 0.2917 0.2886
EOD
set title "both"
plot $g3 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.25436?0xffffff:0x000000) with labels tc rgb variable notitle
unset multiplot
