if (ext eq "png") { set terminal pngcairo size 1050,900 font "Sans,17" } else { set terminal pdfcairo size 7,6 font "Sans,9" }
set output "bm25_grid_ndcg_10.".ext
set encoding utf8
set border 3 lw 1.2 lc rgb "#444444"
set tics nomirror scale 0.6 textcolor rgb "#333333"
set grid ytics lc rgb "#dddddd" lw 1
set key textcolor rgb "#333333"
set multiplot layout 2,2 title "NDCG\\@10 do BM25 por (k1, b)"
unset grid
unset key
set tics scale 0
set xrange [-0.5:2.5]
set yrange [-0.5:2.5]
set xtics ("0" 0, "0.75" 1, "1" 2)
set ytics ("0.5" 0, "1.2" 1, "2.0" 2)
set xlabel "b"
set ylabel "k_1"
set cbrange [0.27076:0.35381]
set palette defined (0 "#f2f7fb", 1 "#1f4e79")
$g0 << EOD
0.2708 0.3001 0.3096
0.2788 0.3182 0.3175
0.2800 0.3275 0.3203
EOD
set title "raw"
plot $g0 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.31229?0xffffff:0x000000) with labels tc rgb variable notitle
$g1 << EOD
0.2802 0.3067 0.3079
0.2874 0.3224 0.3194
0.2869 0.3248 0.3205
EOD
set title "stop"
plot $g1 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.31229?0xffffff:0x000000) with labels tc rgb variable notitle
$g2 << EOD
0.2929 0.3297 0.3359
0.2949 0.3446 0.3422
0.2990 0.3479 0.3478
EOD
set title "stem"
plot $g2 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.31229?0xffffff:0x000000) with labels tc rgb variable notitle
$g3 << EOD
0.2980 0.3317 0.3315
0.3067 0.3481 0.3452
0.3100 0.3538 0.3510
EOD
set title "both"
plot $g3 matrix using 1:2:3 with image notitle, '' matrix using 1:2:(sprintf("%.3f",$3)):($3>0.31229?0xffffff:0x000000) with labels tc rgb variable notitle
unset multiplot
