if (ext eq "png") { set terminal pngcairo size 1650,570 font "Sans,17" } else { set terminal pdfcairo size 11,3.8 font "Sans,9" }
set output "metrics_by_preprocessing.".ext
set encoding utf8
set border 3 lw 1.2 lc rgb "#444444"
set tics nomirror scale 0.6 textcolor rgb "#333333"
set grid ytics lc rgb "#dddddd" lw 1
set key textcolor rgb "#333333"
$map << EOD
raw 0.2506 0.2544
stop 0.2500 0.2559
stem 0.2722 0.2834
both 0.2722 0.2861
EOD
$p10 << EOD
raw 0.2049 0.2098
stop 0.2067 0.2124
stem 0.2164 0.2169
both 0.2173 0.2182
EOD
$r10 << EOD
raw 0.3361 0.3469
stop 0.3396 0.3540
stem 0.3738 0.3702
both 0.3741 0.3730
EOD
set multiplot layout 1,3 title "Vetorial vs BM25 (k1=1.2, b=0.75) por pré-processamento (both = stopwords + stemming)"
set style data histograms
set style histogram clustered gap 1
set style fill solid 0.9 border rgb "#444444"
set boxwidth 0.9
set title "MAP"
set yrange [0:0.358]
set key top left samplen 1.2
plot $map using 2:xtic(1) title "Vetorial" lc rgb "#4c78a8", '' using 3 title "BM25" lc rgb "#f58518"
set title "P\\@10"
set yrange [0:0.273]
unset key
plot $p10 using 2:xtic(1) title "Vetorial" lc rgb "#4c78a8", '' using 3 title "BM25" lc rgb "#f58518"
set title "R\\@10"
set yrange [0:0.468]
unset key
plot $r10 using 2:xtic(1) title "Vetorial" lc rgb "#4c78a8", '' using 3 title "BM25" lc rgb "#f58518"
unset multiplot
