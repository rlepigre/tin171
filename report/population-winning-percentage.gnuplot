set terminal pdf
set output "population-winning-percentage.pdf"

#set title "The rate at which the populations win games"
set xlabel "Generation number"
set ylabel "%"
set grid
plot "population-winning-percentage.dat" using 1:2 with lines notitle
