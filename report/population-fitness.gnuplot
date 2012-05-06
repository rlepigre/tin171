set terminal pdf
set output "population-fitness.pdf"

#set title "Fitness of evolved populations"
set xlabel "Generation number"
set ylabel "Fitness"
set grid
plot "population-fitness.dat" using 1:2 with lines notitle
