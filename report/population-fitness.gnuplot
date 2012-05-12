set terminal pdf
set output "population-fitness.pdf"

#set title "Fitness of evolved populations"
set xlabel "Generation number"
set ylabel "Fitness"
set grid
set key right bottom
plot [0:10000] "population-fitness4.dat" using 1:2 with lines title "Four-player game", \
  "population-fitness3.dat" using 1:2 with lines title "Three-player game", \
 "population-fitness.dat" using 1:2 with lines title "Two-player game"
