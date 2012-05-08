set terminal pdf
set output "population-winning-percentage.pdf"

#set title "The rate at which the populations win games"
set xlabel "Generation number"
set ylabel "%"
set grid
set key right bottom
plot [0:4000] "population-winning-percentage.dat" using 1:2 with lines title "Two-player game", \
  "population-winning-percentage3.dat" using 1:2 with lines title "Three-player game"
