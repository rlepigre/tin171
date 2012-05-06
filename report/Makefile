#
# Makefile for latex pdf generation.
#

NAME = group1Report

all: gen clean

gen: $(NAME).tex
	rubber --inplace --maxerr -1 --short --force --warn all --pdf $(NAME).tex

clean:
	rm -f *~ $(NAME).aux $(NAME).log $(NAME).toc $(NAME).blg $(NAME).bbl

distclean: clean
	rm -f $(NAME).pdf population-fitness.pdf population-winning-percentage.pdf population-fitness.dat population-winning-percentage.dat

$(NAME).tex: population-fitness.pdf population-winning-percentage.pdf

population-fitness.dat: genetic-data.txt
	grep ^Generation genetic-data.txt | sed 's/^Generation \(.*\): Out.*survivor: (\(.*\) \..*/ \1 \2/g'  > population-fitness.dat

population-winning-percentage.dat: genetic-data.txt
	grep ^Generation genetic-data.txt | sed 's/^Generation \(.*\): Out.*That is \(.*\)% wins.*/ \1 \2/g'  > population-winning-percentage.dat

population-fitness.pdf: population-fitness.dat population-fitness.gnuplot
	gnuplot population-fitness.gnuplot

population-winning-percentage.pdf: population-winning-percentage.dat population-winning-percentage.gnuplot
	gnuplot population-winning-percentage.gnuplot
