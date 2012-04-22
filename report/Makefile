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
	rm -f $(NAME).pdf
