rmdfiles = $(shell ls *.Rmd)
htmlfiles = $(rmdfiles:.Rmd=.html)

all: $(htmlfiles)

%.html: %.Rmd
	htmlknitr $<

.PHONY: all
