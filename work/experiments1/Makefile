rmdfiles = $(shell ls *.Rmd)
htmlfiles = $(rmdfiles:.Rmd=.html)

all: $(htmlfiles)

%.html: %.Rmd
	htmlknitr $<

clean:
	rm -f *.md *.html

clobber:
	rm -rf cache/ figure/

.PHONY: all clean clobber
