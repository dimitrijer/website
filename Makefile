.PHONY: all watch build clean deploy

CV  := cv.md
PDF := files/CV_Dimitrije_Radojevic.pdf
CSL := csl/ieee-with-url.csl
BIB := bib/refs.bib
TPL := cv-template.tex

all: clean cv build

watch:
	site watch

build:
	site build

rebuild:
	site rebuild

clean:
	site clean
	rm -f "$(PDF)"

deploy:
	site deploy

$(PDF): $(CV) $(CSL) $(BIB) $(TPL)
	pandoc -s -f markdown-auto_identifiers \
	"$(CV)" \
	-o "$(PDF)" \
	--template="$(TPL)" \
	--bibliography="$(BIB)" \
	--citeproc \
	--csl="$(CSL)" \
	--pdf-engine=xelatex

cv : $(PDF)
