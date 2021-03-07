.PHONY: run build clean

CV  := cv.md
PDF := files/cv.pdf
CSL := csl/ieee-with-url.csl
BIB := bib/refs.bib
TPL := cv-template.tex

run:
	stack run watch

build:
	stack run build

clean:
	stack run clean

$(PDF): $(CV) $(CSL) $(BIB) $(TPL)
	pandoc -s -f markdown-auto_identifiers \
	"$(CV)" \
	-o "$(PDF)" \
	--template="$(TPL)" \
	--bibliography="$(BIB)" \
	--csl="$(CSL)" \
	--pdf-engine=xelatex

cv : $(PDF)
