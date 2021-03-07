.PHONY: all watch build clean deploy

CV  := cv.md
PDF := files/cv.pdf
CSL := csl/ieee-with-url.csl
BIB := bib/refs.bib
TPL := cv-template.tex

all: clean cv build

watch:
	stack run watch

build:
	stack run build

clean:
	stack run clean
	rm -f "$(PDF)"

deploy:
	stack run deploy

$(PDF): $(CV) $(CSL) $(BIB) $(TPL)
	pandoc -s -f markdown-auto_identifiers \
	"$(CV)" \
	-o "$(PDF)" \
	--template="$(TPL)" \
	--bibliography="$(BIB)" \
	--csl="$(CSL)" \
	--pdf-engine=xelatex

cv : $(PDF)
