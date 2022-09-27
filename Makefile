.PHONY: all watch build clean deploy

CV  := cv.md
PDF := files/CV_Dimitrije_Radojevic.pdf
CSL := csl/ieee-with-url.csl
BIB := bib/refs.bib
TPL := cv-template.tex
# Get Nix path to Texlive, we need to supply path to the FontAwesome otf file to
# the CV template.
LATEX_PATH := $(shell which latex)
TEXLIVE_PATH := $(shell nix-store --query $(LATEX_PATH))

all: clean cv build

watch:
	site watch --port 8082

build:
	site build

rebuild:
	site rebuild

clean:
	site clean
	rm -f "$(PDF)" "$(TPL)_subst"

deploy:
	site deploy

$(TPL)_subst: $(TPL)
	sed "s#TEXLIVE_PATH#$(TEXLIVE_PATH)#" $(TPL) >$(TPL)_subst

$(PDF): $(CV) $(CSL) $(BIB) $(TPL)_subst
	pandoc -s -f markdown-auto_identifiers \
	"$(CV)" \
	-o "$(PDF)" \
	--template="$(TPL)_subst" \
	--bibliography="$(BIB)" \
	--citeproc \
	--csl="$(CSL)" \
	--pdf-engine=xelatex

cv : $(PDF)
