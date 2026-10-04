.PHONY: all watch build rebuild deploy clean FORCE

CV  := pages/cv.md
PDF := files/CV_Dimitrije_Radojevic.pdf
CSL := csl/ieee-with-url.csl
BIB := bib/refs.bib
TPL := templates/cv-template.tex
# Get Nix path to Texlive, we need to supply path to the FontAwesome otf file to
# the CV template.
LATEX_PATH := $(shell which latex)
TEXLIVE_PATH := $(shell nix-store --query $(LATEX_PATH))
# Records the Texlive path, so the CV is rebuilt when it changes (e.g. after a
# nixpkgs bump). Only touched when the path actually differs.
TEXLIVE_STAMP := templates/.texlive_path

all: clean cv build

# Some aliases for site commands.
watch:
	site watch

build:
	site build

rebuild:
	site rebuild

deploy:
	site deploy

clean:
	site clean
	rm -f "$(PDF)" "$(TPL)_subst" "$(TEXLIVE_STAMP)"

$(TEXLIVE_STAMP): FORCE
	@echo "$(TEXLIVE_PATH)" | cmp -s - $@ || echo "$(TEXLIVE_PATH)" >$@

$(TPL)_subst: $(TPL) $(TEXLIVE_STAMP)
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
