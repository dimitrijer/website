.PHONY: site preview

run:
	stack run watch

build:
	stack run build

cv: cv.md
	pandoc -s -f markdown-auto_identifiers \
	cv.md \
	-o "files/cv.pdf" \
	--template="cv-template.tex" \
	--bibliography="bib/refs.bib" \
	--csl="csl/ieee-with-url.csl" \
	--pdf-engine=xelatex
