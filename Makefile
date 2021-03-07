.PHONY: site preview

site:
	stack run build

preview:
	stack run preview

cv: cv.md
	pandoc -s -f markdown-auto_identifiers \
	cv.md \
	-o "files/cv.pdf" \
	--template="cv-template.tex" \
	--bibliography="bib/refs.bib" \
	--csl="csl/ieee-with-url.csl" \
	--pdf-engine=xelatex
