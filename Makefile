.PHONY: build clean

build:
	Rscript preprocess.R
	pandoc build/cv.md \
		--pdf-engine=xelatex \
		--template=templates/cv-template.tex \
		--lua-filter=filters/underline-author.lua \
		-o DelCastillo_Resume.pdf

clean:
	rm -f build/cv.md DelCastillo_Resume.pdf
