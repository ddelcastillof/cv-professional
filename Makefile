.PHONY: all build cover-letter clean

all: cv cover-letter

cv:
	Rscript preprocess.R
	pandoc build/cv.md \
		--pdf-engine=xelatex \
		--template=templates/cv-template.tex \
		--lua-filter=filters/bold-author.lua \
		-o DelCastillo_Resume.pdf

cover-letter:
	xelatex -output-directory=cover_letters/output cover_letters/cover_letter.tex
	rm -f cover_letters/output/cover_letter.{aux,log,out}

clean:
	rm -f build/cv.md DelCastillo_Resume.pdf
	rm -f cover_letters/output/cover_letter.pdf cover_letters/output/cover_letter.aux cover_letters/output/cover_letter.log
