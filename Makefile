.PHONY: all build cover-letter clean check

all: build cover-letter

build:
	latexmk
	cp build/cv.pdf DelCastillo_Resume.pdf

cover-letter:
	xelatex -output-directory=cover_letters/output cover_letters/cover_letter.tex
	xelatex -output-directory=cover_letters/output cover_letters/cover_letter.tex
	rm -f cover_letters/output/cover_letter.{aux,log,out}

check: build
	@txt=$$(pdftotext DelCastillo_Resume.pdf - | tr -s '[:space:]' ' '); \
	for s in \
	  "EDUCATION" \
	  "ADDITIONAL EDUCATION" \
	  "SKILLS" \
	  "PROFESSIONAL EXPERIENCE" \
	  "LICENSURE AND CERTIFICATION" \
	  "PUBLICATIONS" \
	  "Peer-Reviewed Publications" \
	  "An Fac med" \
	  "Int. J. Epidemiol" \
	  "BMJ Glob. Health" \
	  "Am. J. Epidemiol" \
	  "Endocrinol. Metab" \
	  "Del Castillo" \
	  "Among authors: Del Castillo" \
	; do \
	  echo "$$txt" | grep -qF "$$s" || { echo "make check FAIL — missing: $$s"; exit 1; }; \
	done; \
	n=$$(pdftotext DelCastillo_Resume.pdf - | grep -cE '^[0-9]+\.'); \
	[ "$$n" -ge 6 ] || { echo "make check FAIL — publication count $$n < 6"; exit 1; }; \
	echo "make check: all assertions pass"

clean:
	latexmk -C
	rm -f DelCastillo_Resume.pdf
	rm -f cover_letters/output/cover_letter.{pdf,aux,log,out}
