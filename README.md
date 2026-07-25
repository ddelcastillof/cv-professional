# Professional CV

Professional résumé built directly with XeLaTeX and biber via latexmk. Content is stored in version-controlled LaTeX files and a Paperpile-synced BibTeX file — no Google Sheets or internet connection required to build.

## Build

Requires TeX Live (XeLaTeX, biber, `biblatex-vancouver`, `fontspec`,
`fontawesome5`, `titlesec`, `enumitem`, `xstring`) and poppler
(`pdftotext`) for `make check`.

```bash
make build          # latexmk: XeLaTeX + biber → build/cv.pdf → DelCastillo_Resume.pdf
make check          # build + smoke-test the PDF text
make cover-letter   # xelatex → cover_letters/output/cover_letter.pdf
make clean          # remove build/ artifacts and DelCastillo_Resume.pdf
```

Content lives in `cv.tex` (preamble) + `content/*.tex`, one file per
section, `\input` in this order: header, education, skills, experience,
certifications, publications.

There is no preprocessing layer — `content/*.tex` is raw LaTeX, so normal
escaping rules apply: `&`, `%`, `_`, `#`, `~` must be written `\&`, `\%`,
`\_`, `\#`, `\textasciitilde`.

The previous R + Pandoc pipeline lives on branch `legacy/academic`.

## Structure

```
cv.tex          # preamble: packages, bibliography setup, section order
content/        # one .tex file per section, \input by cv.tex
bib/            # references.bib synced by Paperpile (do not edit manually)
cover_letters/  # standalone cover letter, built by its own target
.latexmkrc      # latexmk config: XeLaTeX + biber, output to build/
Makefile        # build / check / cover-letter / clean targets
build/          # generated files (gitignored)
```

## Adding Publications

Paperpile syncs `bib/references.bib` automatically. To make a new entry
appear in the résumé, add a `\cvpub{<category>}{<citekey>}` line to
`content/publications.tex` — category is one of the
`\DeclareBibliographyCategory` names declared in `cv.tex` (`peerreviewed`,
`conference`) — then `make build`. Line order is display order. An unknown
cite key warns in `build/cv.blg` rather than failing the build, so run
`make check`.
