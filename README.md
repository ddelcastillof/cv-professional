# Professional CV

Professional CV built with R and Pandoc. Content is stored in version-controlled YAML files and a Paperpile-synced BibTeX file — no Google Sheets or internet connection required to build.

## Build

```bash
make build   # preprocess.R → build/cv.md → pandoc + XeLaTeX → DelCastillo_Resume.pdf
make clean   # remove build/cv.md and DelCastillo_Resume.pdf
```

## Requirements

- R with packages: `yaml`, `dplyr`, `stringr`, `testthat`
- Pandoc
- XeLaTeX (with `fontspec`, `fontawesome5`, `longtable`, `titlesec`, `ulem`)

## Structure

```
data/          # one YAML file per CV section (edit here to update content)
bib/           # references.bib synced by Paperpile (do not edit manually)
filters/       # bold-author.lua — bolds author name in publication entries
templates/     # cv-template.tex — Pandoc LaTeX template
tests/         # testthat unit tests for preprocess.R
build/         # generated files (gitignored)
```

## Adding Publications

Paperpile syncs `bib/references.bib` automatically. To make a new entry appear, add its cite key to `peer_reviewed` or `conference` in `data/pub_categories.yaml`.
