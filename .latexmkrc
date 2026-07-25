# .latexmkrc — build cv.tex with XeLaTeX + biber, aux/output in build/
$pdf_mode = 5;              # xelatex
$out_dir  = 'build';
@default_files = ('cv.tex');
$bibtex_use = 2;            # run biber as needed; latexmk -C removes .bbl
