# Professional CV

This repository contains my CV/Resume designed for non-academic positions. It uses R functions Quarto, and LaTeX to create a PDF document that pulls data directly from Google Sheets.

## Key Architecture
- **cv-ddcf-professional.qmd**: Main document. Renderized all files into a PDF document.
- **functions.R**: R functions that pull data from the Google Spreadsheet.
- **preamble.tex**: LaTeX preamble for the document, defining styles and formatting.
- **cite-scrape.R**: R script that scrapes Google Scholar for publication data.

# Requirements
- R 4.0 or higher
- Font Awesome
- Quarto
- pandoc
- LaTeX
- A Google Sheet with my/your publications, with public access enabled.
