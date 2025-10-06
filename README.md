# Professional CV

This repository contains my CV/Resume designed for non-academic positions. It uses R functions, Quarto, and LaTeX to create a PDF document that pulls data directly from Google Sheets.

## Key Architecture

- **cv-ddcf-professional.qmd**: Main document. Renders all files into a PDF document.
- **functions.R**: R functions that pull data from the Google Spreadsheet.
- **preamble.tex**: LaTeX preamble for the document, defining styles and formatting.
- If needed, my publications can be found in BibTeX format in the **bib** folder. This folder was imported using [Paperpile](https://paperpile.com/)

# Requirements

- R 4.0 or higher
- Font Awesome
- Quarto
- pandoc
- LaTeX
- A Google Sheet with your publications (private access)
