# preprocess.R
# CV preprocessing: reads data/*.yaml + bib/references.bib -> build/cv.md

library(yaml)
library(dplyr)
library(stringr)

# ── Helper functions ──────────────────────────────────────────────────────────

na_to_empty <- function(x) {
  if (is.null(x) || (length(x) == 1 && is.na(x))) "" else as.character(x)
}

format_author_vancouver <- function(author_str) {
  parts    <- strsplit(author_str, ", ", fixed = TRUE)[[1]]
  if (length(parts) == 1) return(trimws(parts[1]))
  last     <- trimws(parts[1])
  given    <- trimws(parts[2])
  words    <- strsplit(given, "\\s+")[[1]]
  initials <- paste(substr(words, 1, 1), collapse = "")
  paste0(last, " ", initials)
}

format_authors_vancouver <- function(authors, max_authors = 6) {
  n         <- length(authors)
  formatted <- sapply(authors, format_author_vancouver)
  if (n <= max_authors) {
    paste(formatted, collapse = ", ")
  } else {
    paste0(paste(formatted[1:max_authors], collapse = ", "), ", et al")
  }
}

write_front_matter <- function(contact) {
  lines <- c(
    "---",
    'title: ""',
    paste0('author: "', contact$name, '"'),
    "documentclass: scrartcl",
    "papersize: letter",
    "fontsize: 9pt",
    "geometry:",
    "  - margin=0.5in",
    "  - heightrounded",
    "  - includefoot",
    "  - footskip=0.5in",
    "colorlinks: true",
    "---",
    ""
  )
  paste(lines, collapse = "\n")
}

raw_latex <- function(lines) {
  c("", "```{=latex}", lines, "```", "")
}

# ── Entry point ───────────────────────────────────────────────────────────────
if (sys.nframe() == 0) main()
