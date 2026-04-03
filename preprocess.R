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

# ── BibTeX parser ─────────────────────────────────────────────────────────────
# Custom parser handles Paperpile's double-quoted field format and multi-line values.

read_bib <- function(bib_path) {
  raw    <- paste(readLines(bib_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  starts <- gregexpr("(?m)^@ARTICLE\\{", raw, perl = TRUE)[[1]]
  if (starts[1] < 0) return(data.frame())

  n    <- length(starts)
  rows <- vector("list", n)

  for (i in seq_len(n)) {
    s     <- starts[i]
    e     <- if (i < n) starts[i + 1L] - 1L else nchar(raw)
    block <- substring(raw, s, e)
    key   <- gsub("(?si)^@ARTICLE\\{([^,\n]+).*$", "\\1", block, perl = TRUE)

    get_val <- function(field) {
      pat <- paste0(
        "(?si)\\b", field,
        "\\s*=\\s*(?:\"([^\"]*)\"|\\{([^{}]*)\\}|(\\b\\d+\\b))"
      )
      m <- regexpr(pat, block, perl = TRUE)
      if (m[1L] < 0L) return(NA_character_)
      caps <- attr(m, "capture.start")
      lens <- attr(m, "capture.length")
      for (g in seq_len(ncol(caps))) {
        cs <- caps[1L, g]; cl <- lens[1L, g]
        if (cs > 0L && cl > 0L) {
          return(trimws(gsub("\\s+", " ", substring(block, cs, cs + cl - 1L), perl = TRUE)))
        }
      }
      NA_character_
    }

    author_raw <- get_val("author")
    authors    <- if (!is.na(author_raw) && nchar(author_raw) > 0L) {
      trimws(strsplit(author_raw, "\\s+and\\s+", perl = TRUE)[[1]])
    } else character(0)

    df           <- data.frame(
      BIBTEXKEY = trimws(key),
      TITLE     = get_val("title"),
      YEAR      = get_val("year"),
      JOURNAL   = get_val("journal"),
      VOLUME    = get_val("volume"),
      NUMBER    = get_val("number"),
      PAGES     = get_val("pages"),
      DOI       = get_val("doi"),
      stringsAsFactors = FALSE
    )
    df$AUTHOR <- list(authors)
    rows[[i]] <- df
  }

  do.call(rbind, Filter(Negate(is.null), rows))
}

# ── Entry point ───────────────────────────────────────────────────────────────
if (sys.nframe() == 0) main()
