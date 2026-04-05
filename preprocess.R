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

# ── Section renderers ─────────────────────────────────────────────────────────

render_contact_header <- function(contact) {
  raw_latex(c(
    "\\noindent",
    "\\begin{minipage}[c]{0.45\\textwidth}",
    paste0("{\\huge\\underline{\\textbf{", contact$name, "}}}"),
    "\\end{minipage}%",
    "\\hfill",
    "\\begin{minipage}[c]{0.50\\textwidth}",
    "\\raggedleft\\large",
    paste0("\\faLinkedin\\hspace{0.4em}\\href{", contact$linkedin_url,
           "}{", contact$linkedin, "} \\\\[2pt]"),
    paste0("\\faEnvelope\\hspace{0.4em}\\href{mailto:", contact$email,
           "}{", contact$email, "} \\\\[2pt]"),
    paste0("\\faGithub\\hspace{0.4em}\\href{", contact$github_url,
           "}{", contact$github, "}"),
    "\\end{minipage}",
    "\\vspace{4pt}"
  ))
}

# render_summary <- function(summary) {
#   raw_latex(c(
#     "\\section{Professional Summary}",
#     "\\vspace{-1.5em}",
#     "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
#     summary$text
#   ))
# }

render_edu_entry <- function(e) {
  c(
    paste0("\\textbf{", e$degree, "} & \\textbf{", e$start, " -- ", e$end, "} \\\\"),
    paste0("\\textsc{", e$institution, "} & \\textsc{", e$location, "} \\\\"),
    "& \\\\"
  )
}

render_education <- function(education, additional = list()) {
  entries   <- unlist(lapply(education, render_edu_entry), recursive = FALSE)
  edu_block <- raw_latex(c(
    "\\section{Education}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    entries,
    "\\end{longtable}"
  ))

  if (length(additional) == 0) return(edu_block)

  add_entries <- unlist(lapply(additional, render_edu_entry), recursive = FALSE)
  add_block   <- raw_latex(c(
    "\\section{Additional Education}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    add_entries,
    "\\end{longtable}"
  ))

  c(edu_block, add_block)
}

render_exp_entry <- function(e) {
  header <- c(
    paste0("\\textbf{", e$title, "} & \\textbf{", e$start, " -- ", e$end, "} \\\\"),
    paste0("\\textsc{", e$institution, "} & \\textsc{", e$location, "} \\\\")
  )
  bullets_block <- if (!is.null(e$bullets) && length(e$bullets) > 0) {
    c(
      "\\multicolumn{2}{p{18cm}}{%",
      "\\begin{itemize}",
      paste0("  \\item ", unlist(e$bullets)),
      "\\end{itemize}",
      "} \\\\",
      "\\\\"
    )
  } else character(0)

  page_break <- if (isTRUE(e$new_page_before)) "\\newpage" else character(0)
  c(page_break, header, bullets_block)
}

render_experience <- function(experience) {
  entries <- unlist(lapply(experience, render_exp_entry), recursive = FALSE)
  raw_latex(c(
    "\\section{Professional Experience}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{p{14cm}>{\\raggedleft\\arraybackslash}p{4cm}}",
    entries,
    "\\end{longtable}"
  ))
}

render_skills <- function(skills) {
  label_map <- list(
    programming    = "Programming",
    cloud          = "Cloud Computing",
    tools          = "Tools",
    data_standards = "Data Standards",
    packages       = "Packages",
    methods        = "Statistical methods",
    languages      = "Languages",
    competencies   = "Core Competencies"
  )
  lines <- c()
  for (key in names(label_map)) {
    val <- skills[[key]]
    if (!is.null(val) && nchar(val) > 0) {
      lines <- c(lines, paste0("\\textbf{", label_map[[key]], ":} ", val, " \\\\"))
    }
  }
  raw_latex(c(
    "\\section{Skills}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    lines
  ))
}

render_certifications <- function(certs) {
  entries <- lapply(certs, function(cert) {
    paste0(cert$name, " & \\textbf{", cert$year, "} \\\\")
  })
  raw_latex(c(
    "\\section{Licensure and Certification}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}",
    "\\begin{longtable}{>{\\raggedright\\arraybackslash}p{17cm} p{1cm}}",
    unlist(entries),
    "\\end{longtable}"
  ))
}

format_pub_entry <- function(row) {
  authors      <- format_authors_vancouver(unlist(row$AUTHOR))
  year         <- na_to_empty(row$YEAR)
  title        <- na_to_empty(row$TITLE)
  journal      <- na_to_empty(row$JOURNAL)
  volume       <- na_to_empty(row$VOLUME)
  number       <- na_to_empty(row$NUMBER)
  pages        <- na_to_empty(row$PAGES)
  doi          <- na_to_empty(row$DOI)

  vol_str <- if (nchar(volume) > 0) {
    v <- if (nchar(number) > 0) paste0(volume, "(", number, ")") else volume
    if (nchar(pages) > 0) paste0(v, ":", pages) else v
  } else ""

  journal_part <- if (nchar(journal) > 0) paste0("*", journal, "*") else ""
  year_vol     <- if (nchar(year) > 0 && nchar(vol_str) > 0) {
    paste0(". ", year, ";", vol_str)
  } else if (nchar(year) > 0) {
    paste0(". ", year)
  } else ""

  doi_part <- if (nchar(doi) > 0) paste0(" doi:", doi, ".") else "."

  paste0(authors, ". ", title, ". ", journal_part, year_vol, doi_part)
}

render_publications <- function(bib_df, pub_categories) {
  orcid_line <- raw_latex(c(
    "\\section{Publications \\hspace{1cm} ORCID: 0000-0002-8609-0312}",
    "\\vspace{-1.5em}",
    "\\textcolor{darkgray}{\\rule{\\textwidth}{0.5pt}}"
  ))

  section_labels <- list(
    peer_reviewed = "Peer-Reviewed Publications",
    conference    = "Conference Abstracts"
  )

  pub_blocks <- list()
  for (cat in names(section_labels)) {
    keys <- pub_categories[[cat]]
    if (is.null(keys) || length(keys) == 0) next

    entries <- lapply(keys, function(key) {
      row <- bib_df[bib_df$BIBTEXKEY == key, ]
      if (nrow(row) == 0) {
        warning("Cite key not found in references.bib: ", key)
        return(NULL)
      }
      format_pub_entry(as.list(row[1, ]))
    })
    entries <- Filter(Negate(is.null), entries)
    if (length(entries) == 0) next

    subsection_header <- raw_latex(paste0("\\subsection{", section_labels[[cat]], "}"))
    items             <- paste0(seq_along(entries), ". ", unlist(entries))
    pub_blocks[[cat]] <- c(subsection_header, "", items, "")
  }

  c(orcid_line, unlist(pub_blocks))
}

# ── Main assembly ─────────────────────────────────────────────────────────────

main <- function() {
  dir.create("build", showWarnings = FALSE)

  contact      <- yaml::read_yaml("data/contact.yaml")
  #summary_data <- yaml::read_yaml("data/summary.yaml")
  edu_raw      <- yaml::read_yaml("data/education.yaml")
  experience   <- yaml::read_yaml("data/experience.yaml")
  skills_data  <- yaml::read_yaml("data/skills.yaml")
  certs        <- yaml::read_yaml("data/certifications.yaml")
  pub_cats     <- yaml::read_yaml("data/pub_categories.yaml")
  bib_df       <- read_bib("bib/references.bib")

  education      <- edu_raw$primary
  additional_edu <- if (!is.null(edu_raw$additional)) edu_raw$additional else list()

  sections <- list(
    write_front_matter(contact),
    render_contact_header(contact),
    #render_summary(summary_data),
    render_education(education, additional_edu),
    render_skills(skills_data),
    render_experience(experience),
    render_certifications(certs),
    render_publications(bib_df, pub_cats)
  )

  writeLines(unlist(sections), "build/cv.md")
  message("build/cv.md written successfully.")
}

# ── Entry point ───────────────────────────────────────────────────────────────
if (sys.nframe() == 0) main()
