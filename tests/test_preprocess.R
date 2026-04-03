library(testthat)
if (basename(getwd()) == "tests") setwd("..")
source("preprocess.R")

# ── na_to_empty ───────────────────────────────────────────────────────────────
test_that("na_to_empty converts NA and NULL to empty string", {
  expect_equal(na_to_empty(NA), "")
  expect_equal(na_to_empty(NA_character_), "")
  expect_equal(na_to_empty(NULL), "")
  expect_equal(na_to_empty("value"), "value")
  expect_equal(na_to_empty(""), "")
})

# ── format_author_vancouver ───────────────────────────────────────────────────
test_that("format_author_vancouver converts Last, First to Last F", {
  expect_equal(format_author_vancouver("Del Castillo, Darwin"), "Del Castillo D")
  expect_equal(format_author_vancouver("Miranda, J Jaime"), "Miranda JJ")
  expect_equal(format_author_vancouver("Smith, Anne"), "Smith A")
})

# ── format_authors_vancouver ──────────────────────────────────────────────────
test_that("format_authors_vancouver joins authors and truncates at 6", {
  six <- c("Author1, A", "Author2, B", "Author3, C",
           "Author4, D", "Author5, E", "Author6, F")
  expect_equal(
    format_authors_vancouver(six),
    "Author1 A, Author2 B, Author3 C, Author4 D, Author5 E, Author6 F"
  )
  seven <- c(six, "Author7, G")
  expect_true(grepl("et al", format_authors_vancouver(seven)))
  expect_false(grepl("Author7", format_authors_vancouver(seven)))
})

# ── write_front_matter ────────────────────────────────────────────────────────
test_that("write_front_matter produces valid YAML front matter block", {
  contact <- list(name = "Test Name, MD")
  result <- write_front_matter(contact)
  expect_true(grepl("^---", result))
  expect_true(grepl("documentclass: scrartcl", result))
  expect_true(grepl("fontsize: 9pt", result))
  expect_true(grepl("margin=0.5in", result))
  expect_true(grepl("Test Name, MD", result))
})

# ── read_bib ──────────────────────────────────────────────────────────────────
test_that("read_bib returns empty data.frame for empty bib file", {
  tmp <- tempfile(fileext = ".bib")
  writeLines("% empty placeholder", tmp)
  result <- read_bib(tmp)
  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 0)
  unlink(tmp)
})

test_that("read_bib parses a minimal BibTeX entry", {
  bib_content <- c(
    '@ARTICLE{TestKey2021,',
    '  author = "Del Castillo, Darwin and Smith, Jane",',
    '  title = "Test Publication",',
    '  journal = "Test Journal",',
    '  year = "2021",',
    '  volume = "1",',
    '  number = "2",',
    '  doi = "10.1234/test"',
    '}'
  )
  tmp <- tempfile(fileext = ".bib")
  writeLines(bib_content, tmp)
  result <- read_bib(tmp)
  expect_equal(nrow(result), 1)
  expect_equal(result$BIBTEXKEY, "TestKey2021")
  expect_equal(result$YEAR, "2021")
  expect_equal(result$JOURNAL, "Test Journal")
  expect_equal(length(result$AUTHOR[[1]]), 2)
  unlink(tmp)
})
