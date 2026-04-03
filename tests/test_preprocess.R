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
