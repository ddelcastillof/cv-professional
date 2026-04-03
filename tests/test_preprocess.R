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

# ── render_contact_header ─────────────────────────────────────────────────────
test_that("render_contact_header produces raw LaTeX block with FontAwesome icons", {
  contact <- list(
    name         = "Test Name, MD",
    linkedin     = "test-linkedin",
    linkedin_url = "https://www.linkedin.com/in/test/",
    email        = "test@example.com",
    github       = "testuser",
    github_url   = "https://github.com/testuser"
  )
  result <- paste(render_contact_header(contact), collapse = "\n")
  expect_true(grepl("```\\{=latex\\}", result))
  expect_true(grepl("\\\\faLinkedin", result))
  expect_true(grepl("\\\\faEnvelope", result))
  expect_true(grepl("\\\\faGithub", result))
  expect_true(grepl("Test Name, MD", result))
})

# ── render_summary ────────────────────────────────────────────────────────────
test_that("render_summary produces a LaTeX Professional Summary section", {
  data   <- list(text = "Test summary paragraph.")
  result <- paste(render_summary(data), collapse = "\n")
  expect_true(grepl("\\\\section\\{Professional Summary\\}", result))
  expect_true(grepl("Test summary paragraph\\.", result))
  expect_true(grepl("```\\{=latex\\}", result))
})

# ── render_education ──────────────────────────────────────────────────────────
test_that("render_education produces longtable for primary entries", {
  edu    <- list(list(degree = "MPH", institution = "UW",
                      location = "Seattle -- USA", start = "Sep 2023", end = "Jun 2025"))
  result <- paste(render_education(edu, additional = list()), collapse = "\n")
  expect_true(grepl("\\\\section\\{Education\\}", result))
  expect_true(grepl("longtable", result))
  expect_true(grepl("MPH", result))
  expect_true(grepl("Sep 2023", result))
})

test_that("render_education adds Additional Education block when provided", {
  edu <- list(list(degree = "MPH", institution = "UW",
                   location = "Seattle", start = "2023", end = "2025"))
  add <- list(list(degree = "Certificate", institution = "UPCH",
                   location = "Lima", start = "2021", end = "2022"))
  result <- paste(render_education(edu, add), collapse = "\n")
  expect_true(grepl("\\\\section\\{Additional Education\\}", result))
  expect_true(grepl("Certificate", result))
})

# ── render_experience ─────────────────────────────────────────────────────────
test_that("render_experience includes bullets in itemize environment", {
  exp    <- list(list(title = "Research Assistant", institution = "Test University",
                      location = "Seattle -- USA", start = "Jan 2024", end = "Current",
                      bullets = list("Did research.", "Wrote papers.")))
  result <- paste(render_experience(exp), collapse = "\n")
  expect_true(grepl("\\\\section\\{Professional Experience\\}", result))
  expect_true(grepl("\\\\begin\\{itemize\\}", result))
  expect_true(grepl("Did research\\.", result))
})

test_that("render_experience inserts newpage when new_page_before is true", {
  exp <- list(
    list(title = "Job 1", institution = "Inst", location = "City",
         start = "2020", end = "2022", bullets = list("Bullet.")),
    list(title = "Job 2", institution = "Inst", location = "City",
         start = "2022", end = "Current", new_page_before = TRUE,
         bullets = list("Bullet."))
  )
  result <- paste(render_experience(exp), collapse = "\n")
  expect_true(grepl("\\\\newpage", result))
})

# ── render_skills ─────────────────────────────────────────────────────────────
test_that("render_skills includes all professional CV category labels", {
  skills <- list(
    programming    = "R, Python",
    cloud          = "Linux",
    tools          = "VS Code",
    data_standards = "HIPAA",
    packages       = "tidyverse",
    methods        = "Bayesian modelling",
    languages      = "English",
    competencies   = "Communication"
  )
  result <- paste(render_skills(skills), collapse = "\n")
  expect_true(grepl("\\\\section\\{Skills\\}", result))
  expect_true(grepl("\\\\textbf\\{Programming:\\}", result))
  expect_true(grepl("\\\\textbf\\{Data Standards:\\}", result))
  expect_true(grepl("\\\\textbf\\{Statistical methods:\\}", result))
  expect_true(grepl("HIPAA", result))
})

# ── render_certifications ─────────────────────────────────────────────────────
test_that("render_certifications produces two-column longtable", {
  data   <- list(list(name = "CITI Program", year = "2023"))
  result <- paste(render_certifications(data), collapse = "\n")
  expect_true(grepl("\\\\section\\{Licensure and Certification\\}", result))
  expect_true(grepl("CITI Program", result))
  expect_true(grepl("2023", result))
})
