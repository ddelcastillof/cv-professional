library(pander)
library(stringr)
library(dplyr)
library(googlesheets4)
library(lubridate)
library(kableExtra)

gs4_deauth()

gscholar_stats <- function(url) {
  cites <- get_cites(url)
  return(paste(
    'Citations:', cites$citations, '|',
    'h-index:',   cites$hindex, '|',
    'i10-index:', cites$i10index
  ))
}

get_cites <- function(url) {
  tryCatch({
    html <- xml2::read_html(url)
    node <- rvest::html_nodes(html, xpath='//*[@id="gsc_rsb_st"]')
    if (length(node) == 0) {
      return(data.frame(citations = "N/A", hindex = "N/A", i10index = "N/A"))
    }
    cites_df <- rvest::html_table(node)[[1]]
    cites <- data.frame(t(as.data.frame(cites_df)[,2]))
    names(cites) <- c('citations', 'hindex', 'i10index')
    return(cites)
  }, error = function(e) {
    return(data.frame(citations = "N/A", hindex = "N/A", i10index = "N/A"))
  })
}

get_cv_sheet <- function(sheet) {
    return(read_sheet(
        ss = 'https://docs.google.com/spreadsheets/d/1yEYPdjQNqIw_lrOjUPDKjx9wMfzTeYrcdrtSDRj6Zhw/edit',
        sheet = sheet
    ))
}

make_ordered_list <- function(x) {
    return(pandoc.list(x, style = 'ordered'))
}

make_bullet_list <- function(x) {
  return(pandoc.list(x, style = 'bullet'))
}

make_ordered_list_filtered <- function(df, cat) {
  return(df |>
    filter(.data$category == {{cat}}) |>
        mutate(
            citation = str_replace_all(
                .data$citation,
                "\\\\\\*(\\w+),",
                "\\\\*\\\\underline{\\1},"
            )
        ) |>
    pull(.data$citation) |>
    make_ordered_list()
  )
}

make_bullet_list_filtered <- function(df, cat) {
  return(df |>
    filter(.data$category == {{cat}}) |>
        mutate(
            citation = str_replace_all(
                .data$citation,
                "\\\\\\*(\\w+),",
                "\\\\*\\\\underline{\\1},"
            )
        ) |>
    pull(.data$citation) |>
    make_bullet_list()
  )
}

na_to_space <- function(x) {
  return(ifelse(is.na(x), '', x))
}

enquote <- function(x) {
  return(paste0('"', x, '"'))
}

markdown_url <- function(url) {
  return(paste0('[', url, '](', url,')'))
}

make_grants_table <- function(df, cat) {
  grants_filtered <- df |>
    filter(.data$status == cat) |>
    mutate(
      funder = paste0(.data$funder_2, 
                      ifelse(.data$funder_1 == "", "",
                             paste0(", ", .data$funder_1))),
      period = paste0(.data$year_start, " – ", .data$year_end),
      investigators = paste0(.data$pi, 
                            ifelse(.data$coi_1 == "", "",
                                   paste0(", ", .data$coi_1)),
                            ifelse(.data$coi_2 == "", "",
                                   paste0(", ", .data$coi_2))),
      budget = paste0(ifelse(.data$budget_currency == "USD",
                             "$", ""),
                      .data$budget_total)
    )
    
  for (i in seq_len(nrow(grants_filtered))) {
    grant <- grants_filtered[i, ]
    
    grant_table <- data.frame(
      Category = c("Funder:", "Project Code:", "Title:", "Investigators:",
                   "Period:", "Budget:", "Role:"),
      Details = c(grant$funder, grant$project_code, grant$title,
                  grant$investigators, grant$period, grant$budget,
                  grant$role),
      stringsAsFactors = FALSE
    )
    
    print(kableExtra::kable(grant_table, 
      format = "latex",
      col.names = NULL,
      align = c("l", "l"),
      booktabs = TRUE,
      vline = "",
      bottomrule = "",
      toprule = "",
      midrule = "",
      linesep = ""
    ))
    
    if (i < nrow(grants_filtered)) {
      cat("\n")
    }
  }
}