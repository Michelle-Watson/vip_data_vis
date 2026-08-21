# Remove all columns listed in "cols" that are present in the data frame
drop_columns <- function(df, cols) {
  for (col in cols) {
    if (col %in% names(df)) df[[col]] <- NULL
  }
  df
}


# Helper: check if a character column contains a usable URL
is_valid_url <- function(x) {
  !is.na(x) & trimws(x) != "" & !trimws(x) %in% c("NR", "-", "No DOI")
}

# Build clickable Study column:
#   - use Link if not empty / NA
#   - else use DOI
#   - if neither is usable, keep plain text
# Make the "Study" column clickable, using Link first, then DOI
make_study_clickable <- function(df) {
  has_link <- is_valid_url(df$Link)
  has_doi <- is_valid_url(df$DOI)

  chosen_url <- ifelse(
    has_link,
    df$Link,
    ifelse(has_doi, df$DOI, NA_character_)
  )

  # Save plain text for sorting
  df$Study_plain <- df$Study

  df$Study <- ifelse(
    !is.na(chosen_url),
    paste0('<a href="', chosen_url, '" target="_blank">', df$Study, '</a>'),
    df$Study
  )
  df
}

# Make a generic label column clickable using Link/DOI
make_label_clickable <- function(df, label_col) {
  has_link <- is_valid_url(df$Link)
  has_doi <- is_valid_url(df$DOI)

  chosen_url <- ifelse(
    has_link,
    df$Link,
    ifelse(has_doi, df$DOI, NA_character_)
  )

  df[[label_col]] <- ifelse(
    !is.na(chosen_url),
    paste0(
      '<a href="',
      chosen_url,
      '" target="_blank">',
      df[[label_col]],
      '</a>'
    ),
    df[[label_col]]
  )
  df
}

# Build columnDefs list from desired widths (only for existing columns)
make_col_defs <- function(df, widths) {
  existing <- intersect(names(widths), names(df))
  lapply(existing, function(col) {
    list(
      width = paste0(as.integer(widths[col]), "px"),
      targets = col
    )
  })
}


# Reusable sidebar layout for filter + main content
filterSidebarLayout <- function(
  sidebar_content,
  main_content,
  sidebar_id,
  toggle_btn = TRUE
) {
  sidebarLayout(
    sidebarPanel(
      id = sidebar_id,
      width = 3,
      sidebar_content
    ),
    mainPanel(
      width = 9,
      if (toggle_btn) {
        tags$div(
          style = "margin-bottom: 8px;",
          actionButton(
            inputId = paste0("toggle_", sidebar_id),
            label = "☰ Show/Hide Filters",
            class = "btn-sm"
          )
        )
      },
      main_content
    )
  )
}
