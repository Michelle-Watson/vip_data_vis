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

  df$Study <- ifelse(
    !is.na(chosen_url),
    paste0('<a href="', chosen_url, '" target="_blank">', df$Study, '</a>'),
    df$Study
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
