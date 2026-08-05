# --- Always hide these internal / detailed columns ---
always_hide <- c(
  "char_row_id",
  "Covidence ID",
  "Age (years)",
  "Study Period",
  "Setting Details",
  "Vaccine Products",
  "Outcomes Reported",
  "Funding Source",
  # "Risk of Bias",
  "Country/Region",
  "Study Design Specifics",
  "Link"
)


article_cols <- c("Journal", "PMID", "PMCID", "DOI")

# --- Desired column order (all possible columns) ---
column_order <- c(
  "char_row_id",
  "Covidence ID",
  "Study",
  "Virus",
  "Population",
  "Age (years)",
  "Total N",
  "N_numeric",
  "Study Period",
  "Study Design",
  "Study Design Specifics",
  "Country/Region",
  "Setting Details",
  "Vaccine Products",
  "Outcomes Reported",
  "Funding Source",
  "Risk of Bias",
  "Journal",
  "PMID",
  "PMCID",
  "DOI",
  "Link"
)

# --- Column widths (pixels) – only for columns that still exist ---
desired_widths <- c(
  "Study" = 150,
  "Population" = 130,
  "Total N" = 80,
  "Study Design" = 160,
  "Risk of Bias" = 120,
  "Virus" = 130
)

pop_code_to_full <- c(
  "OA" = "Older Adults",
  "A" = "Adults",
  "I" = "Infants",
  "C" = "Children",
  "IC" = "Immunocompromised",
  "HR" = "High-risk",
  "P" = "Pregnant",
  "H" = "Healthcare personnel"
)
