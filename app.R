# =============================================================================
# Shiny App - Systematic Review Data Visualisation
# =============================================================================

library(shiny)
library(readxl)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(ggplot2)
library(rlang)


source("shiny_aux/helpers.R")
source("shiny_aux/config.R")
source("shiny_aux/outcome_config.R")

source("shiny_aux/filter_ui.R")
source("shiny_aux/filter_server.R")

source("shiny_aux/styles.R")

# ---- Load data once (runs when the app starts) ----
char_path <- "characteristics_tables/All_Study_Characteristics.xlsx"
char_data <- read_excel(char_path, sheet = "Study Characteristics")
all_char_ids <- unique(char_data$char_row_id)

# Extract numeric study period starts for slider bounds
study_years <- as.integer(char_data$`Study Period Start`)
study_years <- study_years[!is.na(study_years)]
min_study_year <- min(study_years)
max_study_year <- max(study_years)

pop_long <- read_excel(char_path, sheet = "Population_Long")
n_long <- read_excel(char_path, sheet = "N_Long")
virus_long <- read_excel(char_path, sheet = "Virus_Long")
outcomes_long <- read_excel(char_path, sheet = "Outcomes_Long")
rob_long <- read_excel(char_path, sheet = "RoB_Long")


outcome_path <- "outcome_tables/All_Tables_split.xlsx"
# outcome_data <- read_excel(outcome_path, sheet = "All")
outcome_data <- read_excel(outcome_path, sheet = "All", col_types = "text")
all_outcome_ids <- unique(outcome_data$row_id)

outcome_pop_long <- read_excel(outcome_path, sheet = "Population_Long")
outcome_virus_long <- read_excel(outcome_path, sheet = "Virus_Long")

# Domain mapping for outcomes. Each outcome row has exactly one domain
# It's already in the main sheet, but need a long sheet since the domain function expects a long sheet. Just adding this so the code can be reused. Revisit later since this is unnecessary overall
outcome_domain_long <- outcome_data %>%
  select(row_id, Domain)

# Build risk-of-bias mapping for outcomes – one rating per outcome row
outcome_rob_long <- outcome_data %>%
  select(row_id, `Risk of Bias`) %>%
  rename(`Overall Risk` = `Risk of Bias`)


# ---- User Interface ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("VIP 2026-2027"),
  tags$style(HTML(filter_message_css)),
  tabsetPanel(
    tabPanel(
      "About",
      tags$h2(
        tags$b("Welcome to the Vaccine Integrity Project!")
      ),
      tags$p(
        style = "color: #c865ab;",
        "Questions/comments/concerns about this application? Please e‑mail Michelle Watson at",
        tags$a(
          href = "mailto:michellealiciawatson@gmail.com",
          "michellealiciawatson@gmail.com",
          style = "color: #c865ab;" # match the pink color
        )
      ),
      # New paragraph with the Evidence Base link
      tags$p(
        style = "color: #c865ab;",
        tags$a(
          href = "https://vaxintegrity.cidrap.umn.edu/evidence-reviews/2025-2026-respiratory-season",
          "The Evidence Base for 2025-26 Respiratory Season Immunizations",
          style = "color: #0066cc;"
        )
      ),
      tags$h2(
        tags$b("About the Vaccine Integrity Project:")
      ),
      tags$p(
        "CIDRAP's Vaccine Integrity Project is an initiative dedicated to safeguarding vaccine use in the U.S. so that it remains grounded in the best available science, free from external influence, and focused on optimizing protection of individuals, families, and communities against vaccine-preventable diseases."
      ),
      tags$p(
        "The Vaccine Integrity Project issued its final report from the planning phase summarizing its findings from the exploratory phase, focused on what is needed to ensure the integrity of the U.S. vaccine system, including vaccine evaluations and clinical guidelines based on rigorous and timely reviews."
      ),
      tags$p(
        "The Vaccine Integrity Project is focusing on actions that stemmed from its earlier work:"
      ),
      tags$ul(
        tags$li(
          tags$strong("Implementing a rapid response accountability effort."),
          " In response to misleading and inaccurate claims, the Vaccine Integrity Project aims to launch a rapid response communications initiative to monitor and address vaccine- and public health-related misinformation originating from official, federal sources in real time."
        ),
        tags$li(
          tags$strong(
            "Developing and disseminating the evidence base for immunization recommendations and clinical consideration."
          ),
          " Engaging with healthcare providers, the public health community, and medical societies, CIDRAP is leading a comprehensive review of scientific evidence to inform immunization recommendations so that clinicians have evidence-backed guidance on the key immunizations for all ages on COVID, RSV, and influenza heading into respiratory virus season."
        ),
        tags$li(
          tags$strong("Fostering continued collaboration and visibility."),
          " No single organization can operate in isolation. The scale and complexity of the challenges ahead demand ongoing collaboration and coordinated action across the ecosystem. Regular convening will support better alignment, reduce duplication, and help prioritize and address emerging issues in real time."
        ),
      ),
      tags$p(
        "The systematic review includes studies published between January 2020 - July 2026."
      )
    ),
    tabPanel(
      "Studies",
      filterSidebarLayout(
        sidebar_id = "studies_sidebar",
        toggle_btn = FALSE,
        sidebar_content = tagList(
          checkboxInput(
            "lightweight",
            "Lightweight view (hide article details)",
            value = TRUE
          ),
          div(
            style = "margin-bottom: 8px;",
            downloadButton(
              "downloadFilteredStudies",
              "Download filtered view",
              class = "btn-sm"
            )
          ),
          simpleFiltersUI("simple_filters"),
          advancedFiltersUI("advanced")
        ),
        main_content = tagList(
          tags$div(
            class = "filter-message",
            style = "margin-bottom: 10px; font-size: 15px;",
            textOutput("study_count")
          ),
          div(
            style = "display: flex; justify-content: flex-end; margin: 12px 0 8px 0;",
            downloadButton(
              "downloadFull",
              "Download Study Characteristics",
              class = "btn-sm"
            )
          ),
          DTOutput("studies_table")
        )
      )
    ),

    tabPanel(
      "Outcomes",
      filterSidebarLayout(
        sidebar_id = "outcomes_sidebar",
        toggle_btn = FALSE,
        sidebar_content = tagList(
          checkboxInput(
            "outcome_lightweight",
            "Lightweight view (hide counts)",
            value = TRUE
          ),
          div(
            style = "margin-bottom: 8px;",
            downloadButton(
              "downloadFilteredOutcomes",
              "Download filtered view",
              class = "btn-sm"
            )
          ),
          outcomeSimpleFiltersUI("outcome_simple_filters"),
          advancedFiltersUI("outcome_advanced")
        ),
        main_content = tagList(
          tags$div(
            class = "filter-message",
            style = "margin-bottom: 10px; font-size: 15px;",
            textOutput("outcome_count")
          ),
          div(
            style = "display: flex; justify-content: flex-end; margin: 12px 0 8px 0;",
            downloadButton(
              "downloadOutcomes",
              "Download Outcomes",
              class = "btn-sm"
            )
          ),
          DTOutput("outcomes_table")
        )
      )
    )
  )
)

# ---- Server logic ----
server <- function(input, output, session) {
  observeEvent(input$toggle_studies_sidebar, {
    shinyjs::toggle("studies_sidebar", anim = TRUE)
  })
  # age_filter <- ageGroupFilterServer("age_group", pop_long)
  # pop_type_filter <- popTypeFilterServer("pop_type", pop_long)
  # virus_filter <- virusFilterServer("virus", virus_long)
  # study_design_filter <- studyDesignFilterServer("study_design", char_data)
  # rob_filter <- robFilterServer("rob", rob_long)

  # ---- 1. Create all filters EXCEPT study period ----
  age_filter <- ageGroupFilterServer(
    "simple_filters-age_group",
    pop_long,
    all_ids = all_char_ids
  )
  pop_type_filter <- popTypeFilterServer(
    "simple_filters-pop_type",
    pop_long,
    all_ids = all_char_ids
  )
  virus_filter <- virusFilterServer(
    "simple_filters-virus",
    virus_long,
    all_ids = all_char_ids
  )
  study_design_filter <- studyDesignFilterServer(
    "simple_filters-study_design",
    char_data,
    all_ids = all_char_ids
  )
  domain_filter <- domainFilterServer(
    "simple_filters-domain",
    outcomes_long,
    all_ids = all_char_ids
  )
  rob_filter <- robFilterServer(
    "simple_filters-rob",
    rob_long,
    all_ids = all_char_ids
  )
  advanced_filter <- advancedFilterServer(
    "advanced",
    pop_long,
    all_ids = all_char_ids
  )

  # ---- 2. Create the study period module (NO filtered_data yet) ----
  # study_period_filter <- studyPeriodFilterServer(
  #   "simple_filters-study_period",
  #   char_data
  # )
  study_period_module <- studyPeriodFilterServer(
    "simple_filters-study_period",
    char_data,
    all_ids = all_char_ids
  )

  # ---- 3. Now define filtered_data (safe because study_period_filter exists) ----
  filtered_data <- reactive({
    # ids_period <- study_period_filter()
    ids_period <- study_period_module$ids()
    ids_age <- age_filter()
    ids_type <- pop_type_filter()
    ids_virus <- virus_filter()
    ids_design <- study_design_filter()
    ids_domain <- domain_filter()
    ids_rob <- rob_filter()
    ids_adv <- advanced_filter()

    keep_ids <- Reduce(
      intersect,
      list(
        ids_age,
        ids_type,
        ids_virus,
        ids_design,
        ids_domain,
        ids_rob,
        ids_period,
        ids_adv
      )
    )

    message("--- Study filter diagnostics ---")
    message("  age:        ", length(ids_age))
    message("  type:       ", length(ids_type))
    message("  virus:      ", length(ids_virus))
    message("  design:     ", length(ids_design))
    message("  domain:     ", length(ids_domain))
    message("  rob:        ", length(ids_rob))
    message("  period:     ", length(ids_period))
    message("  advanced:   ", length(ids_adv))
    message("  all studies:", length(all_char_ids))

    # ---- temporary diagnostic: find dropped IDs ----
    all_ids <- char_data$char_row_id # everyone
    dropped_ids <- setdiff(all_ids, keep_ids)
    if (length(dropped_ids) > 0) {
      message("Studies dropped by filters: ", length(dropped_ids))
      message(
        "Dropped char_row_id: ",
        paste(sort(dropped_ids), collapse = ", ")
      )
    } else {
      message("No studies dropped by filters")
    }
    # ---- end diagnostic ----

    char_data %>% filter(char_row_id %in% keep_ids)
  })

  observe({
    study_period_module$setFilteredData(filtered_data())
  })

  observe({
    n <- nrow(filtered_data())
    message("filtered_data rows: ", n)
  })

  # ---- 4. Activate simple filters ----
  # simpleFiltersServer("simple_filters")
  simpleFiltersServer(
    "simple_filters",
    reset_study_period = study_period_module$resetSlider
  )

  # output$`simple_filters-study_period-period_hist` <- renderPlotly({
  #   df <- filtered_data()
  #
  #   yrs <- df$`Study Period Start`
  #   yrs <- yrs[!is.na(yrs)]
  #
  #   hist_df <- as.data.frame(table(yrs))
  #   names(hist_df) <- c("year", "count")
  #   hist_df$year <- as.integer(as.character(hist_df$year))
  #
  #   plot_ly(
  #     data = hist_df,
  #     x = ~year,
  #     y = ~count,
  #     type = "bar",
  #     marker = list(color = "#007bc2"),
  #     width = 0.6
  #   ) %>%
  #     layout(
  #       autosize = FALSE,
  #       width = 250, # ← add this
  #       height = 80, # ← add this
  #       xaxis = list(visible = FALSE),
  #       yaxis = list(visible = FALSE),
  #       plot_bgcolor = "rgba(0,0,0,0)",
  #       paper_bgcolor = "rgba(0,0,0,0)",
  #       margin = list(l = 0, r = 0, t = 0, b = 0)
  #     )
  # })

  # ---- Prepare the final displayed data (clickable links, hidden cols, reorder) ----
  processed_data <- reactive({
    display <- filtered_data()

    # Add hidden numeric column for Total N sorting
    display <- display %>%
      left_join(n_long %>% select(char_row_id, N), by = "char_row_id") %>%
      rename(N_numeric = N)

    # Make Study column clickable (helper)
    display <- make_study_clickable(display)

    # always_hide, article_cols, column_order -> config.R, drop_columns -> helpers.R
    display <- drop_columns(display, always_hide)
    if (isTRUE(input$lightweight)) {
      display <- drop_columns(display, article_cols)
    }

    # Reorder columns
    display <- display[, intersect(column_order, names(display)), drop = FALSE]
    # Sort by the plain‑text study name (ascending)
    display <- display %>% arrange(Study_plain)
    # Remove the helper column – no longer needed
    display <- display %>% select(-Study_plain)

    # if ("Study_plain" %in% names(display)) {
    #   display <- display %>% arrange(Study_plain)
    # } else {
    #   # Fallback: sort by the HTML Study column (will sort alphabetically)
    #   display <- display %>% arrange(Study)
    # }

    display
  })

  output$study_count <- renderText({
    n <- nrow(processed_data())
    if (n == 0) {
      return("No studies match the current filters")
    }
    paste("Showing", n, "studies")
  })

  output$studies_table <- renderDT({
    display <- processed_data()
    # Build columnDefs
    col_defs <- make_col_defs(display, desired_widths)

    # Append definitions for the hidden sorting column
    # DT column indices are 0‑based
    total_n_visible_idx <- which(names(display) == "Total N") - 1
    n_numeric_idx <- which(names(display) == "N_numeric") - 1

    # Make the visible Study column sort by the hidden plain‑text column
    # study_visible_idx <- which(names(display) == "Study") - 1
    # study_plain_idx <- which(names(display) == "Study_plain") - 1

    # Hide the numeric study period columns and link Study Period to Study Period Start
    sp_visible_idx <- which(names(display) == "Study Period") - 1
    sp_start_idx <- which(names(display) == "Study Period Start") - 1
    sp_end_idx <- which(names(display) == "Study Period End") - 1

    col_defs <- c(
      col_defs,
      list(
        # list(targets = study_visible_idx, orderData = study_plain_idx),
        # list(targets = study_plain_idx, visible = FALSE),
        list(targets = total_n_visible_idx, orderData = n_numeric_idx),
        list(targets = n_numeric_idx, visible = FALSE),
        list(targets = sp_visible_idx, orderData = sp_start_idx),
        list(targets = sp_start_idx, visible = FALSE),
        list(targets = sp_end_idx, visible = FALSE)
      )
    )

    # --- datatable ---
    datatable(
      display,
      rownames = FALSE,
      escape = FALSE, # don't escape HTML chars
      filter = "none", # no column filters yet
      options = list(
        # showing # of # + pagination at bottom. Top only has pagination f=filter=search bar
        dom = "<'top' p f> t <'bottom' i p>",
        pageLength = 50,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = col_defs
      )
    )
  })
  # ---- Download handlers ----
  output$downloadFull <- downloadHandler(
    filename = function() {
      "All_Study_Characteristics.xlsx"
    },
    content = function(file) {
      file.copy(char_path, file)
    }
  )
  output$downloadFilteredStudies <- downloadHandler(
    filename = function() {
      paste0("Filtered_Studies_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      wb_sheets <- readxl::excel_sheets(char_path)
      all_data <- lapply(wb_sheets, function(s) {
        read_excel(char_path, sheet = s)
      })
      names(all_data) <- wb_sheets

      # Replace the main sheet with the filtered data (all columns)
      all_data[["Study Characteristics"]] <- filtered_data()

      writexl::write_xlsx(all_data, file)
    }
  )

  # ---- Outcomes tab ----

  # 1. Create a separate study‑period filter for outcomes
  outcome_study_period <- studyPeriodFilterServer(
    "outcome_simple_filters-study_period",
    char_data,
    all_ids = all_outcome_ids # we still filter by study start years from the characteristics
  )

  # 2. Instantiate all other filters for outcomes (use the SAME filter functions)
  outcome_age_filter <- ageGroupFilterServer(
    "outcome_simple_filters-age_group",
    outcome_pop_long, # <-- use outcomes Population_Long
    id_col = "row_id",
    all_ids = all_outcome_ids # <-- filter by outcome row ID
  )
  outcome_type_filter <- popTypeFilterServer(
    "outcome_simple_filters-pop_type",
    outcome_pop_long, # <-- same outcomes Population_Long
    id_col = "row_id",
    all_ids = all_outcome_ids
  )
  outcome_virus_filter <- virusFilterServer(
    "outcome_simple_filters-virus",
    outcome_virus_long,
    id_col = "row_id",
    all_ids = all_outcome_ids
  )
  outcome_design_filter <- studyDesignFilterServer(
    "outcome_simple_filters-study_design",
    char_data,
    all_ids = all_outcome_ids
  )
  outcome_domain_filter <- domainFilterServer(
    "outcome_simple_filters-domain",
    outcome_domain_long,
    id_col = "row_id",
    all_ids = all_outcome_ids
  )
  outcome_rob_filter <- robFilterServer(
    "outcome_simple_filters-rob",
    outcome_rob_long,
    id_col = "row_id",
    all_ids = all_outcome_ids
  )
  outcome_advanced_filter <- advancedFilterServer(
    "outcome_advanced",
    outcome_pop_long, # outcomes Population_Long (loaded earlier)
    id_col = "row_id",
    all_ids = all_outcome_ids
  )

  outcome_type_of_outcome_filter <- typeOfOutcomeFilterServer(
    "outcome_simple_filters-type_of_outcome",
    outcome_data,
    id_col = "row_id",
    all_ids = all_outcome_ids
  )

  # 3. Combine all filters to get a vector of allowed char_row_ids
  filtered_outcome_ids <- reactive({
    # Filters that return row_id
    ids_age <- outcome_age_filter()
    ids_type <- outcome_type_filter()
    ids_virus <- outcome_virus_filter()
    ids_domain <- outcome_domain_filter()
    ids_rob <- outcome_rob_filter()
    ids_type_of_outcome <- outcome_type_of_outcome_filter()
    ids_adv <- outcome_advanced_filter()

    # Filters that still return char_row_id
    ids_design <- outcome_design_filter()
    ids_period <- outcome_study_period$ids()

    # Convert char_row_id → row_id
    char_to_row <- outcome_data %>% distinct(char_row_id, row_id)
    char_ids <- Reduce(intersect, list(ids_design, ids_period))
    char_row_ids <- char_to_row %>%
      filter(char_row_id %in% char_ids) %>%
      pull(row_id)

    # Final intersection
    result_ids <- Reduce(
      intersect,
      list(
        ids_age,
        ids_type,
        ids_virus,
        ids_type_of_outcome,
        ids_domain,
        ids_rob,
        ids_adv,
        char_row_ids
      )
    )

    # Diagnostics – print to console
    message("--- Outcome filter diagnostics ---")
    message("  age:        ", length(ids_age))
    message("  type:       ", length(ids_type))
    message("  virus:      ", length(ids_virus))
    message("  design:     ", length(ids_design))
    message("  type_of_outcome: ", length(ids_type_of_outcome))
    message("  domain:     ", length(ids_domain))
    message("  rob:        ", length(ids_rob))
    message("  period:     ", length(ids_period))
    message("  advanced:   ", length(ids_adv))
    message("  char_row_ids: ", length(char_row_ids))
    message("  final intersection: ", length(result_ids))

    # Temporary diagnostic – find dropped outcome rows
    all_ids_out <- all_outcome_ids
    dropped_out <- setdiff(all_ids_out, result_ids)
    if (length(dropped_out) > 0) {
      message("Outcomes dropped by filters: ", length(dropped_out))
      dropped_info <- outcome_data %>%
        filter(row_id %in% dropped_out) %>%
        distinct(row_id, char_row_id, `Study Label`) %>%
        arrange(row_id)
      print(as.data.frame(dropped_info), max = 200)
    } else {
      message("No outcomes dropped by filters")
    }
    # ---- end outcome diagnostic ----

    # Explicit return
    result_ids
  })

  # 4. Filter the outcomes data based on char_row_id
  filtered_outcome_data <- reactive({
    outcome_data %>% filter(row_id %in% filtered_outcome_ids())
  })

  # Temporary diagnostic – check whether the filtered data shrinks when filters are active
  observe({
    n <- nrow(filtered_outcome_data())
    message("filtered_outcome_data rows: ", n)
  })

  # 5. Pass the filtered outcome data to the study‑period histogram
  observe({
    outcome_study_period$setFilteredData(
      # histogram still shows distribution of study start years
      char_data %>% filter(char_row_id %in% filtered_outcome_ids())
    )
  })

  # 6. Process the outcome table (plain study labels, column hiding, sorting)
  processed_outcome_data <- reactive({
    display <- filtered_outcome_data()

    # Hide unwanted columns
    display <- drop_columns(display, outcome_always_hide)

    # Rename Vaccine -> Comparison
    names(display)[names(display) == "Vaccine"] <- "Comparison"

    # Conditionally hide ecological total columns
    if (isTRUE(input$outcome_lightweight)) {
      display <- drop_columns(
        display,
        c(
          "Number of events (ecological studies)",
          "N total (ecological studies)",
          "Number of events in intervention arm",
          "Sample size intervention",
          "Number of events in comparator arm",
          "Sample size comparator"
        )
      )
    }

    # Reorder columns
    display <- display[,
      intersect(outcome_column_order, names(display)),
      drop = FALSE
    ]

    # Sort alphabetically by Study Label
    display <- display %>% arrange(`Study Label`)
    display
  })

  # 7. Row count text
  output$outcome_count <- renderText({
    n <- nrow(processed_outcome_data())
    if (n == 0) {
      return("No outcome rows match the current filters")
    }
    paste("Showing", n, "outcome rows")
  })

  # 8. Render the outcomes table
  # server = TRUE, # large dataset – server‑side processing
  output$outcomes_table <- renderDT(server = TRUE, {
    display <- processed_outcome_data()
    col_defs <- make_col_defs(display, outcome_desired_widths)

    # Add sorting helper for Estimate (95% CI) via hidden numeric columns if present
    # For now we skip extra sorting helpers – you can add them later if needed.

    datatable(
      display,
      rownames = FALSE,
      escape = FALSE,
      filter = "none",
      options = list(
        # dom = "<'top' p> t <'bottom' i p>",
        # showing # of # + pagination at bottom. Top only has pagination f=filter=search bar
        dom = "<'top' p f> t <'bottom' i p>",
        pageLength = 25,
        scrollX = FALSE,
        autoWidth = FALSE,
        columnDefs = col_defs
      )
    )
  })

  # 9. Download handler for outcomes
  output$downloadOutcomes <- downloadHandler(
    filename = function() "Filtered_Outcomes.xlsx",
    content = function(file) {
      write_xlsx(processed_outcome_data(), file)
    }
  )
  output$downloadFilteredOutcomes <- downloadHandler(
    filename = function() {
      paste0("Filtered_Outcomes_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      # 1. Read the Footnotes sheet from the original workbook
      footnotes_df <- read_excel(outcome_path, sheet = "Footnotes")

      # 2. Get the filtered outcome data as a plain data frame
      filt_df <- as.data.frame(
        filtered_outcome_data(),
        stringsAsFactors = FALSE
      )

      # 3. Build a workbook with ONLY the filtered sheet + Footnotes
      out <- list(
        `Filtered_Outcomes` = filt_df,
        Footnotes = footnotes_df
      )

      # 4. Write the new workbook
      writexl::write_xlsx(out, file)
    }
  )
  # 10. Clear‑all button for outcomes
  simpleFiltersServer(
    "outcome_simple_filters",
    reset_study_period = outcome_study_period$resetSlider
  )
}

# ---- Run the app ----
shinyApp(ui, server)
