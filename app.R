# =============================================================================
# Shiny App - Systematic Review Data Visualisation
# =============================================================================

library(shiny)
library(readxl)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)

source("shiny_aux/helpers.R")
source("shiny_aux/config.R")

source("shiny_aux/filter_ui.R")
source("shiny_aux/filter_server.R")

source("shiny_aux/styles.R")

# ---- Load data once (runs when the app starts) ----
char_path <- "characteristics_tables/All_Study_Characteristics.xlsx"
char_data <- read_excel(char_path, sheet = "Study Characteristics")
pop_long <- read_excel(char_path, sheet = "Population_Long")
n_long <- read_excel(char_path, sheet = "N_Long")
virus_long <- read_excel(char_path, sheet = "Virus_Long")
rob_long <- read_excel(char_path, sheet = "RoB_Long")


# ---- User Interface ----
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Vaccine Integrity Project"),
  tags$style(HTML(filter_message_css)),
  tabsetPanel(
    tabPanel(
      "Studies",

      # Lightweight toggle
      checkboxInput(
        "lightweight",
        "Lightweight view (hide article details)",
        value = TRUE
      ),

      # ---- Simple filters (always visible) ----
      # ageGroupFilterUI("age_group"),
      # popTypeFilterUI("pop_type"),
      # virusFilterUI("virus"),
      # studyDesignFilterUI("study_design"),
      # robFilterUI("rob"),

      simpleFiltersUI("simple_filters"),

      # ---- Advanced filters (collapsible) ----
      advancedFiltersUI("advanced"),

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
)

# ---- Server logic ----
server <- function(input, output, session) {
  # age_filter <- ageGroupFilterServer("age_group", pop_long)
  # pop_type_filter <- popTypeFilterServer("pop_type", pop_long)
  # virus_filter <- virusFilterServer("virus", virus_long)
  # study_design_filter <- studyDesignFilterServer("study_design", char_data)
  # rob_filter <- robFilterServer("rob", rob_long)

  age_filter <- ageGroupFilterServer("simple_filters-age_group", pop_long)
  pop_type_filter <- popTypeFilterServer("simple_filters-pop_type", pop_long)
  virus_filter <- virusFilterServer("simple_filters-virus", virus_long)
  study_design_filter <- studyDesignFilterServer(
    "simple_filters-study_design",
    char_data
  )
  rob_filter <- robFilterServer("simple_filters-rob", rob_long)
  simpleFiltersServer("simple_filters")

  advanced_filter <- advancedFilterServer("advanced", pop_long)

  filtered_data <- reactive({
    ids_age <- age_filter()
    ids_type <- pop_type_filter()
    ids_virus <- virus_filter()
    ids_design <- study_design_filter()
    ids_rob <- rob_filter()
    ids_adv <- advanced_filter()

    keep_ids <- Reduce(
      intersect,
      list(ids_age, ids_type, ids_virus, ids_design, ids_rob, ids_adv)
    )
    char_data %>% filter(char_row_id %in% keep_ids)
  })

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
    display
  })

  output$studies_table <- renderDT({
    display <- processed_data()
    # Build columnDefs
    col_defs <- make_col_defs(display, desired_widths)

    # Append definitions for the hidden sorting column
    # DT column indices are 0‑based
    total_n_visible_idx <- which(names(display) == "Total N") - 1
    n_numeric_idx <- which(names(display) == "N_numeric") - 1

    col_defs <- c(
      col_defs,
      list(
        list(targets = total_n_visible_idx, orderData = n_numeric_idx),
        list(targets = n_numeric_idx, visible = FALSE)
      )
    )

    # --- datatable ---
    datatable(
      display,
      # server = FALSE   # set to TRUE when you have >1000 rows
      rownames = FALSE,
      escape = FALSE, # don't escape HTML chars
      filter = "none", # no column filters yet
      options = list(
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
}

# ---- Run the app ----
shinyApp(ui, server)
