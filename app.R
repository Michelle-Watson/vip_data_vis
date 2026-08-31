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
library(bslib)

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
# outcome_domain_long <- outcome_data %>%
#   select(row_id, Domain)
outcome_domain_long <- outcome_data %>%
  select(row_id, Domain) %>%
  mutate(
    Domain = if_else(
      grepl("epidem|ecological", Domain, ignore.case = TRUE),
      "Epidemiologic",
      Domain
    )
  )

# Build risk-of-bias mapping for outcomes – one rating per outcome row
outcome_rob_long <- outcome_data %>%
  select(row_id, `Risk of Bias`) %>%
  rename(`Overall Risk` = `Risk of Bias`)


# Load footnotes from both source workbooks
char_footnotes <- read_excel(char_path, sheet = "Footnotes", col_types = "text")
outcome_footnotes <- read_excel(
  outcome_path,
  sheet = "Footnotes",
  col_types = "text"
)

combined_footnotes <- bind_rows(char_footnotes, outcome_footnotes) %>%
  distinct(Term, Definition)

# print(combined_footnotes)

# Custom definitions table (used on About page)
# Custom definitions table for the About page
custom_definitions <- tribble(
  ~Term                                                                     , ~Definition                                                                                                                                                                                                                                                                                      ,
  "OA"                                                                      , "Older adults (≥65 years)"                                                                                                                                                                                                                                                                       ,
  "A"                                                                       , "Adults (19-64 years)"                                                                                                                                                                                                                                                                           ,
  "I"                                                                       , "Infants (<12 months)"                                                                                                                                                                                                                                                                           ,
  "C"                                                                       , "Children (12 months-18 years)"                                                                                                                                                                                                                                                                  ,
  "IC"                                                                      , "Immunocompromised"                                                                                                                                                                                                                                                                              ,
  "HR"                                                                      , "Other co-occurring conditions (broader than immunocompromised; includes other chronic conditions)"                                                                                                                                                                                              ,
  "P"                                                                       , "Pregnant individuals (and infants born after vaccination during pregnancy)"                                                                                                                                                                                                                     ,
  "H"                                                                       , "Healthcare personnel"                                                                                                                                                                                                                                                                           ,
  "Older Adults"                                                            , "Older adults (≥65 years)"                                                                                                                                                                                                                                                                       ,
  "Adults"                                                                  , "Adults (19-64 years)"                                                                                                                                                                                                                                                                           ,
  "Infants"                                                                 , "Infants (<12 months)"                                                                                                                                                                                                                                                                           ,
  "Children"                                                                , "Children (12 months-18 years)"                                                                                                                                                                                                                                                                  ,
  "Immunocompromised"                                                       , "Immunocompromised"                                                                                                                                                                                                                                                                              ,
  "Other co-occurring conditions"                                           , "Other co-occurring conditions (broader than immunocompromised; includes other chronic conditions)"                                                                                                                                                                                              ,
  "Pregnant"                                                                , "Pregnant individuals (and infants born after vaccination during pregnancy)"                                                                                                                                                                                                                     ,
  "Healthcare personnel"                                                    , "Healthcare personnel"                                                                                                                                                                                                                                                                           ,
  "Study Period"                                                            , "Season range from the outcomes sheet, formatted as Start - End. NR if not reported."                                                                                                                                                                                                            ,
  "Study Period Start"                                                      , "Start season/year from the outcomes sheet."                                                                                                                                                                                                                                                     ,
  "Study Period End"                                                        , "End season/year from the outcomes sheet."                                                                                                                                                                                                                                                       ,
  "Total N"                                                                 , "Total sample size (may be number of participants, mother‑infant pairs, or reports)"                                                                                                                                                                                                             ,
  "Age Range"                                                               , "Displayed as X - Y unit; - = not reported"                                                                                                                                                                                                                                                      ,
  "Minimum Age"                                                             , "Raw minimum age from data; - = not reported"                                                                                                                                                                                                                                                    ,
  "Maximum Age"                                                             , "Raw maximum age from data; - = not reported"                                                                                                                                                                                                                                                    ,
  "Minimum Age (days)"                                                      , "Minimum age converted to days (0 = no lower bound). Used for slider filtering only."                                                                                                                                                                                                            ,
  "Maximum Age (days)"                                                      , "Maximum age converted to days (36525 ≈ 100 years = no upper bound). Used for slider filtering only."                                                                                                                                                                                            ,
  "Vaccine Formulation for Comparator Arm"                                  , "'Not Vaccinated', 'No Comparator', 'Ineligible Comparator', 'No Vaccine', 'No Updated Dose', 'Unknown Status', and 'Historical Comparator' are comparator descriptions. 'Hepatitis A' and 'MenC' are non-influenza/COVID/RSV vaccine comparators. 'NR' means the information was not reported." ,
  "Strain Targeted by Intervention Vaccine"                                 , "Shortened strain name from strain map; - = not specified"                                                                                                                                                                                                                                       ,
  "Strain Targeted by Comparator Vaccine"                                   , "Shortened strain name from strain map; - = not specified"                                                                                                                                                                                                                                       ,
  "Sample size intervention"                                                , "NR = not reported"                                                                                                                                                                                                                                                                              ,
  "Number of events in intervention arm"                                    , "NR = not reported; 0 = zero events"                                                                                                                                                                                                                                                             ,
  "Number of events in comparator arm"                                      , "NR = not reported; 0 = zero events"                                                                                                                                                                                                                                                             ,
  "Number of events (ecological studies)"                                   , "NA (not an ecological study) = not applicable for non-ecological designs; NR = not reported (ecological study missing data)"                                                                                                                                                                    ,
  "N total (ecological studies)"                                            , "NA (not an ecological study) = not applicable for non-ecological designs; NR = not reported (ecological study missing data)"                                                                                                                                                                    ,
  "Risk of Bias"                                                            , "Assessment in Progress = bias assessment not yet completed or field was empty"                                                                                                                                                                                                                  ,
  "Risk of Bias domains marked Some Concerns (RoB2) or Moderate (ROBINS‑I)" , "Specific domains that were rated as Some Concerns (RoB2) or Moderate (ROBINS‑I). Assessment in Progress if empty."                                                                                                                                                                              ,
  "Risk of Bias domains marked High (RoB2) or Serious/Critical (ROBINS‑I)"  , "Specific domains that were rated as High (RoB2) or Serious/Critical (ROBINS‑I). Assessment in Progress if empty."                                                                                                                                                                               ,
  "Gestational Age Min (weeks)"                                             , "Minimum gestational age in weeks (for pregnant populations). NA if not reported."                                                                                                                                                                                                               ,
  "Gestational Age Max (weeks)"                                             , "Maximum gestational age in weeks (for pregnant populations). NA if no upper limit or not reported."                                                                                                                                                                                             ,
  "Gestational Age Display"                                                 , "Display string for gestational age range (e.g., 'GA: 14-27w'). '-' for non-pregnant populations."                                                                                                                                                                                               ,
  "Estimate (95% CI)"                                                       , "Estimates rounded to 2 decimal places. NE (...) and NR (...) are shown as entered"                                                                                                                                                                                                              ,
  "Estimate Type"                                                           , "Type of estimate (e.g., OR = Odds Ratio, VE = Vaccine Effectiveness, IRR = Incidence Rate Ratio, TR = Time Ratio, CR = Cum­ulative Risk). NR if not reported."                                                                                                                                  ,
  "Point Estimate"                                                          , "Numeric point estimate (parsed)."                                                                                                                                                                                                                                                               ,
  "CI Lower"                                                                , "Numeric lower CI bound (parsed)."                                                                                                                                                                                                                                                               ,
  "CI Upper"                                                                , "Numeric upper CI bound (parsed)."                                                                                                                                                                                                                                                               ,
  "Definition [of outcome]"                                                 , "Case definition used for the outcome."                                                                                                                                                                                                                                                          ,
  "Factors Adjusted"                                                        , "NA (RCT) = no adjustment required; Unadjusted = no adjustment variables entered; - = Estimate not calculable; otherwise comma‑separated list"                                                                                                                                                   ,
  "Study Design"                                                            , "'Non‑randomized comparative' is renamed to 'Observational - with comparator group'"                                                                                                                                                                                                             ,
  "Study Design Specifics"                                                  , "Additional design details from extractors (e.g., post‑hoc, subanalysis); NR if not provided"                                                                                                                                                                                                    ,
  "Type of Outcome"                                                         , "Analysis category for filtering: Adjusted Comparative, Unadjusted Comparative, Single Arm, Ecological."                                                                                                                                                                                         ,
  "Vaccine name shortening"                                                 , "Long vaccine descriptions are shortened to formulation abbreviations (e.g., IIV4, BNT162b2); brand names appear in separate columns"                                                                                                                                                            ,
  "Follow-up"                                                               , "Follow-up time as reported in the consensus sheet. NR if not reported."                                                                                                                                                                                                                         ,
  "Follow-up (days)"                                                        , "Follow-up time converted to days. Used for filtering/sorting if needed."                                                                                                                                                                                                                        ,
  "Subgroup letter"                                                         , "Appended to study label when a row reports a subgroup already covered by another row – alerts reader to avoid double‑counting"                                                                                                                                                                  ,
  "Notes 1"                                                                 , "Free‑text notes from the extractor (e.g. narrative summary)"                                                                                                                                                                                                                                    ,
  "Notes 2"                                                                 , "Free‑text notes from the extractor (e.g. narrative summary)"
)

# ---- User Interface ----
ui <- fluidPage(
  # experiment with bootstrap themes. ver 3
  # theme = bs_theme(version = 5, bootswatch = "flatly")
  # theme = bs_theme(version = 5),
  useShinyjs(),
  # titlePanel("VIP 2026-2027"),
  # tags$div(
  #   class = "top-controls",
  #   input_dark_mode(id = "theme_mode", mode = "light"),
  #   tags$img(
  #     src = "VIP_Logo_Horizontal.png",
  #     class = "app-logo"
  #   )
  # )
  tags$div(
    class = "top-controls",
    # actionLink(
    #   inputId = "toggle_dark",
    #   label = icon("moon"),
    #   class = "dark-toggle"
    # ),
    tags$img(
      src = "VIP_Logo_Horizontal.png",
      class = "app-logo"
    )
  ),
  tags$style(HTML(filter_message_css)),
  tabsetPanel(
    tabPanel(
      "About",
      tags$div(
        class = "about-page-content",

        tags$div(
          class = "about-logo-card",
          tags$img(src = "VIP_Logo_Horizontal.png", class = "about-logo")
        ),

        tags$h1(
          "Interactive Data Tool: Evidence for Respiratory Season Immunizations (2026-2027 update)"
        ),

        tags$p(
          "This tool aims to summarize newly published research evaluating the effectiveness and safety of vaccine and immunization products for COVID-19, influenza, and respiratory syncytial virus (RSV) that are available in the United States."
        ),

        tags$h3("About the tool"),
        tags$p(
          HTML(
            "Data on respiratory virus immunizations were identified through systematic reviews (one per virus, three total) of peer-reviewed research published between August 2025 and June 2026. Updating the Vaccine Integrity Project’s evidence review for the ",
            "<a href='https://www.nejm.org/doi/full/10.1056/NEJMsa2514268'>2025-26 respiratory virus season</a>, ",
            "these reviews followed a structured and transparent process for identifying the science underpinning recommendations for respiratory virus immunizations. The review methods and results are reported more extensively in the protocols and publications linked below."
          )
        ),

        tags$h3("How to use this tool"),
        tags$p(
          "Visit the “studies” or “outcome” tab, then filter the results by virus, population or study characteristics, or the type of information reported."
        ),
        tags$ul(
          tags$li(
            tags$strong("Studies tab"),
            " - a list of included studies with reference information, study characteristics, and risk of bias assessment"
          ),
          tags$li(
            tags$strong("Outcomes tab"),
            " - estimates of vaccine effectiveness or efficacy (VE) and safety extracted from included studies"
          )
        ),
        tags$p(
          "The default view is “lightweight” to optimize screen space; uncheck “lightweight view” to show all columns."
        ),

        tags$h3("Read the systematic review publications"),
        tags$p("Published online September 2, 2026"),
        tags$ul(
          tags$li(tags$a(
            "COVID-19", #  (doi:10.1001/jama.2026.18191)
            href = "https://doi.org/10.1001/jama.2026.18191"
          )),
          tags$li(tags$a(
            "Influenza", #  (doi:10.1001/jama.2026.18126)
            href = "https://doi.org/10.1001/jama.2026.18126"
          )),
          tags$li(tags$a(
            "RSV", # (doi:10.1001/jama.2026.17871)
            href = "https://doi.org/10.1001/jama.2026.17871"
          ))
        ),

        tags$h3("Read the review protocols"),
        tags$p("Registered on April 14, 2026; updated on May 16, 2026"),
        tags$ul(
          tags$li(tags$a(
            "COVID-19",
            href = "https://www.crd.york.ac.uk/PROSPERO/view/CRD420261365950"
          )),
          tags$li(tags$a(
            "Influenza",
            href = "https://www.crd.york.ac.uk/PROSPERO/view/CRD420261365916"
          )),
          tags$li(tags$a(
            "RSV",
            href = "https://www.crd.york.ac.uk/PROSPERO/view/CRD420261365938"
          ))
        ),

        tags$h3("Access the source data and code"),
        tags$ul(
          tags$li(tags$a(
            "View this project on GitHub",
            href = "https://github.com/michelle-Watson",
            target = "_blank"
          ))
        ),

        tags$h3("Cite this tool"),
        tags$p(
          "Watson, M. (2026). ",
          tags$em("Respiratory Season Data Dashboard (2026-2027)"),
          " (Version 1.0.0) [Computer software]. Zenodo. ",
          tags$a(
            href = "https://zenodo.org/records/22199763",
            target = "_blank",
            "https://doi.org/10.5281/zenodo.22199763"
          )
        ),

        tags$h3("Contact"),
        tags$p(
          "Developer: Michelle Watson - ",
          tags$a(
            href = "mailto:michellealiciawatson@gmail.com",
            "michellealiciawatson@gmail.com"
          )
        ),
        tags$p(
          "Questions or feedback about the data tool? Email: ",
          tags$a(
            href = "mailto:admin@evidencefoundation.org",
            "admin@evidencefoundation.org"
          )
        ),

        tags$div(
          class = "definitions-box",
          tags$details(
            tags$summary("Definitions / Abbreviations"),
            tags$div(
              class = "definitions-content",
              tableOutput("about_definitions_table")
            )
          )
        ),

        tags$hr(),

        tags$p(
          "This page was created using ",
          tags$a("shiny", href = "https://shiny.posit.co/"),
          ": Web Application Framework for R. ",
          "Chang W, Cheng J, Allaire J, Sievert C, Schloerke B, Aden-Buie G, Xie Y, Allen J, McPherson J, Dipert A, Borges B (2026). shiny: Web Application Framework for R. R package version 1.14.0.9000, https://shiny.posit.co/."
        )
      )
    ),
    # tabPanel(
    #   "Studies",
    #   filterSidebarLayout(
    #     sidebar_id = "studies_sidebar",
    #     toggle_btn = FALSE,
    #     sidebar_content = tagList(
    #       checkboxInput(
    #         "lightweight",
    #         "Lightweight view (hide article details)",
    #         value = TRUE
    #       ),
    #       div(
    #         style = "margin-bottom: 8px;",
    #         downloadButton(
    #           "downloadFilteredStudies",
    #           "Download filtered view",
    #           class = "btn-sm"
    #         )
    #       ),
    #       simpleFiltersUI("simple_filters"),
    #       advancedFiltersUI("advanced")
    #     ),
    #     main_content = tagList(
    #       tags$div(
    #         class = "filter-message",
    #         style = "margin-bottom: 10px; font-size: 15px;",
    #         textOutput("study_count")
    #       ),
    #       div(
    #         style = "display: flex; justify-content: flex-end; margin: 12px 0 8px 0;",
    #         downloadButton(
    #           "downloadFull",
    #           "Download Study Characteristics",
    #           class = "btn-sm"
    #         )
    #       ),
    #       # DTOutput("studies_table")
    #       div(class = "table-responsive", DTOutput("studies_table"))
    #     )
    #   )
    # )
    tabPanel(
      "Studies",
      layout_sidebar(
        #height = "calc(67vh + 140px)",
        sidebar = sidebar(
          width = 300,
          resizable = FALSE,
          open = list(desktop = "open", mobile = "closed"),
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
        # Main content
        tags$div(
          class = "main-content",
          # tags$p(
          #   style = "color: #666; font-size: 12px; margin-bottom: 6px;",
          #   "Use the arrow button to show or hide the filter sidebar."
          # ),
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
          div(class = "table-responsive", DTOutput("studies_table"))
        )
      )
    ),

    # tabPanel(
    #   "Outcomes",
    #   filterSidebarLayout(
    #     sidebar_id = "outcomes_sidebar",
    #     toggle_btn = FALSE,
    #     sidebar_content = tagList(
    #       checkboxInput(
    #         "outcome_lightweight",
    #         "Lightweight view (hide counts, outcome definition, factors adjusted)",
    #         value = TRUE
    #       ),
    #       div(
    #         style = "margin-bottom: 8px;",
    #         downloadButton(
    #           "downloadFilteredOutcomes",
    #           "Download filtered view",
    #           class = "btn-sm"
    #         )
    #       ),
    #       outcomeSimpleFiltersUI("outcome_simple_filters"),
    #       advancedFiltersUI("outcome_advanced")
    #     ),
    #     main_content = tagList(
    #       tags$div(
    #         class = "filter-message",
    #         style = "margin-bottom: 10px; font-size: 15px;",
    #         textOutput("outcome_count")
    #       ),
    #       div(
    #         style = "display: flex; align-items: center; justify-content: space-between; margin: 12px 0 8px 0; gap: 10px;",
    #         tags$span(
    #           style = "color: #888; font-size: 12px; font-style: italic;",
    #           "Ecological studies, and other studies, may not appear in the outcomes view if they did not report an estimate. All extracted data remains available for download."
    #         ),
    #         downloadButton(
    #           "downloadOutcomes",
    #           "Download Outcomes",
    #           class = "btn-sm"
    #         )
    #       ),
    #       # DTOutput("outcomes_table")
    #       div(class = "table-responsive", DTOutput("outcomes_table"))
    #     )
    #   )
    # )
    tabPanel(
      "Outcomes Data",
      layout_sidebar(
        # height = "calc(59vh + 170px)",
        sidebar = sidebar(
          width = 300,
          resizable = FALSE,
          open = list(desktop = "open", mobile = "closed"),
          checkboxInput(
            "outcome_lightweight",
            "Lightweight view (hide counts, outcome definition, factors adjusted)",
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
        # Main content
        tags$div(
          class = "main-content",
          # tags$p(
          #   style = "color: #666; font-size: 12px; margin-bottom: 6px;",
          #   "Use the arrow button to show or hide the filter sidebar."
          # ),
          tags$div(
            class = "filter-message",
            style = "margin-bottom: 10px; font-size: 15px;",
            textOutput("outcome_count")
          ),
          div(
            style = "display: flex; align-items: center; justify-content: space-between; margin: 12px 0 8px 0; gap: 10px;",
            tags$span(
              style = "color: #888; font-size: 12px; font-style: italic;",
              "Ecological studies, and other studies, may not appear in the outcomes view if they did not report an estimate. All extracted data remains available for download."
            ),
            downloadButton(
              "downloadOutcomes",
              "Download Outcomes",
              class = "btn-sm"
            )
          ),
          div(class = "table-responsive", DTOutput("outcomes_table"))
        )
      )
    ),
  )
)

# ---- Server logic ----
server <- function(input, output, session) {
  # Uncomment or bootstrap style
  # bslib::bs_themer()
  # observe({
  #   session$setCurrentTheme(
  #     if (input$theme_mode == "dark") {
  #       bs_theme(version = 5, bootswatch = "darkly")
  #     } else {
  #       bs_theme(version = 5)
  #     }
  #   )
  # })

  # observeEvent(input$toggle_dark, {
  #   shinyjs::toggleClass(selector = "body", class = "dark-mode")
  # })

  output$about_definitions_table <- renderTable(
    {
      combined_footnotes
    },
    rownames = FALSE,
    colnames = TRUE,
    striped = TRUE,
    bordered = FALSE,
    spacing = "s",
    align = "l"
  )
  # output$about_definitions <- renderUI({
  #   if (nrow(combined_footnotes) == 0) {
  #     return(p("No definitions available."))
  #   }
  #
  #   tagList(
  #     lapply(seq_len(nrow(combined_footnotes)), function(i) {
  #       tags$div(
  #         class = "definition-item",
  #         tags$span(class = "definition-term", combined_footnotes$Term[i]),
  #         tags$span(class = "definition-sep", " — "),
  #         tags$span(class = "definition-text", combined_footnotes$Definition[i])
  #       )
  #     })
  #   )
  # })

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
    data = char_data,
    id_col = "char_row_id",
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

    # Display only the numeric N value (no text)
    display$`Total N` <- display$N_numeric

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

    # Add responsive priorities: lower = more important, collapses later
    priority_map <- c(
      "Study" = 1,
      "Virus" = 2,
      "Population" = 3,
      "Total N" = 4,
      "Study Design" = 5,
      "Domains Reported" = 6,
      "Risk of Bias" = 7,
      "Study Period" = 8,
      "Country/Region" = 9,
      "Funding Source" = 10,
      "Age (years)" = 11,
      "Setting Details" = 12,
      "Vaccine Products" = 13,
      "Journal" = 14,
      "PMID" = 15,
      "PMCID" = 16,
      "DOI" = 17,
      "Link" = 18,
      "Study Design Specifics" = 19,
      "Study Period Start" = 20,
      "Study Period End" = 21
    )

    # Keep only priorities for columns that are actually present
    present <- intersect(names(priority_map), names(display))

    for (col in present) {
      col_defs <- c(
        col_defs,
        list(list(
          targets = col,
          responsivePriority = as.integer(priority_map[[col]])
        ))
      )
    }

    # Study should always stay visible, even if other columns collapse
    col_defs <- c(
      col_defs,
      list(list(
        targets = "Study",
        className = "all"
      ))
    )

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
      extensions = c("FixedColumns", "Responsive"),
      # extensions = "FixedColumns",
      # extensions = "Responsive", # responsive for mobile, try it out. it DISABLES horizontal scrolling, we will always ONLY SHOW what can fit on the screen
      options = list(
        # showing # of # + pagination at bottom. Top only has pagination f=filter=search bar
        dom = "<'top' f> t <'bottom' i p>",
        pageLength = 50,
        # scrollX = FALSE,
        scrollX = TRUE,

        # Enable internal vertical + horizontal scrolling. NEED to set vetical heigh for table to freeze the top row
        # scrollY = "67vh", #69 vh?
        # scrollY = "calc(100vh - 190px)",
        scrollY = "66vh",
        scrollX = TRUE,

        # Freeze the first column
        fixedColumns = list(leftColumns = 1),

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
      wb_sheets <- readxl::excel_sheets(char_path)
      all_data <- lapply(wb_sheets, function(s) {
        read_excel(char_path, sheet = s)
      })
      names(all_data) <- wb_sheets

      cols_to_remove <- c("char_row_id", "Covidence ID")

      all_data <- lapply(all_data, function(df) {
        drop_columns(df, cols_to_remove)
      })

      writexl::write_xlsx(all_data, file)
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

      # Remove columns from every sheet in the export
      cols_to_remove <- c("char_row_id", "Covidence ID")

      all_data <- lapply(all_data, function(df) {
        drop_columns(df, cols_to_remove)
      })

      writexl::write_xlsx(all_data, file)
    }
  )

  # ---- Outcomes tab ----

  # 1. Create a separate study‑period filter for outcomes
  outcome_study_period <- studyPeriodFilterServer(
    "outcome_simple_filters-study_period",
    data = outcome_data,
    id_col = "row_id",
    all_ids = all_outcome_ids
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
  # outcome_design_filter <- studyDesignFilterServer(
  #   "outcome_simple_filters-study_design",
  #   char_data,
  #   all_ids = all_outcome_ids
  # )
  outcome_design_filter <- outcomeStudyDesignFilterServer(
    "outcome_simple_filters-study_design",
    outcome_data = outcome_data,
    id_col = "row_id",
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
    # # Filters that return row_id
    # ids_age <- outcome_age_filter()
    # ids_type <- outcome_type_filter()
    # ids_virus <- outcome_virus_filter()
    # ids_domain <- outcome_domain_filter()
    # ids_rob <- outcome_rob_filter()
    # ids_type_of_outcome <- outcome_type_of_outcome_filter()
    # ids_period <- outcome_study_period$ids()
    # ids_adv <- outcome_advanced_filter()
    #
    # # Filters that still return char_row_id
    # ids_design <- outcome_design_filter()
    #
    # # Convert char_row_id → row_id
    # char_to_row <- outcome_data %>% distinct(char_row_id, row_id)
    # char_ids <- Reduce(intersect, list(ids_design))
    # char_row_ids <- char_to_row %>%
    #   filter(char_row_id %in% char_ids) %>%
    #   pull(row_id)
    #
    # # Final intersection
    # result_ids <- Reduce(
    #   intersect,
    #   list(
    #     ids_age,
    #     ids_type,
    #     ids_virus,
    #     ids_type_of_outcome,
    #     ids_domain,
    #     ids_rob,
    #     ids_period,
    #     ids_adv,
    #     char_row_ids
    #   )
    # )

    # All outcome filters now return outcome row IDs (`row_id`),
    # so we can intersect them directly without any study‑level conversion.
    ids_age <- outcome_age_filter()
    ids_type <- outcome_type_filter()
    ids_virus <- outcome_virus_filter()
    ids_domain <- outcome_domain_filter()
    ids_rob <- outcome_rob_filter()
    ids_type_of_outcome <- outcome_type_of_outcome_filter()
    ids_design <- outcome_design_filter()
    ids_period <- outcome_study_period$ids()
    ids_adv <- outcome_advanced_filter()

    result_ids <- Reduce(
      intersect,
      list(
        ids_age,
        ids_type,
        ids_virus,
        ids_design,
        ids_type_of_outcome,
        ids_domain,
        ids_rob,
        ids_period,
        ids_adv
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
    # message("  char_row_ids: ", length(char_row_ids))
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
      # char_data %>% filter(char_row_id %in% filtered_outcome_ids())
      outcome_data %>% filter(row_id %in% filtered_outcome_ids())
    )
  })

  # 6. Process the outcome table (plain study labels, column hiding, sorting)
  processed_outcome_data <- reactive({
    display <- filtered_outcome_data()

    # Add Link/DOI from Study Characteristics for clickable Study Label
    display <- display %>%
      left_join(
        char_data %>%
          select(char_row_id, Link, DOI) %>%
          mutate(char_row_id = as.character(char_row_id)),
        by = "char_row_id"
      )

    # Sort alphabetically by plain Study Label BEFORE converting to HTML
    display <- display %>% arrange(`Study Label`)

    # Make the main study label clickable
    display <- make_label_clickable(display, "Study Label")

    # Exclude rows where Point Estimate is missing / NR / NA
    display <- display %>%
      filter(
        !is.na(`Point Estimate`) &
          trimws(`Point Estimate`) != "" &
          !trimws(`Point Estimate`) %in% c("NR", "NA")
      )

    # Hide unwanted columns
    display <- drop_columns(display, outcome_always_hide)

    # Rename Vaccine -> Comparison
    names(display)[names(display) == "Vaccine"] <- "Comparison"

    # Rename Type of Outcome -> Type of Estimate
    names(display)[names(display) == "Type of Outcome"] <- "Type of Estimate"

    # Rename Domain -> Type of Outcome
    names(display)[names(display) == "Domain"] <- "Type of Outcome"

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
          "Sample size comparator",
          "Factors Adjusted",
          "Study Period",
          "Definition [of outcome]"
        )
      )
    }

    # Reorder columns
    display <- display[,
      intersect(outcome_column_order, names(display)),
      drop = FALSE
    ]

    # Sort alphabetically by Study Label
    # display <- display %>% arrange(`Study Label`)
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

    # Add responsive priorities: lower = more important, collapses later
    outcome_priority_map <- c(
      "Study Label" = 1,
      "Outcome" = 2,
      "Estimate (95% CI)" = 3,
      "Population" = 4,
      "Vaccine Formulation for Intervention Arm" = 5,
      "Vaccine Formulation for Comparator Arm" = 6,
      "Age Range" = 7,
      "Study Design" = 8,
      "Definition [of outcome]" = 9,
      "Risk of Bias" = 10,
      "Factors Adjusted" = 11,
      "Sample size intervention" = 12,
      "Sample size comparator" = 13,
      "Follow-up" = 14,
      "Number of events in intervention arm" = 15,
      "Number of events in comparator arm" = 16,
      "Strain Targeted by Intervention Vaccine" = 17,
      "Strain Targeted by Comparator Vaccine" = 18,
      "Comparison" = 19,
      "Study Period" = 20,
      "Number of events (ecological studies)" = 21,
      "N total (ecological studies)" = 22,
      "Follow-up (days)" = 23,
      "Minimum Age" = 24,
      "Maximum Age" = 25,
      "Intervention" = 26,
      "Comparator" = 27,
      "Type of Estimate" = 28,
      "Estimate Type" = 29
    )

    present <- intersect(names(outcome_priority_map), names(display))

    for (col in present) {
      col_defs <- c(
        col_defs,
        list(list(
          targets = col,
          responsivePriority = as.integer(outcome_priority_map[[col]])
        ))
      )
    }

    # Study Label must always remain visible
    col_defs <- c(
      col_defs,
      list(list(
        targets = "Study Label",
        className = "all"
      ))
    )

    # Add sorting helper for Estimate (95% CI) via hidden numeric columns if present
    # For now we skip extra sorting helpers – you can add them later if needed.

    datatable(
      display,
      rownames = FALSE,
      escape = FALSE,
      filter = "none",
      extensions = c("FixedColumns", "Responsive"),
      # extensions = "Responsive", # responsive for mobile, try it out. it DISABLES horizontal scrolling, we will always ONLY SHOW what can fit on the screen
      options = list(
        # dom = "<'top' p> t <'bottom' i p>",
        # showing # of # + pagination at bottom. Top only has pagination f=filter=search bar
        dom = "<'top' f> t <'bottom' i p>",
        pageLength = 25,

        # Must be set for the header row to freeze
        # scrollY = "calc(59vh - 1px)",
        # scrollY = "calc(100vh - 190px)",
        scrollY = "64vh",
        scrollX = TRUE,
        # scrollX = FALSE,

        fixedColumns = list(leftColumns = 1),

        autoWidth = FALSE,
        columnDefs = col_defs
      )
    )
  })

  # 9. Download handler for outcomes
  output$downloadOutcomes <- downloadHandler(
    filename = function() "Filtered_Outcomes.xlsx",
    content = function(file) {
      filt_df <- as.data.frame(
        filtered_outcome_data(),
        stringsAsFactors = FALSE
      )

      cols_to_remove <- c(
        "char_row_id",
        "row_id",
        "Study Label with subgroup indication",
        "Covidence ID",
        "Minimum Age (days)",
        "Maximum Age (days)"
      )

      filt_df <- drop_columns(filt_df, cols_to_remove)

      writexl::write_xlsx(filt_df, file)
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

      # 3. Remove unwanted columns from the filtered outcomes sheet
      cols_to_remove <- c(
        "char_row_id",
        "row_id",
        "Study Label with subgroup indication",
        "Covidence ID",
        "Minimum Age (days)",
        "Maximum Age (days)"
      )

      filt_df <- drop_columns(filt_df, cols_to_remove)

      # 4. Build a workbook with ONLY the cleaned filtered sheet + Footnotes
      out <- list(
        `Filtered_Outcomes` = filt_df,
        Footnotes = footnotes_df
      )

      # 5. Write the new workbook
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
