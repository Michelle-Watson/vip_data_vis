# ---- Simple Age Group filter (checkboxes) ----
ageGroupFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Age Group", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("ages"),
      label = NULL,
      choices = c(
        "Older Adults (≥ 65 years)" = "OA",
        "Adults (19 - 64 years)" = "A",
        "Children (1 - 18 years)" = "C",
        "Infants (< 1 year)" = "I"
      ),
      selected = NULL
    ),
    tags$div(class = "filter-message", textOutput(ns("age_message")))
  )
}

# ---- Simple Population Type filter (checkboxes) ----
popTypeFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Population Type", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("pop_types"),
      label = NULL,
      choices = c(
        "Immunocompromised" = "IC",
        "High‑risk" = "HR",
        "Pregnant" = "P",
        "Healthcare personnel" = "H"
      ),
      selected = NULL
    ),
    tags$div(class = "filter-message", textOutput(ns("type_message")))
  )
}

virusFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Virus", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("viruses"),
      label = NULL,
      choices = c("Influenza", "COVID", "RSV"),
      selected = NULL # none selected = no filter (show all)
    ),
    tags$div(class = "filter-message", textOutput(ns("virus_message")))
  )
}

studyDesignFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Study Design", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("designs"),
      label = NULL,
      choices = c(
        "RCT" = "RCT",
        "Observational - with comparator group" = "Observational - with comparator group",
        "Non-randomized single arm" = "Non-randomized single arm",
        "Ecological" = "Ecological"
      ),
      selected = NULL
    ),
    tags$div(class = "filter-message", textOutput(ns("design_message")))
  )
}

robFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Risk of Bias", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("rob_levels"),
      label = NULL,
      choices = c(
        "Low" = "Low",
        "Some Concerns / Moderate" = "Some Concerns / Moderate",
        "High / Serious" = "High / Serious",
        "Critical" = "Critical"
      ),
      selected = NULL
    ),
    tags$div(class = "filter-message", textOutput(ns("rob_message")))
  )
}

domainFilterUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Domains", style = "margin-bottom: 20px;"),
    checkboxGroupInput(
      ns("domains"),
      label = NULL,
      choices = c(
        "Effectiveness" = "Effectiveness",
        "Safety" = "Safety",
        "Pregnancy-specific safety" = "Pregnancy-specific safety",
        "Epidemiologic" = "Epidemiologic"
      ),
      selected = NULL
    ),
    tags$div(class = "filter-message", textOutput(ns("domain_message")))
  )
}

simpleFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    # Row containing the "Show" toggle, the clear button, and the "Cleared!" message
    div(
      style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
      checkboxInput(ns("show_simple"), "Show simple filters", value = TRUE),
      div(
        style = "display: flex; align-items: baseline; gap: 8px;",
        actionLink(
          ns("clear_simple"),
          label = "Clear all simpler filters\U1F9F9",
          style = "color: #888; text-decoration: none; cursor: pointer;"
        ),
        textOutput(ns("cleared_msg"))
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("show_simple"), "'] == true"),
      wellPanel(
        fluidRow(
          class = "filter-row",
          column(2, ageGroupFilterUI(ns("age_group"))),
          column(2, popTypeFilterUI(ns("pop_type"))),
          column(2, virusFilterUI(ns("virus"))),
          column(2, studyDesignFilterUI(ns("study_design"))),
          column(2, domainFilterUI(ns("domain"))),
          column(2, robFilterUI(ns("rob")))
        )
      )
    )
  )
}

# wellpanel makes the bg grey

# most recent, lets try this
advancedFiltersUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "display: flex; align-items: baseline; justify-content: space-between; margin-bottom: 10px;",
      checkboxInput(ns("show"), "Show advanced filters", value = FALSE),
      div(
        style = "display: flex; align-items: baseline; gap: 8px;",
        actionLink(
          ns("clear_advanced"),
          label = "Clear all advanced filters \U1F9F9",
          style = "color: #888; text-decoration: none; cursor: pointer;"
        ),
        textOutput(ns("cleared_msg"))
      )
    ),
    conditionalPanel(
      condition = paste0("input['", ns("show"), "'] == true"),
      wellPanel(
        tags$p(
          style = "font-size: 12px; color: #555; margin-bottom: 12px;",
          "Ignore = you don’t mind whether this group is present or not. A study with this tag will be shown as long as the other criteria are satisfied."
        ),
        fluidRow(
          class = "filter-row",
          column(
            6,
            h4("Age Group (advanced)"),
            uiOutput(ns("age_advanced")),
            tags$div(
              class = "filter-message",
              textOutput(ns("age_adv_message"))
            )
          ),
          column(
            6,
            h4("Population Type (advanced)"),
            uiOutput(ns("pop_type_advanced")),
            tags$div(
              class = "filter-message",
              textOutput(ns("pop_type_adv_message"))
            )
          )
        )
      )
    )
  )
}
