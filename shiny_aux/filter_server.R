studyPeriodFilterServer <- function(
  id,
  data,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    filtered_data_reactive <- reactiveVal(NULL)

    setFilteredData <- function(fd) {
      filtered_data_reactive(fd)
    }

    # Numeric years from the data, ignoring missing
    years_raw <- suppressWarnings(as.numeric(data[["Study Period Start"]]))
    years <- years_raw[!is.na(years_raw)]

    if (length(years) == 0) {
      min_yr <- 0
      max_yr <- 0
    } else {
      min_yr <- min(years)
      max_yr <- max(years)
    }

    output$slider_ui <- renderUI({
      sliderInput(
        session$ns("year_range"),
        NULL,
        min = min_yr,
        max = max_yr,
        value = c(min_yr, max_yr),
        step = 1,
        sep = ""
      )
    })

    ids <- reactive({
      sel <- input$year_range

      # If the slider is still at the full default range, do not filter.
      if (is.null(sel) || (sel[1] == min_yr && sel[2] == max_yr)) {
        return(unique(all_ids %||% data[[id_col]]))
      }

      # When a narrower range is selected, keep only rows with a valid start year.
      data %>%
        mutate(
          sp_start_num = suppressWarnings(as.numeric(`Study Period Start`))
        ) %>%
        filter(
          !is.na(sp_start_num),
          sp_start_num >= sel[1],
          sp_start_num <= sel[2]
        ) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })

    output$period_hist <- renderPlot(
      {
        fd <- filtered_data_reactive()
        req(fd)

        yrs <- suppressWarnings(as.numeric(fd[["Study Period Start"]]))
        yrs <- yrs[!is.na(yrs)]

        hist_df <- as.data.frame(table(yrs))
        names(hist_df) <- c("year", "count")

        ggplot(hist_df, aes(x = factor(year), y = count)) +
          geom_col(fill = "#007bc2", width = 0.55) +
          scale_x_discrete(
            breaks = sort(unique(hist_df$year)),
            labels = sort(unique(hist_df$year))
          ) +
          theme_minimal(base_size = 9) +
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.x = element_text(color = "#555"),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            plot.margin = margin(0, 0, 0, 0)
          )
      },
      height = 80,
      bg = "transparent"
    )

    output$period_message <- renderText({
      sel <- input$year_range
      if (is.null(sel) || (sel[1] == min_yr && sel[2] == max_yr)) {
        return("Showing all study periods")
      }
      paste("Showing studies with start year", sel[1], "to", sel[2])
    })

    list(
      ids = ids,
      setFilteredData = setFilteredData,
      resetSlider = function() {
        updateSliderInput(session, "year_range", value = c(min_yr, max_yr))
      }
    )
  })
}


# ---- Age Group filter server ----
ageGroupFilterServer <- function(
  id,
  pop_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Helper message
    output$age_message <- renderText({
      selected <- input$ages
      if (is.null(selected) || length(selected) == 0) {
        return("Showing all ages")
      }
      # Map short codes to full labels
      full_names <- pop_code_to_full[selected]
      # English‑style listing
      if (length(full_names) == 1) {
        paste("Showing studies that include", full_names)
      } else if (length(full_names) == 2) {
        paste(
          "Showing studies that include",
          full_names[1],
          "or",
          full_names[2]
        )
      } else {
        n <- length(full_names)
        paste(
          "Showing studies that include",
          paste(full_names[-n], collapse = ", "),
          "or",
          full_names[n]
        )
      }
    })

    # Filter logic, uses id_col -> variable with a string.either uses char_row_id (char based) or row_id (outcomes based)
    reactive({
      selected <- input$ages
      if (is.null(selected) || length(selected) == 0) {
        # return(unique(pop_long[[id_col]]))
        # rows were being silently dropped because a field wasn't filled out
        # Return the full set of IDs provided by the caller
        return(all_ids %||% unique(pop_long[[id_col]]))
      }
      pop_long %>%
        filter(`Population Code` %in% selected) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

# ---- Population Type filter server ----
popTypeFilterServer <- function(
  id,
  pop_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Helper message
    output$type_message <- renderText({
      selected <- input$pop_types
      if (is.null(selected) || length(selected) == 0) {
        return("Showing all population types")
      }
      full_names <- pop_code_to_full[selected]
      if (length(full_names) == 1) {
        paste("Showing studies that include", full_names)
      } else if (length(full_names) == 2) {
        paste(
          "Showing studies that include",
          full_names[1],
          "or",
          full_names[2]
        )
      } else {
        n <- length(full_names)
        paste(
          "Showing studies that include",
          paste(full_names[-n], collapse = ", "),
          "or",
          full_names[n]
        )
      }
    })

    # Filter logic
    reactive({
      selected <- input$pop_types
      if (is.null(selected) || length(selected) == 0) {
        # return(unique(pop_long[[id_col]]))
        # rows were being silently dropped because a field wasn't filled out
        # Return the full set of IDs provided by the caller
        return(all_ids %||% unique(pop_long[[id_col]]))
      }
      pop_long %>%
        filter(`Population Code` %in% selected) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

virusFilterServer <- function(
  id,
  virus_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Dynamic helper message
    output$virus_message <- renderText({
      selected <- input$viruses
      if (is.null(selected) || length(selected) == 0) {
        return("Showing studies for all viruses")
      }
      if (length(selected) == 1) {
        paste("Showing studies for", selected)
      } else if (length(selected) == 2) {
        paste("Showing studies for", selected[1], "or", selected[2])
      } else {
        paste("Showing studies for Influenza, COVID, or RSV")
      }
    })

    # Filter logic
    reactive({
      selected <- input$viruses
      if (is.null(selected) || length(selected) == 0) {
        # return(unique(virus_long[[id_col]]))
        # rows were being silently dropped because a field wasn't filled out
        # Return the full set of IDs provided by the caller
        return(all_ids %||% unique(virus_long[[id_col]]))
      }
      virus_long %>%
        filter(Virus %in% selected) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

studyDesignFilterServer <- function(id, char_data, all_ids = NULL) {
  moduleServer(id, function(input, output, session) {
    # Dynamic helper message
    output$design_message <- renderText({
      selected <- input$designs
      if (is.null(selected) || length(selected) == 0) {
        return("Showing all study designs")
      }
      if (length(selected) == 1) {
        paste("Showing", selected, "studies only")
      } else if (length(selected) == 2) {
        paste("Showing", selected[1], "or", selected[2], "studies")
      } else {
        paste("Showing selected study designs")
      }
    })

    # Filter logic - uses the main characteristics table directly
    reactive({
      selected <- input$designs
      if (is.null(selected) || length(selected) == 0) {
        return(all_ids %||% unique(char_data$char_row_id))
        # rows were being silently dropped because a field wasn't filled out
        # Return the full set of IDs provided by the caller
      }
      char_data %>%
        filter(`Study Design` %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

robFilterServer <- function(
  id,
  rob_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Mapping from simplified categories to original Overall Risk values
    category_map <- list(
      "Low" = c(
        "Low",
        "Low risk of bias (except for concerns about uncontrolled confounding)"
      ),
      "Some Concerns / Moderate" = c("Some Concerns", "Moderate"),
      "High / Serious" = c("High", "Serious"),
      "Critical" = c("Critical")
    )

    # Dynamic helper message
    output$rob_message <- renderText({
      selected <- input$rob_levels
      if (is.null(selected) || length(selected) == 0) {
        return("Showing studies with any risk of bias assessment")
      }
      if (length(selected) == 1) {
        paste("Showing studies with", selected, "risk of bias")
      } else if (length(selected) == 2) {
        paste(
          "Showing studies with",
          selected[1],
          "or",
          selected[2],
          "risk of bias"
        )
      } else {
        paste("Showing studies with selected risk of bias levels")
      }
    })

    # Filter logic
    reactive({
      selected <- input$rob_levels
      if (is.null(selected) || length(selected) == 0) {
        return(all_ids %||% unique(rob_long[[id_col]]))
      }
      original_risks <- unlist(category_map[selected])
      rob_long %>%
        filter(`Overall Risk` %in% original_risks) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

domainFilterServer <- function(
  id,
  domain_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    output$domain_message <- renderText({
      selected <- input$domains
      if (is.null(selected) || length(selected) == 0) {
        return("Showing studies from all domains")
      }
      if (length(selected) == 1) {
        paste("Showing studies that include", selected)
      } else if (length(selected) == 2) {
        paste("Showing studies that include", selected[1], "or", selected[2])
      } else {
        paste(
          "Showing studies that include",
          paste(selected[-length(selected)], collapse = ", "),
          "or",
          selected[length(selected)]
        )
      }
    })

    reactive({
      selected <- input$domains
      if (is.null(selected) || length(selected) == 0) {
        return(all_ids %||% unique(domain_long[[id_col]]))
      }
      domain_long %>%
        filter(Domain %in% selected) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

simpleFiltersServer <- function(id, reset_study_period = NULL) {
  moduleServer(id, function(input, output, session) {
    # Observe the clear button click (id is "clear_simple" inside this module)
    observeEvent(input$clear_simple, {
      # Update each nested checkboxGroupInput to empty selection
      updateCheckboxGroupInput(
        session,
        "age_group-ages",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "pop_type-pop_types",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "virus-viruses",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "study_design-designs",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "domain-domains",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "rob-rob_levels",
        selected = character(0)
      )
      updateCheckboxGroupInput(
        session,
        "type_of_outcome-types",
        selected = character(0)
      )
      # Reset study period slider via its own reset function
      if (!is.null(reset_study_period)) {
        reset_study_period()
      }

      # Show "Cleared!" message
      output$cleared_msg <- renderText("Cleared!")

      # Remove the message after 3 seconds
      shinyjs::delay(3000, {
        output$cleared_msg <- renderText("")
      })
    })
  })
}

typeOfOutcomeFilterServer <- function(
  id,
  outcome_data,
  id_col = "row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    output$type_of_outcome_message <- renderText({
      selected <- input$types
      if (is.null(selected) || length(selected) == 0) {
        return("Showing all outcome types")
      }
      if (length(selected) == 1) {
        paste("Showing", selected, "outcomes only")
      } else if (length(selected) == 2) {
        paste("Showing", selected[1], "or", selected[2], "outcomes")
      } else {
        paste(
          "Showing",
          paste(selected[-length(selected)], collapse = ", "),
          "or",
          selected[length(selected)],
          "outcomes"
        )
      }
    })

    reactive({
      selected <- input$types
      if (is.null(selected) || length(selected) == 0) {
        return(all_ids %||% unique(outcome_data[[id_col]]))
      }
      outcome_data %>%
        filter(`Type of Outcome` %in% selected) %>%
        pull(!!sym(id_col)) %>%
        unique()
    })
  })
}

# ---- Advanced filter server (currently only population tri‑state) ----
advancedFilterServer <- function(
  id,
  pop_long,
  id_col = "char_row_id",
  all_ids = NULL
) {
  moduleServer(id, function(input, output, session) {
    # Module-level code vectors
    age_codes <- c("OA", "A", "C", "I")
    age_full <- c(
      "OA" = "Older Adults (≥ 65 years)",
      "A" = "Adults (19 – 64 years)",
      "C" = "Children (1 – 18 years)",
      "I" = "Infants (< 1 year)"
    )
    type_codes <- c("IC", "HR", "P", "H")
    type_full <- pop_code_to_full[type_codes]
    all_codes <- c(age_codes, type_codes)

    # ---- UI: Age Group advanced ----
    output$age_advanced <- renderUI({
      lapply(seq_along(age_codes), function(i) {
        radioButtons(
          inputId = session$ns(paste0("pop_", age_codes[i])),
          label = age_full[i],
          choices = c(
            "Ignore" = "ignore",
            "Include" = "include",
            "Exclude" = "exclude"
          ),
          selected = "ignore",
          inline = TRUE
        )
      })
    })

    output$age_adv_message <- renderText({
      states <- vapply(
        age_codes,
        function(code) {
          val <- input[[paste0("pop_", code)]]
          if (is.null(val)) "ignore" else val
        },
        character(1)
      )

      if (all(states == "ignore")) {
        return("All groups set to Ignore – showing all ages (no filtering)")
      }

      inc <- pop_code_to_full[age_codes][states == "include"]
      exc <- pop_code_to_full[age_codes][states == "exclude"]
      msg <- ""
      if (length(inc) > 0) {
        msg <- paste0("Include: ", paste(inc, collapse = ", "))
      }
      if (length(exc) > 0) {
        if (nchar(msg) > 0) {
          msg <- paste0(msg, " AND ")
        }
        msg <- paste0(msg, "Exclude: ", paste(exc, collapse = ", "))
      }
      msg
    })

    # ---- UI: Population Type advanced ----
    output$pop_type_advanced <- renderUI({
      lapply(seq_along(type_codes), function(i) {
        radioButtons(
          inputId = session$ns(paste0("pop_", type_codes[i])),
          label = type_full[i],
          choices = c(
            "Ignore" = "ignore",
            "Include" = "include",
            "Exclude" = "exclude"
          ),
          selected = "ignore",
          inline = TRUE
        )
      })
    })

    output$pop_type_adv_message <- renderText({
      states <- vapply(
        type_codes,
        function(code) {
          val <- input[[paste0("pop_", code)]]
          if (is.null(val)) "ignore" else val
        },
        character(1)
      )
      if (all(states == "ignore")) {
        return(
          "All groups set to Ignore - showing all population types (no filtering)"
        )
      }
      inc <- type_full[states == "include"]
      exc <- type_full[states == "exclude"]
      msg <- ""
      if (length(inc) > 0) {
        msg <- paste0("Include: ", paste(inc, collapse = ", "))
      }
      if (length(exc) > 0) {
        if (nchar(msg) > 0) {
          msg <- paste0(msg, "  AND  ")
        }
        msg <- paste0(msg, "Exclude: ", paste(exc, collapse = ", "))
      }
      msg
    })

    # ---- Clear all advanced filters ----
    observeEvent(input$clear_advanced, {
      for (code in all_codes) {
        updateRadioButtons(
          session = session,
          inputId = paste0("pop_", code),
          selected = "ignore"
        )
      }
      output$cleared_msg <- renderText("Cleared!")
      shinyjs::delay(3000, {
        output$cleared_msg <- renderText("")
      })
    })

    # ---- Reactive filter logic ----
    reactive({
      include_codes <- all_codes[vapply(
        all_codes,
        function(code) {
          identical(input[[paste0("pop_", code)]], "include")
        },
        logical(1)
      )]
      exclude_codes <- all_codes[vapply(
        all_codes,
        function(code) {
          identical(input[[paste0("pop_", code)]], "exclude")
        },
        logical(1)
      )]

      if (length(include_codes) == 0 && length(exclude_codes) == 0) {
        return(all_ids %||% unique(pop_long[[id_col]]))
      }

      inc_ids <- if (length(include_codes) > 0) {
        pop_long %>%
          filter(`Population Code` %in% include_codes) %>%
          pull(!!sym(id_col)) %>%
          unique()
      } else {
        unique(pop_long[[id_col]])
      }

      exc_ids <- if (length(exclude_codes) > 0) {
        pop_long %>%
          filter(`Population Code` %in% exclude_codes) %>%
          pull(!!sym(id_col)) %>%
          unique()
      } else {
        integer(0)
      }

      setdiff(inc_ids, exc_ids)
    })
  })
}
