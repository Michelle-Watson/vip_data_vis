studyPeriodFilterServer <- function(id, char_data) {
  moduleServer(id, function(input, output, session) {
    # Year range from the full dataset (static)
    years <- as.integer(char_data$`Study Period Start`)
    years <- years[!is.na(years)]
    min_yr <- min(years)
    max_yr <- max(years)

    # Render slider
    output$slider_ui <- renderUI({
      sliderInput(
        session$ns("year_range"),
        label = NULL,
        min = min_yr,
        max = max_yr,
        value = c(min_yr, max_yr),
        step = 1,
        sep = ""
      )
    })

    # Mini histogram (full data)
    output$period_hist <- renderPlot(
      {
        library(ggplot2)

        df <- data.frame(
          year = names(table(years)),
          count = as.numeric(table(years))
        )

        ggplot(df, aes(x = year, y = count)) +
          geom_col(fill = "#007bc2", width = 0.55) + # compressed bars
          theme_void() + # removes background, axes, gridlines
          theme(
            plot.background = element_rect(fill = "transparent", color = NA),
            panel.background = element_rect(fill = "transparent", color = NA),
            # panel.grid.major.y = element_line(color = "grey90", size = 0.3),  # optional subtle gridlines
            plot.margin = margin(0, 0, 0, 0)
          )
      },
      height = 80,
      bg = "transparent"
    )

    # Dynamic message
    output$period_message <- renderText({
      sel <- input$year_range
      if (is.null(sel) || (sel[1] == min_yr && sel[2] == max_yr)) {
        return("Showing all study periods")
      }
      paste("Showing studies with start year", sel[1], "to", sel[2])
    })

    # Filter logic
    reactive({
      sel <- input$year_range
      if (is.null(sel) || (sel[1] == min_yr && sel[2] == max_yr)) {
        return(unique(char_data$char_row_id))
      }
      char_data %>%
        filter(
          `Study Period Start` >= sel[1] & `Study Period Start` <= sel[2]
        ) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

# ---- Age Group filter server ----
ageGroupFilterServer <- function(id, pop_long) {
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

    # Filter logic (unchanged)
    reactive({
      selected <- input$ages
      if (is.null(selected) || length(selected) == 0) {
        return(unique(pop_long$char_row_id))
      }
      pop_long %>%
        filter(`Population Code` %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

# ---- Population Type filter server ----
popTypeFilterServer <- function(id, pop_long) {
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

    # Filter logic (unchanged)
    reactive({
      selected <- input$pop_types
      if (is.null(selected) || length(selected) == 0) {
        return(unique(pop_long$char_row_id))
      }
      pop_long %>%
        filter(`Population Code` %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

virusFilterServer <- function(id, virus_long) {
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
        return(unique(virus_long$char_row_id))
      }
      virus_long %>%
        filter(Virus %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

studyDesignFilterServer <- function(id, char_data) {
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
        return(unique(char_data$char_row_id))
      }
      char_data %>%
        filter(`Study Design` %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

robFilterServer <- function(id, rob_long) {
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
        return(unique(rob_long$char_row_id))
      }
      # Convert selected simplified categories to original Overall Risk values
      original_risks <- unlist(category_map[selected])
      rob_long %>%
        filter(`Overall Risk` %in% original_risks) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

domainFilterServer <- function(id, outcomes_long) {
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
        return(unique(outcomes_long$char_row_id))
      }
      outcomes_long %>%
        filter(Domain %in% selected) %>%
        pull(char_row_id) %>%
        unique()
    })
  })
}

simpleFiltersServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Observe the clear button click (id is "clear_simple" inside this module)
    observeEvent(input$clear_simple, {
      # Update each nested checkboxGroupInput to empty selection
      updateSliderInput(
        session,
        "study_period-year_range",
        value = c(min_study_year, max_study_year)
      )
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

      # Show "Cleared!" message
      output$cleared_msg <- renderText("Cleared!")

      # Remove the message after 3 seconds
      shinyjs::delay(3000, {
        output$cleared_msg <- renderText("")
      })
    })
  })
}


# ---- Advanced filter server (currently only population tri‑state) ----
advancedFilterServer <- function(id, pop_long) {
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
      # Use vapply to safely get states (default "ignore" if NULL)
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
        return(unique(pop_long$char_row_id))
      }

      inc_ids <- if (length(include_codes) > 0) {
        pop_long %>%
          filter(`Population Code` %in% include_codes) %>%
          pull(char_row_id) %>%
          unique()
      } else {
        unique(pop_long$char_row_id)
      }

      exc_ids <- if (length(exclude_codes) > 0) {
        pop_long %>%
          filter(`Population Code` %in% exclude_codes) %>%
          pull(char_row_id) %>%
          unique()
      } else {
        integer(0)
      }

      setdiff(inc_ids, exc_ids)
    })
  })
}
