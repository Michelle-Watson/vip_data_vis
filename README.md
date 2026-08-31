# VIP Data Visualisation Shiny App

A comprehensive R Shiny application for exploring systematically reviewed evidence on COVID‑19, influenza, and RSV vaccine effectiveness and safety.

## Table of Contents

- [VIP Data Visualisation Shiny App](#vip-data-visualisation-shiny-app)
  - [Table of Contents](#table-of-contents)
  - [Key Features](#key-features)
  - [Repository Structure](#repository-structure)
  - [Data Sources](#data-sources)
  - [Local Installation and Running](#local-installation-and-running)
    - [Prerequisites](#prerequisites)
    - [Clone the repository](#clone-the-repository)
    - [Run the app in RStudio](#run-the-app-in-rstudio)
  - [Required R Packages](#required-r-packages)
  - [Using the App](#using-the-app)
    - [Studies Tab](#studies-tab)
    - [Outcomes Tab](#outcomes-tab)
    - [Lightweight View](#lightweight-view)
    - [Download Buttons](#download-buttons)
  - [Deployment](#deployment)
    - [ShinyApps.io](#shinyappsio)
  - [Embedding on a Website](#embedding-on-a-website)
  - [Configuration](#configuration)
    - [Population mapping](#population-mapping)
    - [Table column order and hiding](#table-column-order-and-hiding)
  - [Filtering Logic](#filtering-logic)
  - [Downloading Data](#downloading-data)
    - [Main Downloads](#main-downloads)
    - [Filtered Downloads](#filtered-downloads)
  - [Customising the App](#customising-the-app)
    - [Changing filters](#changing-filters)
    - [Changing table columns](#changing-table-columns)
    - [Changing styles](#changing-styles)
    - [Updating logos](#updating-logos)
  - [Acknowledgements](#acknowledgements)

The Vaccine Integrity Project (VIP) interactive data tool summarises newly published research evaluating the effectiveness and safety of vaccines and immunisation products for:

- COVID‑19
- Influenza
- Respiratory syncytial virus (RSV)

The tool is built in R Shiny and provides two main interactive tabs:

- **Studies** - a list of included studies with reference information, study characteristics, and risk‑of‑bias assessments.
- **Outcomes** - vaccine effectiveness/efficacy and safety estimates extracted from the included studies.

The underlying data come from systematic reviews of peer‑reviewed research conducted as part of the VIP project.

## Key Features

- Interactive filtering by:
  - Virus
  - Population
  - Age group
  - Study period
  - Study design
  - Outcome domain
  - Risk of bias
  - Vaccine and strain
- Responsive layout with collapsible sidebar filters.
- Freeze header row and first column in data tables.
- Responsive/mobile‑friendly table behaviour.
- Lightweight / full‑view toggles.
- Download full source Excel workbooks.
- Download filtered views as Excel files.
- About page with project information and collapsible definitions.
- Dynamic row counts and filter helper messages.
- Support for embedded deployment via iframe.

## Repository Structure

```
├── app.R # Main Shiny application
├── shiny_aux/
│ ├── config.R # Study configuration (columns, filters, population map)
│ ├── outcome_config.R # Outcome configuration
│ ├── filter_ui.R # Filter UI components
│ ├── filter_server.R # Filter server logic
│ ├── helpers.R # Helper functions
│ └── styles.R # Custom CSS
├── characteristics_tables/
│ └── All_Study_Characteristics.xlsx
├── outcome_tables/
│ └── All_Tables_split.xlsx
├── www/
│ ├── VIP_Logo_Horizontal.png
│ └── VIP_Logo_Vertical.png
└── README.md
```

## Data Sources

The Shiny app reads from two primary Excel workbooks:

- `characteristics_tables/All_Study_Characteristics.xlsx`
  - Sheet **Study Characteristics**
  - Additional sheets: `Footnotes`, `Outcomes_Long`, `N_Long`, `Population_Long`, `Virus_Long`, `RoB_Long`
- `outcome_tables/All_Tables_split.xlsx`
  - Sheet **All**
  - Additional sheets: `Footnotes`, `Population_Long`, `Virus_Long`

These workbooks are generated from the consensus data‑extraction file using data processing scripts (available upon request).

## Local Installation and Running

### Prerequisites

- R (>= 4.0 recommended)
- RStudio (optional but recommended)
- Internet access for package installation

### Clone the repository

```bash
git clone https://github.com/your‑org/vip-data-vis.git
cd vip-data-vis
```

### Run the app in RStudio

Open `app.R.`
Click **Run App**.

## Required R Packages

The app depends on:

```r
library(shiny)
library(readxl)
library(DT)
library(shinyjs)
library(dplyr)
library(writexl)
library(ggplot2)
library(rlang)
library(bslib)
```

Install with:

```r
install.packages(c(
  "shiny", "readxl", "DT", "shinyjs",
  "dplyr", "writexl", "ggplot2",
  "rlang", "bslib"
))
```

## Using the App

### Studies Tab

Displays one row per included study with columns such as:

- Study
- Virus
- Population
- Age range
- Study period
- Total N
- Study design
- Risk of bias
- Funding source
- Journal / PMID / PMCID / DOI / Link

**Filters available:**

- Virus
- Type of Outcome
- Age Group
- Population Type
- Study Period
- Study Design
- Risk of Bias
- Advanced population filters

### Outcomes Tab

Displays extracted outcome estimates with columns including:

- Study label
- Population
- Age range
- Outcome
- Estimate
- Estimate type
- Risk of bias
- Vaccine formulations and strains
- Sample sizes

**Filters available:**

- Virus
- Type of Outcome
- Age Group
- Population Type
- Study Period
- Study Design
- Type of Estimate
- Risk of Bias

### Lightweight View

Both tabs include a lightweight view toggle that hides less critical columns to reduce horizontal scrolling.

### Download Buttons

- **Download full Excel** - exports the original source workbook.
- **Download filtered view** - exports only the currently visible rows, including a Footnotes sheet.

## Deployment

### ShinyApps.io

1. Create an account at [shinyapps.io](https://www.shinyapps.io/).
2. Install `rsconnect`:
   ```r
   install.packages("rsconnect")
   ```
3. Authenticate:
   ```r
   rsconnect::setAccountInfo(
    name = "your‑account",
    token = "your‑token",
    secret = "your‑secret"
    )
   ```
4. Deploy:
   ```r
   rsconnect::deployApp(appDir = "path/to/app")
   ```

## Embedding on a Website

Use an iframe:

```html
<iframe
  src="https://your-account.shinyapps.io/vip_data_vis/"
  width="100%"
  height="700"
  style="border:none;"
  allowfullscreen
>
</iframe>
```

## Configuration

### Population mapping

`shiny_aux/config.R` contains:

```r
pop_code_to_full <- c(
  "OA" = "Older Adults",
  "A"  = "Adults",
  "I"  = "Infants",
  "C"  = "Children",
  "IC" = "Immunocompromised",
  "HR" = "Other co-occurring conditions",
  "P"  = "Pregnant",
  "H"  = "Healthcare personnel"
)
```

### Table column order and hiding

`shiny_aux/outcome_config.R` and `config.R` define:

- `always_hide` - columns never displayed in the main table.
- `article_cols` - columns hidden in lightweight view.
- `column_order` - display order.
- `desired_widths` - column width preferences.

## Filtering Logic

Each filter uses long‑format helper tables (`Population_Long`, `Virus_Long`, etc.) to map user selections to a set of row IDs. The final data set is the intersection of all active filter ID sets.

When no value is selected in a filter, the filter returns all IDs, so no rows are accidentally dropped.

## Downloading Data

### Main Downloads

- **Download Study Characteristics** - full source workbook, cleaned but unmodified columns.
- **Download Outcomes** - all outcome rows in the source workbook.

### Filtered Downloads

Both tabs include **Download filtered view**, which exports only the currently visible rows.

Selected columns can be removed at export time via `drop_columns`.

## Customising the App

### Changing filters

Edit `shiny_aux/filter_ui.R` and `shiny_aux/filter_server.R` to add or modify filters.

### Changing table columns

Update the relevant configuration files:

- `config.R` for Studies.
- `outcome_config.R` for Outcomes.

### Changing styles

Edit `shiny_aux/styles.R`.

### Updating logos

Replace the files in the `www/` folder with your own images.

## Acknowledgements

This work builds on the Vaccine Integrity Project (VIP) led by CIDRAP.  
We thank all reviewers, extractors, and contributors who made this systematic review possible.
