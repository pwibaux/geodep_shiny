## =============================================================================
## GEODEP - SHINY APP
## =============================================================================

library(here)
library(dplyr)
library(tidyr)
library(ggplot2)
library(shiny)
library(leaflet)
library(htmltools)
library(DT)
library(sf)
library(readr)
library(zip)
library(ggtext)
library(treemapify)
library(shinycssloaders)
here::i_am('geodep_shiny.Rproj')

## -----------------------------------------------------------------------
## 0. Load precomputed inputs
## -----------------------------------------------------------------------

geodep_inputs <- readRDS("1_Data/geodep_shiny_inputs.rds")

DEP_YEAR                  <- geodep_inputs$DEP_YEAR
eu_countries               <- geodep_inputs$eu_countries
sector_names               <- geodep_inputs$sector_names
sector_choices               <- geodep_inputs$sector_choices
world_polygons                  <- geodep_inputs$world_polygons
iso_name_lookup                   <- geodep_inputs$iso_name_lookup
country_choices                     <- geodep_inputs$country_choices
country_choices_ui                    <- geodep_inputs$country_choices_ui
dep_import_base                         <- geodep_inputs$dep_import_base
dep_export_base                           <- geodep_inputs$dep_export_base
traded_by_country                           <- geodep_inputs$traded_by_country
traded_by_country_export                      <- geodep_inputs$traded_by_country_export
imports_sector_long_all                         <- geodep_inputs$imports_sector_long_all
imports_final <- geodep_inputs$imports_final
exports_final <- geodep_inputs$exports_final

rm(geodep_inputs); gc()

sector_choices_ui <- list(
  "All Sectors" = "all",
  "Strategic Sector" = c(
    "All Strategic Sectors"        = "sect_strategic",
    "\u2003Critical Raw Materials" = "sect_crm",
    "\u2003Dual Use"               = "sect_dual_use",
    "\u2003Health"                 = "sect_health",
    "\u2003Agrifood"               = "sect_agrifood",
    "\u2003Energy"                 = "sect_energy"
  )
)

COL_ZERO   <- "#a8a8a8"  
COL_NODATA <- "transparent" 

## -----------------------------------------------------------------------
## 1. Small helpers
## -----------------------------------------------------------------------
to_eun <- function(iso3) {
  if_else(iso3 %in% eu_countries, "EUN", iso3)
}

iso_display_name <- function(iso3) {
  case_match(
    iso3,
    "EUN" ~ "European Union",
    .default = iso3
  )
}

iso_name <- function(iso3) {
  matched <- iso_name_lookup$name[match(iso3, iso_name_lookup$iso_a3)]
  ifelse(is.na(matched), iso3, matched)
}

## -----------------------------------------------------------------------
## Helper: build README text for the zip download
## -----------------------------------------------------------------------
format_kb <- function(bytes) {
  format(round(bytes / 1000), big.mark = " ", scientific = FALSE)
}

generate_readme <- function(direction, selection, sector_filter, map_metric,
                            zip_filename, size_kb_unzipped, size_kb_zipped = NULL,
                            is_full = FALSE) {
  
  is_import <- direction == "import"
  
  unzipped_line <- paste0(size_kb_unzipped, " Kilobytes")
  zipped_line   <- if (is.null(size_kb_zipped)) "" else paste0(size_kb_zipped, " Kilobytes")
  
  if (is_full) {
    selection_line <- paste0(
      "Selection used for this extract : FULL DATASET \u2014 no country or sector filter applied",
      " | Direction = ", if (is_import) "Imports" else "Exports",
      " | Extraction date = ", Sys.Date()
    )
  } else {
    selection_desc <- if (length(selection) == 2) {
      paste0("Importer = ", iso_display_name(selection[1]), " (", selection[1], "), ",
             "Exporter = ", iso_display_name(selection[2]), " (", selection[2], ")")
    } else if (length(selection) == 1) {
      paste0(if (is_import) "Importer" else "Exporter", " = ",
             iso_display_name(selection[1]), " (", selection[1], ")")
    } else {
      "No country selected (full table for current filters)"
    }
    
    sector_label <- if (sector_filter == "all") "All Sectors" else sector_filter
    metric_label <- if (map_metric == "count") "Share of number of products" else "Share of trade value"
    
    selection_line <- paste0(
      "Selection used for this extract : Direction = ", if (is_import) "Imports" else "Exports",
      " | ", selection_desc,
      " | Sector filter = ", sector_label,
      " | Map metric = ", metric_label,
      " | Extraction date = ", Sys.Date()
    )
  }
  
  if (is_import) {
    header <- paste0(
      "Name of the dataset : ", zip_filename, "\n",
      "Format : csv\n",
      "Delimiter : ,\n\n",
      "Release Date : \n\n",
      "Weblink : https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41\n\n",
      "DOI : \n\n",
      "Size (unzipped) : ", unzipped_line, "\n\n",
      "Size (zipped) : ", zipped_line, "\n\n",
      "Software : this dataset was created using Stata 16\n\n",
      "Contents: GeoDep provides an assessment of trade dependencies for the year 2024 at the HS6 product level (revision 2022). GeoDep_M assesses import vulnerabilities by measuring concentration, substitutability, and strategic sector categorization (e.g., health, energy, agrifood, dual-use).\n\n",
      "List of Variables (GeoDep_M - Import Dependencies) :\n",
      "iso_d -- Importer (ISO 3-digit country code)\n",
      "hs6 -- Product category (HS6 product rev. 2022)\n",
      "year -- Year 2024\n",
      "import_dpt -- Total import value (in thousands current USD) of HS6 product p, to destination d, in year t\n",
      "c1_M -- Level of concentration of imports\n",
      "c2_M -- Level of concentration of world exports\n",
      "c3_M -- Substitutability of exports by domestic supply\n",
      "c4_M -- Criteria 4: c1 > 0.4 & c2 > 0.4 & c3 > 1 = 2/3 previous years\n",
      "first_odpt -- ISO code of the leading exporter in total imports of the HS6 product p, to destination d, in year t\n",
      "share_odpt -- Share of the leading exporter in total imports of the HS6 product p, to destination d, in year t\n",
      "dependent -- =1 if [c1 > 0.4 & c2 > 0.4 & c3 > 1 & c4 = 1], 0 otherwise\n",
      "sect_crm -- =1 if the HS6 product belongs to CRM UNCTAD list, 0 otherwise\n",
      "sect_dual_use -- =1 if the HS6 product belongs to Dual use EU list, 0 otherwise\n",
      "sect_health -- =1 if the HS6 product belongs to Health nomenclature list, 0 otherwise\n",
      "sect_energy -- =1 if the HS6 product belongs to Energy ECT list, 0 otherwise\n",
      "sect_agrifood -- =1 if the HS6 product belongs to Agrifood WB or Fertilisants FAO list, 0 otherwise\n",
      "sect_other -- =1 if the HS6 product is not strategic\n\n",
      "Additional useful information : Not applicable\n\n",
      "Example of 1 line : Not applicable\n\n",
      "Licence : Creative Commons BY NC SA\n\n",
      "Reference (Please cite when using this dataset) : \n\n",
      selection_line, "\n"
    )
  } else {
    header <- paste0(
      "Name of the dataset : ", zip_filename, "\n",
      "Format : csv\n",
      "Delimiter : ,\n\n",
      "Release Date : \n\n",
      "Weblink : https://www.cepii.fr/CEPII/fr/bdd_modele/bdd_modele_item.asp?id=41\n\n",
      "DOI : \n\n",
      "Size (unzipped) : ", unzipped_line, "\n\n",
      "Size (zipped) : ", zipped_line, "\n\n",
      "Software : this dataset was created using Stata 16\n\n",
      "Contents: GeoDep provides an assessment of trade dependencies for the year 2024 at the HS6 product level (revision 2022). GeoDep_X assesses export dependencies by measuring market concentration and substitutability by domestic demand.\n\n",
      "List of Variables (GeoDep_X - Export Dependencies) :\n",
      "iso_o -- Exporter (ISO 3-digit country code)\n",
      "hs6 -- Product category (HS6 product rev. 2022)\n",
      "year -- Year 2024\n",
      "export_opt -- Total export value (in thousands current USD) of HS6 product p, from origin o, in year t\n",
      "c1_X -- Level of concentration of exports\n",
      "c2_X -- Level of concentration of world imports\n",
      "c3_X -- Substitutability of exports by domestic demand\n",
      "c4_X -- Criteria 4: c1_X > 0.4 & c2_X > 0.4 & c3_X > 1 = 2/3 previous years\n",
      "first_dpto -- ISO code of the first destination in total exports of the HS6 product p, from origin o, in year t\n",
      "share_dpto -- Share of the first destination in total exports of the HS6 product p, from origin o, in year t\n",
      "dependent -- =1 if [c1_X > 0.4 & c2_X > 0.4 & c3_X > 1 & c4_X = 1], 0 otherwise\n\n",
      "Additional useful information : Not applicable\n\n",
      "Example of 1 line : Not applicable\n\n",
      "Licence : Creative Commons BY NC SA\n\n",
      "Reference (Please cite when using this dataset) : \n\n",
      selection_line, "\n"
    )
  }
  
  header
}

reference_files <- c("product_codes_HS22_V202601.csv", "country_codes_V202601.csv")

copy_reference_files <- function(tmp_dir, data_dir = "1_Data") {
  src_paths <- file.path(data_dir, reference_files)
  present   <- file.exists(src_paths)
  
  if (any(!present)) {
    warning("Missing reference file(s): ", paste(reference_files[!present], collapse = ", "))
  }
  
  Map(function(src, ok) {
    if (ok) file.copy(src, file.path(tmp_dir, basename(src)), overwrite = TRUE)
  }, src_paths, present)
  
  reference_files[present]
}

## -----------------------------------------------------------------------
## 2. UI
## -----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #eef3f1;
        font-family: 'Helvetica Neue', Arial, sans-serif;
        color: #2b2b2b;
        padding-bottom: 40px;
      }

      .container-fluid {
        max-width: 1400px;
        padding-top: 10px;
        padding-left: 30px;
        padding-right: 30px;
      }

      .title-banner {
        background-color: #1f6f5c;
        color: #ffffff;
        padding: 28px 34px;
        margin: -10px -30px 30px -30px;
        border-bottom: 4px solid #8b3a3a;
        display: grid;
        grid-template-columns: auto 1fr auto;
        align-items: center;
        column-gap: 20px;
      }
     
      .title-banner h1 {
        margin: 0;
        font-size: 30px;
        font-weight: 700;
        letter-spacing: 0.5px;
        text-align: center;
        grid-column: 2;
      }
     
      .banner-logo {
        height: 80px;
        width: auto;    
        display: block;    
        grid-column: 1;
        justify-self: start;
      }
     
      .banner-logo-spacer {
        height: 80px;
        width: auto;
        display: block;
        grid-column: 3;
        justify-self: end;
        visibility: hidden;  
      }

      .intro-text {
        background-color: #dce8e4;
        border-left: 4px solid #1f6f5c;
        padding: 18px 22px;
        border-radius: 4px;
        margin-bottom: 30px;
        color: #2b2b2b;
        line-height: 1.6;
      }

      h3, h4 {
        color: #1f6f5c;
      }
      h3 {
        border-bottom: 2px solid #dce8e4;
        padding-bottom: 10px;
        margin-top: 40px;
        margin-bottom: 20px;
      }

      h3.table-heading {
        border-bottom: none;
        padding-bottom: 0;
        margin-top: 30px;
        margin-bottom: 12px;
        font-size: 18px;
        font-weight: 600;
      }
      label {
        color: #1f6f5c;
        font-weight: 600;
        margin-bottom: 8px;
      }

      .section-block {
        margin-bottom: 36px;
      }

      .btn {
        background-color: #1f6f5c;
        color: #ffffff;
        border: none;
        border-radius: 3px;
        font-weight: 600;
      }
      .btn:hover, .btn:focus {
        background-color: #164f42;
        color: #ffffff;
      }
      #reset_button {
        background-color: #6c7a76;
      }
      #reset_button:hover {
        background-color: #56635f;
      }
      #info_button {
        background-color: #8b3a3a;
      }
      #info_button:hover {
        background-color: #6e2c2c;
      }
      #download_table {
        background-color: #8b3a3a;
        width: 100%;
        display: block;
        text-align: center;
      }
      #download_table:hover {
        background-color: #6e2c2c;
      }
      #swap_button {
        padding-left: 14px;
        padding-right: 14px;
      }
      
      #shiny-notification-panel .shiny-notification {
        background-color: #ffffff;
        border: 1px solid #cfe0da;
        border-left: 5px solid #1f6f5c;
        color: #2b2b2b;
        border-radius: 4px;
      }
      
      #shiny-notification-panel .progress {
        background-color: #dce8e4;   
        height: 10px;
        border-radius: 3px;
      }
      
      #shiny-notification-panel .progress-bar {
        background-color: #1f6f5c;   
      }
      
      #shiny-notification-panel .shiny-notification-content-text {
        color: #2b2b2b;
      }

      .form-group {
        margin-bottom: 20px;
      }
      .form-control, .selectize-input {
        border: 1px solid #b7cdc6;
        border-radius: 3px;
        padding: 8px 10px;
      }
      .form-control:focus, .selectize-input.focus {
        border-color: #1f6f5c;
        box-shadow: 0 0 0 2px rgba(31, 111, 92, 0.2);
      }

      select#sector_filter optgroup {
        font-weight: 700;
        font-style: normal;
      color: #1f6f5c;
      }

      select#sector_filter option {
        font-weight: normal;
        color: #2b2b2b;
      }
      .radio-inline, .radio label {
        color: #2b2b2b;
        font-weight: normal;
      }
      .radio {
        margin-bottom: 6px;
      }

      .well {
        background-color: #f4f8f6;
        border: 1px solid #cfe0da;
        border-left: 5px solid #8b3a3a;
        border-radius: 4px;
        padding: 20px 24px;
        margin-bottom: 20px;
      }

      #dependency_map {
        border: 1px solid #b7cdc6;
        border-radius: 4px;
        margin-bottom: 30px;
        box-sizing: border-box;
      }

      .leaflet-control.info.legend {
        font-size: 11px;
        padding: 6px 10px;
        line-height: 1.3;
        max-width: 300px;
      }
      .leaflet-control.info.legend strong {
        font-size: 11px;
      }
      .horizontal-legend {
        width: 290px;
      }

      hr {
        border-top: 1px solid #cfe0da;
        margin: 34px 0;
      }

      table.dataTable thead th {
        background-color: #1f6f5c;
        color: #ffffff;
        padding: 10px 12px;
      }
      table.dataTable tbody td {
        padding: 8px 12px;
      }
      .dataTables_wrapper {
        margin-top: 10px;
        overflow-x: auto;
        width: 100%;
      }

      .app-layout {
        display: flex;
        gap: 24px;
        align-items: flex-start;
      }
      .sidebar-panel {
        flex: 0 0 300px;
        background-color: #ffffff;
        border: 1px solid #cfe0da;
        border-radius: 6px;
        padding: 22px;
        box-sizing: border-box;
        min-height: 650px;
        display: flex;
        flex-direction: column;
        position: sticky;
        top: 20px;
        max-height: calc(100vh - 40px);
        overflow-y: auto;
      }
     
      .main-panel {
        flex: 1;
        min-width: 0;
      }
      .sidebar-section {
        margin-bottom: 22px;
      }
      .sidebar-section:last-child {
        margin-bottom: 0;
      }
      .swap-wrap {
        display: flex;
        justify-content: center;
        margin: 2px 0 18px 0;
      }
      .sidebar-actions {
        display: flex;
        gap: 8px;
      }
      .sidebar-actions .btn {
        flex: 1;
      }
      .irs-bar {
        background-color: #1f6f5c !important;
        border-top: 1px solid #1f6f5c !important;
        border-bottom: 1px solid #1f6f5c !important;
      }
      .irs-bar-edge {
        background-color: #1f6f5c !important;
        border: 1px solid #1f6f5c !important;
      }
      .irs-single, .irs-to, .irs-from {
        background-color: #1f6f5c !important;
      }
      .irs-single:after, .irs-to:after, .irs-from:after {
        border-top-color: #1f6f5c !important;
      }

      @media (max-width: 768px) {
        .app-layout {
          flex-direction: column;
        }
        .sidebar-panel {
          flex: none;
          width: 100%;
          min-height: 0;
          margin-bottom: 20px;
          position: static;
          top: auto;
          max-height: none;
        }
        .title-banner {
          grid-template-columns: 1fr;
          justify-items: center;
          row-gap: 12px;
          text-align: center;
        }
        .banner-logo {
          height: 60px;
        }
        .banner-logo-spacer {
          display: none;    
        }
        .title-banner h1 {
          font-size: 22px;
        }
        .horizontal-legend {
          width: 230px;
        }
      }
    "))
  ),
  
  div(class = "title-banner",
      tags$img(src = "IFE2-Logo-B.png", class = "banner-logo"),
      h1("GeoDep — Trade Dependencies"),
      tags$img(src = "IFE2-Logo-B.png", class = "banner-logo-spacer", `aria-hidden` = "true")
  ),
  
  div(class = "intro-text",
      uiOutput("intro_text")
  ),
  
  div(class = "app-layout",
      div(class = "sidebar-panel",
          div(class = "sidebar-section",
              radioButtons("dep_direction", "Dimension of dependencies:",
                           choices = c("Imports" = "import", "Exports" = "export"),
                           selected = "import")
          ),
          div(class = "sidebar-section",
              selectInput("sector_filter", "Sector:",
                          choices = sector_choices_ui, selected = "all", width = "100%")
          ),
          div(class = "sidebar-section",
              radioButtons("map_metric", "Map shows:",
                           choices = c("Share of number of products" = "count",
                                       "Share of trade value" = "value"),
                           selected = "count")
          ),
          div(class = "sidebar-section",
              selectizeInput("importer_select", "Importer (Destination):",
                             choices = country_choices_ui, selected = "",
                             options = list(placeholder = "Type a country name..."),
                             width = "100%"),
              div(class = "swap-wrap",
                  actionButton("swap_button", "\u21c5 Swap")
              ),
              selectizeInput("exporter_select", "Exporter (Origin):",
                             choices = country_choices_ui, selected = "",
                             options = list(placeholder = "Type a country name..."),
                             width = "100%")
          ),
          div(class = "sidebar-section",
              conditionalPanel(
                condition = "(input.importer_select !== '' && input.exporter_select === '') || (input.importer_select === '' && input.exporter_select !== '')",
                sliderInput("top_n_slider", "Top Partners in Treemap:",
                            min = 3, max = 10, value = 5, step = 1, width = "100%")
              )
          ),
          div(class = "sidebar-section sidebar-actions",
              actionButton("reset_button", "Reset selection"),
              actionButton("info_button", "\u2139 Methodology")
          ),
          div(class = "sidebar-section",
              style = "margin-top: auto;",
              conditionalPanel(
                condition = "input.importer_select !== ''",
                downloadButton("download_table", "Download selection (ZIP)")
              ),
              tags$div(style = "height: 10px;"),
              downloadButton("download_full", "Download full dataset (ZIP)",
                             style = "background-color: #1f6f5c; color: white; border: none; width: 100%; display: block; text-align: center;")
          )
      ),
      
      div(class = "main-panel",
          withSpinner(
            leafletOutput("dependency_map", height = "650px"),
            type = 8, color = "#1f6f5c", size = 1.5
          ),
          
          div(class = "section-block",
              uiOutput("partners_panel"),
              hr(),
              uiOutput("table_heading"),
              withSpinner(
                DTOutput("dependency_table"),
                type = 8, color = "#1f6f5c", size = 1
              )
          )
      )
  )
)

## -----------------------------------------------------------------------
## 3. Server
## -----------------------------------------------------------------------

server <- function(input, output, session) {
  selected_countries <- reactiveVal(character())
  suppress_input_sync <- reactiveVal(0L)  
  
  
  observeEvent(input$info_button, {
    showModal(modalDialog(
      title = "Methodology",
      size = "l",
      p("This app is built on the GeoDep database (CEPII), using the methodology described in ",
        tags$a(href = "https://www.cepii.fr/CEPII/fr/publications/pb/abstract.asp?NoDoc=14223",
               target = "_blank",
               "Lefebvre & Wibaux (2024), \u201cImport Dependencies: Where Does the EU Stand?\u201d, CEPII Policy Brief n\u00b02024-47"),
        "."),
      p("A product (HS 6-digit) is classified as ", tags$strong("import-dependent"),
        " for a country only if it meets all four of the following criteria at once:"),
      tags$ol(
        tags$li(tags$strong("Import concentration : "),
                " a Herfindahl-Hirschman Index (HHI) computed on the country's import shares by origin exceeds 0.4, meaning its supply of the product is concentrated among few trading partners."),
        tags$li(tags$strong("World export concentration : "),
                " an HHI computed on world export shares (by exporting country) for that product also exceeds 0.4, meaning few countries in the world are even capable of supplying it \u2014 so switching to an alternative supplier is hard, not just currently avoided."),
        tags$li(tags$strong("Non-substitutability by domestic supply : "),
                " the ratio of the country's imports to its own exports of the product is above 1. This assumes a country's exports of a good broadly proxy the domestic production that could, in principle, be redirected to satisfy domestic demand instead; if imports exceed exports, domestic capacity cannot realistically cover the shortfall."),
        tags$li(tags$strong("Persistence : "),
                " all three criteria above must hold in at least two of the last three years, so a one-off or temporary spike in concentration does not count as a structural dependency.")
      ),
      p("The same four criteria, applied symmetrically, define ", tags$strong("export-dependent"),
        " products: import concentration and world import concentration replace the export-side equivalents, and the roles of imports and exports are reversed in the non-substitutability ratio. Use the \u201cShow dependencies for\u201d toggle above the map to switch between the two views."),
      p("On the map, exposure is shown either as the share of traded HS6 products for which the country is dependent (\u201cShare of products\u201d), or as the share of its total trade value concentrated in those dependent products (\u201cShare of trade value\u201d). Countries shown in dark grey are not dependent on any product (0%); countries left blank have no data."),
      p("When an Importer and an Exporter are both selected, the partner panels show their ",
        tags$em("leading"), " dependency partner \u2014 for the Importer, the exporter supplying the largest share of a given dependent product's import value; for the Exporter, the destination absorbing the largest share of a given dependent product's export value."),
      p("Sector groupings (Critical Raw Materials, Dual Use, Health, Agrifood, Energy, Other) come from dedicated reference lists (UNCTAD, EU dual-use regulation, CEPII health nomenclature, FAO, World Bank) and are only available for ", tags$strong("Import"), " dependencies (GeoDep_M); Export dependencies (GeoDep_X) are not sector-tagged."),
      p("Figures reflect 2024 (the only year for which the dependency indicators are available in this dataset) and EU-27 member states are aggregated into a single entity (EUN)."),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
  
  observeEvent(input$reset_button, {
    selected_countries(character())
  })
  
  observeEvent(input$sector_filter, {
    selected_countries(character())
  })
  
  observeEvent(input$dep_direction, {
    selected_countries(character())
    
    if (input$dep_direction == "export") {
      updateSelectInput(session, "sector_filter",
                        choices = c("All Sectors" = "all"),
                        selected = "all")
      updateSelectizeInput(session, "importer_select",
                           label = "Exporter (Origin):")
      updateSelectizeInput(session, "exporter_select",
                           label = "Importer (Destination):")
    } else {
      updateSelectInput(session, "sector_filter",
                        choices = sector_choices_ui,
                        selected = "all")
      updateSelectizeInput(session, "importer_select",
                           label = "Importer (Destination):")
      updateSelectizeInput(session, "exporter_select",
                           label = "Exporter (Origin):")
    }
  }, ignoreInit = FALSE)
  
  exclude_choice <- function(choices, exclude_val) {
    if (is.list(choices)) {
      lapply(choices, function(grp) grp[grp != exclude_val])
    } else {
      choices[choices != exclude_val]
    }
  }
  
  prev_sel <- reactiveVal(character())
  observeEvent(selected_countries(), {
    sel <- selected_countries()
    imp <- if (length(sel) >= 1) sel[1] else ""
    exp <- if (length(sel) >= 2) sel[2] else ""
    
    p_sel <- prev_sel()
    p_imp <- if (length(p_sel) >= 1) p_sel[1] else ""
    p_exp <- if (length(p_sel) >= 2) p_sel[2] else ""
    
    changed_imp <- !identical(imp, p_imp)
    changed_exp <- !identical(exp, p_exp)
    
    choices_imp <- if (exp != "") exclude_choice(country_choices_ui, exp) else country_choices_ui
    choices_exp <- if (imp != "") exclude_choice(country_choices_ui, imp) else country_choices_ui
    
    need_imp_ui <- !identical(input$importer_select, imp)
    need_exp_ui <- !identical(input$exporter_select, exp)
    
    update_imp <- need_imp_ui || changed_exp
    update_exp <- need_exp_ui || changed_imp
    
    prev_sel(sel)
    
    if (update_imp || update_exp) {
      if (update_imp) {
        updateSelectizeInput(session, "importer_select",
                             choices = choices_imp, selected = imp)
      }
      if (update_exp) {
        updateSelectizeInput(session, "exporter_select",
                             choices = choices_exp, selected = exp)
      }
    }
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$importer_select, {
    cur <- selected_countries()
    new_imp <- input$importer_select
    new_sel <- c(new_imp, if (length(cur) >= 2) cur[2] else NA)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    
    if (!identical(new_sel, cur)) {
      selected_countries(new_sel)
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$exporter_select, {
    cur <- selected_countries()
    imp <- if (length(cur) >= 1) cur[1] else NA
    new_sel <- c(imp, input$exporter_select)
    new_sel <- new_sel[!is.na(new_sel) & new_sel != ""]
    
    if (!identical(new_sel, cur)) {
      selected_countries(new_sel)
    }
  }, ignoreInit = TRUE)
  
  sector_map_data <- reactive({
    direction <- input$dep_direction
    
    base <- if (direction == "import") {
      traded_by_country |>
        rename(iso_plot = iso_d, dep1 = dependant_M_MC_t, dep2 = c4_M_MC, val = import_dpt)
    } else {
      traded_by_country_export |>
        rename(iso_plot = iso_o, dep1 = dependant_X_MC_t, dep2 = c4_X_MC, val = export_opt)
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    total_traded <- base |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_total = n(), total_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    dependent <- base |>
      filter(dep1 == 1, dep2 == 1) |>
      distinct(iso_plot, hs6, val) |>
      group_by(iso_plot) |>
      summarise(n_dep = n(), dep_value = sum(val, na.rm = TRUE), .groups = "drop")
    
    total_traded |>
      left_join(dependent, by = "iso_plot") |>
      mutate(
        n_dep       = replace_na(n_dep, 0),
        dep_value   = replace_na(dep_value, 0),
        count_share = 100 * n_dep / n_total,
        value_share = 100 * dep_value / total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  }) |> bindCache(input$sector_filter, input$dep_direction)
  
  export_hover_data <- reactive({
    export_base <- dep_export_base
    
    export_top3 <- export_base |>
      group_by(iso_o, hs6) |>
      mutate(dest_share = imports / export_opt) |>
      ungroup() |>
      filter(dest_share > 0.5) |>
      group_by(iso_o, iso_d) |>
      summarise(dep_count = n(), .groups = "drop") |>
      arrange(iso_o, desc(dep_count)) |>
      group_by(iso_o) |>
      slice_head(n = 3) |>
      mutate(part = paste0(iso_name(iso_d), " (", dep_count, ")")) |>
      summarise(export_top3 = paste(part, collapse = "; "), .groups = "drop") |>
      rename(iso_plot = iso_o)
    
    export_top3
  })
  
  partner_map_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base       <- dep_import_base |> filter(iso_d == iso1)
      partner_col <- "iso_o"
      total_col   <- "import_dpt"
      value_col   <- "imports"
    } else {
      base       <- dep_export_base |> filter(iso_o == iso1)
      partner_col <- "iso_d"
      total_col   <- "export_opt"
      value_col   <- "imports"
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    base <- base |> rename(partner_iso = all_of(partner_col))
    
    totals <- base |>
      distinct(hs6, .data[[total_col]]) |>
      summarise(total_value = sum(.data[[total_col]], na.rm = TRUE), n_products = n())
    
    base |>
      group_by(hs6) |>
      mutate(partner_share = .data[[value_col]] / .data[[total_col]]) |>
      ungroup() |>
      group_by(partner_iso) |>
      summarise(
        supply_value = sum(.data[[value_col]], na.rm = TRUE),
        n_dominant   = n_distinct(hs6[partner_share > 0.5]),
        .groups      = "drop"
      ) |>
      mutate(
        iso_plot    = partner_iso,
        n_dep       = n_dominant,
        n_total     = totals$n_products,
        dep_value   = supply_value,
        total_value = totals$total_value,
        count_share = 100 * n_dominant / totals$n_products,
        value_share = 100 * supply_value / totals$total_value
      ) |>
      select(iso_plot, count_share, value_share, n_dep, n_total, dep_value, total_value)
  })
  
  active_map_data <- reactive({
    sel <- selected_countries()
    if (length(sel) == 0) {
      sector_map_data()
    } else {
      data <- partner_map_data()
      if (is.null(data)) {
        data <- tibble(
          iso_plot = character(), count_share = double(), value_share = double(),
          n_dep = double(), n_total = double(), dep_value = double(), total_value = double()
        )
      }
      data
    }
  })
  
  sector_map_sf <- reactive({
    counts <- active_map_data()
    sel    <- selected_countries()
    
    out <- world_polygons |>
      mutate(iso_plot = to_eun(iso_a3)) |>
      mutate(feature_id = paste0(iso_a3, "___", row_number())) |>
      left_join(counts, by = "iso_plot")
    
    
    if (length(sel) >= 1) {
      countries_with_data <- if (input$dep_direction == "import") {
        unique(traded_by_country$iso_d)
      } else {
        unique(traded_by_country_export$iso_o)
      }
      
      n_tot <- if (all(is.na(out$n_total))) 0 else max(out$n_total, na.rm = TRUE)
      
      out <- out |>
        mutate(
          has_data    = iso_plot %in% countries_with_data,
          count_share = if_else(is.na(count_share) & has_data, 0, count_share),
          value_share = if_else(is.na(value_share) & has_data, 0, value_share),
          n_dep       = if_else(is.na(n_dep)       & has_data, 0, n_dep),
          n_total     = if_else(is.na(n_total)     & has_data, n_tot, n_total)
        )
    }
    out
  })
  
  output$intro_text <- renderUI({
    if (input$dep_direction == "export") {
      p("Click a country on the map to select it as the Exporter (Origin); click a second country to select it as the Importer (Destination). You can also search by name below. EU-27 member states are treated as a single entity (EUN).")
    } else {
      p("Click a country on the map to select it as the Importer (Destination); click a second country to select it as the Exporter (Origin). You can also search by name below. EU-27 member states are treated as a single entity (EUN).")
    }
  })
  
  output$dependency_map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE, minZoom = 2, maxZoom = 6,
                                     maxBoundsViscosity = 1.0)) |>
      addProviderTiles(providers$Esri.WorldGrayCanvas,
                       options = providerTileOptions(noWrap = TRUE)) |>
      setMaxBounds(-180, -85, 180, 85) |>
      htmlwidgets::onRender(
        "function(el, x) {
           L.control.zoom({ position: 'bottomright' }).addTo(this);
           var map = this;
     
           function fitIt() {
             map.invalidateSize();
             map.fitBounds([[-58, -170], [83, 190]]);
           }
     
           setTimeout(fitIt, 200);
           setTimeout(fitIt, 800);
           setTimeout(fitIt, 1500);

           window.addEventListener('resize', function() { map.invalidateSize(); });
           window.addEventListener('orientationchange', function() {
             setTimeout(function() { map.invalidateSize(); }, 300);
           });
         }"
      )
  })
  
  observe({
    map_sf <- sector_map_sf()
    metric <- input$map_metric
    sel    <- selected_countries()
    direction_label <- if (input$dep_direction == "import") "import" else "export"
    partner_role    <- if (input$dep_direction == "import") "exporter" else "destination"
    
    fill_values <- if (metric == "count") map_sf$count_share else map_sf$value_share
    
    if (length(sel) >= 1) {
      iso1_label <- iso_display_name(sel[1])
      metric_label <- if (metric == "count") {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               "s led by each ", partner_role)
      } else {
        paste0("Share of ", iso1_label, "'s dependent ", direction_label,
               " trade value by ", partner_role)
      }
    } else {
      metric_label <- if (metric == "count") {
        paste0("Share of products dependent (", direction_label, "s)")
      } else {
        paste0("Share of trade value dependent (", direction_label, "s)")
      }
    }
    
    pos_vals   <- fill_values[!is.na(fill_values) & fill_values > 0]
    pal_domain <- if (length(pos_vals) > 0) pos_vals else c(0, 1) 
    
    pal <- colorNumeric(
      palette  = "RdYlGn",
      domain   = pal_domain,
      reverse  = TRUE,
      na.color = "transparent"
    )
    
    fill_col <- dplyr::case_when(
      is.na(fill_values) ~ COL_NODATA,
      fill_values == 0   ~ COL_ZERO,
      TRUE               ~ pal(fill_values)
    )
    
    label_text <- with(map_sf, {
      header <- if_else(iso_plot == "EUN", "European Union (EU-27)", name)
      detail <- if (metric == "count") {
        ifelse(
          is.na(count_share), "No data",
          paste0(round(count_share, 1), "% (",
                 ifelse(is.na(n_dep), 0, n_dep), " of ",
                 ifelse(is.na(n_total), 0, n_total), " products)")
        )
      } else {
        ifelse(
          is.na(value_share), "No data",
          paste0(round(value_share, 1), "% of trade value")
        )
      }
      paste0("<strong>", header, "</strong><br/>", metric_label, ": ", detail)
    })
    label_arg <- lapply(label_text, HTML)
    
    rng          <- range(pal_domain)
    grad_cols    <- pal(seq(rng[1], rng[2], length.out = 10))
    gradient_css <- paste0("linear-gradient(to right, ",
                           paste(grad_cols, collapse = ", "), ")")

    span <- diff(rng)
    if (span > 0) {
      step_candidates <- c(0.5, 1, 2, 2.5, 5, 10, 20, 25, 50)
      tick_counts <- sapply(step_candidates, function(s) {
        lo   <- floor(rng[1] / s) * s
        hi   <- ceiling(rng[2] / s) * s
        vals <- seq(lo, hi, by = s)
        sum(vals >= rng[1] & vals <= rng[2])
      })
      best_step <- step_candidates[which.min(abs(tick_counts - 4))]
      lo        <- floor(rng[1] / best_step) * best_step
      hi        <- ceiling(rng[2] / best_step) * best_step
      tick_vals <- seq(lo, hi, by = best_step)
      tick_vals <- tick_vals[tick_vals >= rng[1] & tick_vals <= rng[2]]
      if (length(tick_vals) < 2) {
        tick_vals <- round(seq(rng[1], rng[2], length.out = 4) / best_step) * best_step
      }
      tick_pos  <- 100 * (tick_vals - rng[1]) / span
    } else {
      tick_vals <- round(rng[1] / 5) * 5
      tick_pos  <- 50
    }
    ticks_html <- paste0(
      "<div style='position:absolute; left:", tick_pos, "%; top:0; ",
      "transform:translateX(-50%); text-align:center;'>",
      "<div style='width:1px; height:5px; background:#666; margin:0 auto;'></div>",
      "<div style='font-size:10px; margin-top:1px; white-space:nowrap;'>",
      round(tick_vals, 1), "%</div></div>",
      collapse = ""
    )
    
    legend_html <- paste0(
      "<div class='horizontal-legend'>",
      "<div style='font-weight:bold; margin-bottom:4px;'>", metric_label, " (%)</div>",

      "<div style='margin:0 16px;'>",
      "<div style='height:12px; width:100%; opacity:0.7; border:1px solid #ccc; ",
      "box-sizing:border-box; background:", gradient_css, ";'></div>",
      "<div style='position:relative; height:26px;'>", ticks_html, "</div>",
      "</div>",

      "<div style='display:flex; gap:14px; margin-top:6px;'>",
      "<span><i style='display:inline-block; width:12px; height:12px; ",
      "vertical-align:middle; margin-right:4px; background:", COL_ZERO, ";'></i>",
      "Not dependent (0%)</span>",
      "<span><i style='display:inline-block; width:12px; height:12px; ",
      "vertical-align:middle; margin-right:4px; background:#fff; border:1px solid #999;'></i>",
      "No data</span>",
      "</div>",
      
      "<div style='font-size:10px; color:#666; margin-top:4px;'>",
      "Source: GeoDep IFE-CEPII (2026)</div>",
      "</div>"
    )
    
    proxy <- leafletProxy("dependency_map", data = map_sf) |>
      clearShapes() |>
      clearControls() |>
      addPolygons(
        fillColor   = fill_col,
        weight      = 1,
        color       = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        layerId      = ~feature_id,
        label        = label_arg,
        labelOptions = labelOptions(
          style     = list("font-size" = "12px", "max-width" = "260px", "white-space" = "normal"),
          textsize  = "12px",
          direction = "auto"
        )
      ) |>
      addControl(
        html      = HTML(legend_html),
        position  = "bottomleft",
        layerId   = "legend_main",
        className = "info legend"
      )
    
    if (length(sel) >= 1) {
      imp_sf <- map_sf |> filter(iso_plot == sel[1])
      if (nrow(imp_sf) > 0) {
        proxy <- proxy |> addPolygons(
          data = imp_sf, fill = FALSE, color = "#1f6f5c", weight = 4,
          opacity = 1, layerId = paste0("highlight_importer_", imp_sf$feature_id)
        )
      }
    }
    if (length(sel) >= 2) {
      exp_sf <- map_sf |> filter(iso_plot == sel[2])
      if (nrow(exp_sf) > 0) {
        proxy <- proxy |> addPolygons(
          data = exp_sf, fill = FALSE, color = "#8b3a3a", weight = 4,
          opacity = 1, layerId = paste0("highlight_exporter_", exp_sf$feature_id)
        )
      }
    }
  })
  
  observeEvent(input$dependency_map_shape_click, {
    click <- input$dependency_map_shape_click
    raw_iso <- sub("___.*$", "", click$id)
    clicked_iso <- to_eun(raw_iso)
    current_selection <- selected_countries()
    
    if (length(current_selection) >= 2) {
      selected_countries(clicked_iso)
    } else if (clicked_iso %in% current_selection) {
      return(NULL)
    } else {
      selected_countries(c(current_selection, clicked_iso))
    }
  })
  
  observeEvent(input$swap_button, {
    current_selection <- selected_countries()
    if (length(current_selection) == 2) {
      selected_countries(rev(current_selection))
    }
  })
  
  sector_chart_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    req(input$dep_direction == "import")
    
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    
    sector_cols <- setdiff(unlist(sector_choices_ui, use.names = FALSE),
                           c("all", "sect_strategic"))
    
    base <- dep_import_base |>
      filter(iso_d == iso1) |>
      group_by(hs6) |>
      mutate(partner_share = imports / import_dpt) |>
      ungroup() |>
      mutate(is_dominant = !is.na(iso2) & iso_o == iso2 & partner_share > 0.5)
    
    if (nrow(base) == 0) return(NULL)
    
    dominant_by_hs6 <- base |>
      group_by(hs6) |>
      summarise(is_dominant = any(is_dominant), .groups = "drop")
    
    total_dep      <- nrow(dominant_by_hs6)
    total_dominant <- sum(dominant_by_hs6$is_dominant)
    
    sectors_by_hs6 <- base |>
      distinct(hs6, across(all_of(sector_cols)))
    
    strategic_hs6 <- sectors_by_hs6 |>
      filter(if_any(all_of(sector_cols), ~ .x == 1)) |>
      pull(hs6)
    
    total_strategic          <- length(unique(strategic_hs6))
    total_dominant_strategic <- sum(dominant_by_hs6$is_dominant[dominant_by_hs6$hs6 %in% strategic_hs6])
    
    out <- dominant_by_hs6 |>
      left_join(sectors_by_hs6, by = "hs6") |>
      pivot_longer(cols = all_of(sector_cols), names_to = "sector_code", values_to = "flag") |>
      filter(flag == 1) |>
      mutate(Sector_Name = unlist(sector_names[sector_code])) |>
      group_by(Sector_Name) |>
      summarise(
        n_dep      = n(),
        n_dominant = sum(is_dominant),
        .groups    = "drop"
      ) |>
      mutate(share_dominant = if_else(n_dep > 0, 100 * n_dominant / n_dep, 0))
    
    attr(out, "total_dep")               <- total_dep
    attr(out, "total_dominant")          <- total_dominant
    attr(out, "total_strategic")         <- total_strategic          
    attr(out, "total_dominant_strategic") <- total_dominant_strategic
    out
  })
  
  make_sector_chart <- function() {
    if (input$dep_direction != "import") return(NULL)
    
    df <- sector_chart_data()
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    selection <- selected_countries()
    iso1 <- selection[1]
    iso2 <- if (length(selection) >= 2) selection[2] else NA_character_
    direction_label <- "exporter"
    flow_label       <- "import"
    
    dominant_label <- if (!is.na(iso2)) paste0("Leading supplier: ", iso_display_name(iso2)) else "Other suppliers"
    
    total_dep      <- attr(df, "total_dep")
    total_dominant <- attr(df, "total_dominant")
    total_strategic          <- attr(df, "total_strategic")            
    total_dominant_strategic <- attr(df, "total_dominant_strategic")  
    
    subtitle_text <- if (!is.na(iso2)) {
      paste0(
        "Out of ", total_strategic, " strategic products for which ", iso_name(iso1),
        " is import-dependent, there are ", total_dominant_strategic, " for which ",
        iso_name(iso2), " is the leading exporter"
      )
    } else {
      paste0(
        "Out of ", total_strategic, " strategic products for which ", iso_display_name(iso1),
        " is import-dependent"
      )
    }
    
    df_long <- df |>
      mutate(n_other = n_dep - n_dominant) |>
      select(Sector_Name, n_dep, n_dominant, n_other) |>
      pivot_longer(cols = c(n_dominant, n_other), names_to = "category", values_to = "n") |>
      mutate(category = if_else(category == "n_dominant", dominant_label, "Other"))
    
    sector_order <- df |> arrange(n_dep) |> pull(Sector_Name)
    df_long <- df_long |> mutate(Sector_Name = factor(Sector_Name, levels = sector_order))
    if (length(selection) >= 2) {
      title_text <- paste0(iso_name(iso1), " ",flow_label, "-dependent products mainly supplied by ", iso_name(iso2))
    } else {
      title_text <- paste0(iso_display_name(iso1), " ",flow_label, " dependencies by sector")
    }
    
    title_text <- paste(strwrap(title_text, width = 58), collapse = "\n")
    
    p <- ggplot(df_long, aes(x = Sector_Name, y = n, fill = category)) +
      geom_col(width = 0.75, color = "white", linewidth = 0.4) +
      geom_text(
        aes(label = ifelse(n > 0, n, "")),
        position = position_stack(vjust = 0.5),
        color = "white", fontface = "bold", size = 3.3
      ) +
      coord_flip() +
      labs(
        x = NULL,
        y = "Dependent products",
        title = title_text,
        subtitle = subtitle_text,
        fill = NULL,
        caption = "Source : GeoDep IFE-CEPII (2026) \n  Note: sectors are not mutually exclusive, a product can belong to more than one sector"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", size = 15, color = "#2b2b2b",
                                     hjust = 0.5, lineheight = 1.15, margin = margin(b = 6)),
        plot.subtitle = element_text(size = 11, color = "#666666", hjust = 0.5, margin = margin(b = 15)),
        plot.caption  = element_text(hjust = 0.5, size = 8.5, color = "#888888", face = "italic",
                                     margin = margin(t = 14)),
        
        panel.grid.major.y = element_blank(),
        panel.grid.minor   = element_blank(),
        panel.grid.major.x = element_line(color = "#e5e5e5", linewidth = 0.5, linetype = "dashed"),
        
        axis.text.y   = element_text(face = "bold", color = "#333333", size = 11),
        axis.text.x   = element_text(color = "#555555"),
        axis.title.x  = element_text(color = "#444444", margin = margin(t = 12)),
        
        legend.position      = "bottom",
        legend.justification = "center",
        legend.margin        = margin(t = 4, b = 0),
        legend.text          = element_text(size = 11, color = "#333333")
      )
    if (!is.na(iso2)) {
      p <- p + scale_fill_manual(values = setNames(c("#8b3a3a", "#1f6f5c"),
                                                   c(dominant_label, "Other")))
    } else {
      p <- p + scale_fill_manual(values = c("Other" = "#1f6f5c"), guide = "none")
    }
    
    p
  }
  
  output$sector_chart <- renderPlot({
    make_sector_chart()
  })
  output$download_plot <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1 <- selection[1]
      iso2 <- if (length(selection) >= 2) paste0("_", selection[2]) else ""
      direction <- input$dep_direction
      paste0("GeoDep_sector_chart_", direction, "_", iso1, iso2, "_2024.png")
    },
    content = function(file) {
      
      p <- make_sector_chart()
      req(p)
      
      ggsave(filename = file, plot = p, device = "png",
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  output$download_partner_plot <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1      <- selection[1]
      direction <- input$dep_direction
      paste0("GeoDep_partner_chart_", direction, "_", iso1, "_2024.png")
    },
    content = function(file) {
      selection <- selected_countries()
      req(length(selection) >= 1)
      iso1      <- selection[1]
      direction <- input$dep_direction
      
      p <- tryCatch(make_partner_sector_chart(iso1, direction, input$top_n_slider), error = function(e) NULL)
      req(p)
      
      ggsave(filename = file, plot = p, device = "png",
             width = 10, height = 6, dpi = 300, bg = "white")
    }
  )
  
  partner_sector_chart_data <- function(iso1, direction = "import", top_n = 10) {
    if (direction == "import") {
      base        <- imports_final |> filter(iso_d == iso1)
      partner_col <- "first_odpt"
    } else {
      base        <- exports_final |> filter(iso_o == iso1)
      partner_col <- "first_dpto"
    }
    
    if ("dependent" %in% names(base)) {
      base <- base |> filter(dependent == 1)
    }
    
    if (nrow(base) == 0) return(NULL)
    
    df0 <- base |> distinct(hs6, partner = .data[[partner_col]])
    
    topN_partners <- df0 |>
      count(partner, sort = TRUE, name = "n_products") |>
      slice_head(n = top_n) |>
      pull(partner)
    
    if (length(topN_partners) == 0) return(NULL)
    
    partner_labels <- topN_partners
    
    df <- df0 |>
      mutate(partner_group = if_else(partner %in% topN_partners,
                                     partner, "ROW")) |>
      count(partner_group, name = "n_dep")
    
    if (nrow(df) == 0) return(NULL)
    
    df |>
      mutate(partner_group = factor(partner_group, levels = c(partner_labels, "ROW"))) |>
      arrange(partner_group)
  }
  
  make_partner_sector_chart <- function(iso1, direction = "import", top_n = 10) {
    df <- partner_sector_chart_data(iso1, direction, top_n)
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    partner_levels <- levels(df$partner_group)
    n_partners     <- length(partner_levels) - 1
    
    base_colors <- c("#1f6f5c", "#8b3a3a", "#4a7c9e", "#e69f00", "#56b4e9",
                     "#009e73", "#f0e442", "#0072b2", "#d55e00", "#cc79a7")
    
    if (n_partners <= length(base_colors)) {
      fill_colors <- setNames(base_colors[seq_len(n_partners)], partner_levels[seq_len(n_partners)])
    } else {
      expanded_pal <- colorRampPalette(base_colors)(n_partners)
      fill_colors <- setNames(expanded_pal, partner_levels[seq_len(n_partners)])
    }
    fill_colors <- c(fill_colors, "ROW" = "#b0b0b0")
    
    total_dep <- sum(df$n_dep)
    
    role_label   <- if (direction == "import") "import-dependent" else "export-dependent"
    partner_role <- if (direction == "import") "leading exporters" else "leading destinations"
    
    shown_isos   <- setdiff(partner_levels, "ROW")
    legend_pairs <- c(paste0(shown_isos, " = ", iso_name(shown_isos)),
                      "ROW = Rest of the World")
    legend_text  <- paste(legend_pairs, collapse = ", ")
    legend_text  <- paste(strwrap(legend_text, width = 80), collapse = "<br>")
    
    subtitle_text <- paste0(
      "**Out of ", total_dep, " products for which ", iso_display_name(iso1),
      " is ", role_label, "**<br>", "Breakdown by Top ", n_partners, " ", partner_role
    )
    
    df <- df |>
      mutate(
        partner_label = if_else(partner_group == "ROW", "ROW", iso_name(as.character(partner_group))),
        pct      = 100 * n_dep / total_dep,
        lab_text = paste0(partner_label, "\n", n_dep, " (", round(pct, 1), "%)")
      )
    
    ggplot(df, aes(area = n_dep, fill = partner_group, label = lab_text)) +
      geom_treemap(color = "white", size = 1.5) +
      geom_treemap_text(
        color = "white",
        place = "centre",
        grow = FALSE,
        reflow = TRUE,
        fontface = "bold",
        size = 11,
        lineheight = 0.9
      ) +
      labs(
        title = paste0(iso_display_name(iso1), " ", role_label, " products"),
        subtitle = subtitle_text,
        caption  = "Source : GeoDep IFE-CEPII (2026)"
      ) +
      scale_fill_manual(values = fill_colors, guide = "none") +
      theme_minimal(base_size = 12) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5, size = 15, color = "#2b2b2b",
                                     margin = margin(b = 4)),
        plot.subtitle = ggtext::element_markdown(hjust = 0.5, size = 10, color = "#666666",
                                                 margin = margin(b = 14), lineheight = 1.3),
        plot.caption  = element_text(hjust = 1, size = 9, color = "#888888", face = "italic",
                                     margin = margin(t = 12)),
        plot.margin   = margin(10, 10, 10, 10)
      )
  }
  
  output$table_heading <- renderUI({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    direction <- input$dep_direction
    iso1_name <- iso_display_name(selection[1])
    
    if (length(selection) == 1) {
      title_text <- if (direction == "import") {
        paste0("Products for which ", iso1_name, " is import-dependent :")
      } else {
        paste0("Products for which ", iso1_name, " is export-dependent :")
      }
    } else {
      iso2_name <- iso_display_name(selection[2])
      title_text <- if (direction == "import") {
        paste0("Products for which ", iso1_name, " is import-dependent and ", iso2_name, " is the leading exporter :")
      } else {
        paste0("Products for which ", iso1_name, " is export-dependent and ", iso2_name, " is the first destination :")
      }
    }
    h3(class = "table-heading", title_text)
  })
  
  output$partners_panel <- renderUI({
    selection <- selected_countries()
    if (length(selection) == 0) return(NULL)
    
    direction <- input$dep_direction
    
    if (direction == "import") {
      
      if (length(selection) == 1) {
        iso1 <- selection[1]
        plot_id <- "partner_sector_plot_1"
        
        local({
          iso_local <- iso1
          output[[plot_id]] <- renderPlot({
            p <- tryCatch(make_partner_sector_chart(iso_local, "import", input$top_n_slider),
                          error = function(e) NULL)
            req(p)
            p
          })
        })
        
        return(
          div(class = "well",
              div(style = "display: flex; gap: 24px; align-items: stretch;",
                  div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
                      div(style = "min-height: 68px;",
                          h4(iso_name(iso1))
                      ),
                      withSpinner(plotOutput(plot_id, height = "320px"), type = 8, color = "#1f6f5c"),
                      div(style = "text-align: right; margin-top: auto; padding-top: 10px;",
                          downloadButton("download_partner_plot", "Download Graph (PNG)",
                                         style = "background-color: #6c7a76; color: white; border: none;")
                      )
                  ),
                  div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
                      div(style = "min-height: 68px;",
                          h4("")
                      ),
                      withSpinner(plotOutput("sector_chart", height = "320px"), type = 8, color = "#1f6f5c"),
                      div(style = "text-align: right; margin-top: auto; padding-top: 10px;",
                          downloadButton("download_plot", "Download Graph (PNG)",
                                         style = "background-color: #6c7a76; color: white; border: none;")
                      )
                  )
              )
          )
        )
      } else {
        return(
          div(class = "well",
              withSpinner(plotOutput("sector_chart", height = "320px"), type = 8, color = "#1f6f5c"),
              div(style = "text-align: right; margin-top: 10px;",
                  downloadButton("download_plot", "Download Graph (PNG)",
                                 style = "background-color: #6c7a76; color: white; border: none;")
              )
          )
        )
      }
      
    } else {
      
      make_export_plot_panel <- function(idx, plot_id, with_download = FALSE) {
        iso_local <- selection[idx]
        
        local({
          iso_fixed <- iso_local
          output[[plot_id]] <- renderPlot({
            p <- tryCatch(make_partner_sector_chart(iso_fixed, "export", input$top_n_slider),
                          error = function(e) NULL)
            req(p)
            p
          })
        })
        
        div(style = "flex: 1; min-width: 0; display: flex; flex-direction: column;",
            withSpinner(plotOutput(plot_id, height = "320px"), type = 8, color = "#1f6f5c"),
            if (with_download) {
              div(style = "text-align: right; margin-top: auto; padding-top: 10px;",
                  downloadButton("download_partner_plot", "Download Graph (PNG)",
                                 style = "background-color: #6c7a76; color: white; border: none;")
              )
            }
        )
      }
      
      charts <- tryCatch({
        if (length(selection) == 1) {
          div(style = "display: flex;",
              make_export_plot_panel(1, "partner_export_plot_1", with_download = TRUE)
          )
        } else {
          div(style = "display: flex; gap: 24px; align-items: stretch;",
              make_export_plot_panel(1, "partner_export_plot_1", with_download = TRUE)
          )
        }
      }, error = function(e) {
        div("Unable to display the partner breakdown for this selection.")
      })
      
      
      div(class = "well", charts)
    }
  })
  
  filtered_dependency_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    if (length(selection) == 2) {
      subject <- selection[1]
      partner <- selection[2]
      
      if (input$dep_direction == "import") {
        result <- dep_import_base |>
          filter(iso_d == subject, iso_o == partner) |>
          group_by(hs6) |>
          mutate(origin_share = imports / import_dpt) |>
          ungroup() |>
          filter(origin_share > 0.5)
        
        if (input$sector_filter != "all") {
          result <- result |> filter(.data[[input$sector_filter]] == 1)
        }
        
        result <- result |>
          mutate(share_odpt = origin_share * 100) |>
          select(
            `HS6 Product` = hs6,
            `Description` = Description,
            `Total Imports (World, k$)` = import_dpt,
            `Imports from Origin (k$)` = imports,
            `Share from Origin (%)` = share_odpt,
            starts_with("sect_"),
            -sect_strategic
          ) |>
          arrange(desc(`Imports from Origin (k$)`))
      } else {
        result <- dep_export_base |>
          filter(iso_o == subject, iso_d == partner) |>
          group_by(hs6) |>
          mutate(dest_share = imports / export_opt) |>
          ungroup() |>
          filter(dest_share > 0.5)
        
        result <- result |>
          mutate(share_dpto = dest_share * 100) |>
          select(
            `HS6 Product` = hs6,
            `Description` = Description,
            `Total Exports (World, k$)` = export_opt,
            `Exports to Destination (k$)` = imports,
            `Share to Destination (%)` = share_dpto
          ) |>
          arrange(desc(`Exports to Destination (k$)`))
      }
      return(result)
    }
    
    iso1 <- selection[1]
    
    if (input$dep_direction == "import") {
      base        <- dep_import_base |> filter(iso_d == iso1)
      total_col   <- "import_dpt"
      total_label <- "Total Imports (World, k$)"
      partner_label  <- "First exporter (% share)"
      partner_col <- "first_odpt"
      value_col   <- "imports"
      match_col   <- "iso_o"
    } else {
      base        <- dep_export_base |> filter(iso_o == iso1)
      total_col   <- "export_opt"
      total_label <- "Total Exports (World, k$)"
      partner_label  <- "First destination (% share)"
      partner_col <- "first_dpto"
      value_col   <- "imports"
      match_col   <- "iso_d"
    }
    
    if (input$sector_filter != "all" && input$sector_filter %in% names(base)) {
      base <- base |> filter(.data[[input$sector_filter]] == 1)
    }
    
    optional_cols <- intersect(
      c("Description", grep("^sect_", names(base), value = TRUE)),
      names(base)
    )
    
    optional_cols <- setdiff(optional_cols, "sect_strategic")

    share_lookup <- base |>
      filter(.data[[match_col]] == .data[[partner_col]]) |>
      mutate(partner_share = 100 * .data[[value_col]] / .data[[total_col]]) |>
      distinct(hs6, partner_share)
    
    result <- base |>
      mutate(!!partner_label := iso_name(.data[[partner_col]])) |>
      distinct(across(all_of(c("hs6", total_col, partner_label, optional_cols)))) |>
      left_join(share_lookup, by = "hs6") |>
      mutate(
        !!partner_label := if_else(
          !is.na(partner_share),
          paste0(.data[[partner_label]], " (", round(partner_share, 1), "%)"),
          .data[[partner_label]]
        )
      ) |>
      select(-partner_share) |>
      arrange(desc(.data[[total_col]]))
    
    colnames(result)[colnames(result) == "hs6"]      <- "HS6 Product"
    colnames(result)[colnames(result) == total_col] <- total_label
    
    result
  })
  
  output$dependency_table <- renderDT({
    data <- filtered_dependency_data()
    sect_cols <- c("sect_crm", "sect_dual_use", "sect_health", "sect_agrifood", "sect_energy", "sect_other")
    sect_labels <- c("Critical Raw Materials", "Dual Use", "Health", "Agrifood", "Energy", "Other")
    
    existing_cols <- intersect(sect_cols, names(data))
    if (length(existing_cols) > 0) {
      if (nrow(data) > 0) {
        data$Sector <- apply(data[existing_cols], 1, function(row) {
          labels_to_use <- sect_labels[sect_cols %in% existing_cols]
          paste(labels_to_use[which(row == 1)], collapse = ", ")
        })
      } else {
        data$Sector <- character(0)
      }
      data <- data |> select(-starts_with("sect_"))
    }
    
    front_cols <- c(
      "HS6 Product",
      "Description",
      "Sector",
      "First exporter (% share)",
      "First destination (% share)",
      "Total Imports (World, k$)",
      "Total Exports (World, k$)"
    )
    existing_front <- intersect(front_cols, names(data))
    remaining_cols <- setdiff(names(data), existing_front)
    data <- data[, c(existing_front, remaining_cols), drop = FALSE]
    
    value_cols <- intersect(
      c("Total Imports (World, k$)", "Total Exports (World, k$)",
        "Imports from Origin (k$)", "Exports to Destination (k$)"),
      colnames(data)
    )
    share_col <- intersect(c("Share from Origin (%)", "Share to Destination (%)"), colnames(data))
    
    order_col <- if ("Imports from Origin (k$)" %in% colnames(data)) {
      "Imports from Origin (k$)"
    } else if ("Exports to Destination (k$)" %in% colnames(data)) {
      "Exports to Destination (k$)"
    } else {
      value_cols[1]
    }
    
    empty_msg <- if (length(selected_countries()) == 2) {
      "No product where the importer is dependent and this exporter is the  supplier (>50%)."
    } else {
      "No dependent products found for this selection."
    }
    
    dt <- datatable(
      data,
      rownames  = FALSE,
      selection = "none",
      options = list(
        pageLength = 10,
        language = list(
          search      = "Search:",
          lengthMenu  = "Show _MENU_ entries",
          info        = "Showing _START_ to _END_ of _TOTAL_ entries",
          paginate    = list(previous = "Previous", `next` = "Next"),
          emptyTable  = empty_msg
        ),
        order = list(list(which(colnames(data) == order_col) - 1, "desc"))
      )
    ) |>
      formatCurrency(
        columns  = value_cols,
        currency = "",
        interval = 3,
        mark     = ",",
        digits   = 0
      )
    
    if (length(share_col) == 1) {
      dt <- dt |> formatRound(columns = share_col, digits = 1)
    }
    
    dt
  })
  
  selection_full_data <- reactive({
    selection <- selected_countries()
    req(length(selection) >= 1)
    
    direction <- input$dep_direction
    iso1      <- selection[1]
    
    full_data <- if (direction == "import") {
      imports_final |> select(-sect_strategic, -Description)
    } else {
      exports_final |> select(-Description)
    }
    
    if (direction == "import") {
      result <- full_data |> filter(iso_d == iso1)
      
      if (input$sector_filter != "all" && input$sector_filter %in% names(result)) {
        result <- result |> filter(.data[[input$sector_filter]] == 1)
      }
      
      if (length(selection) == 2) {
        iso2   <- selection[2]
        result <- result |> filter(first_odpt == iso2)
      }
    } else {
      result <- full_data |> filter(iso_o == iso1)
      
      if (length(selection) == 2) {
        iso2   <- selection[2]
        result <- result |> filter(first_dpto == iso2)
      }
    }
    
    result
  })
  
  output$download_table <- downloadHandler(
    filename = function() {
      selection <- selected_countries()
      prefix    <- if (input$dep_direction == "import") "GeoDep_M" else "GeoDep_X"
      
      if (length(selection) == 2) {
        paste0(prefix, "_dependencies_from_", selection[2], "_to_", selection[1], "_",
               Sys.Date(), ".zip")
      } else if (length(selection) == 1) {
        paste0(prefix, "_dependencies_", selection[1], "_", input$dep_direction, "_",
               Sys.Date(), ".zip")
      } else {
        paste0(prefix, "_dependencies_", Sys.Date(), ".zip")
      }
    },
    contentType = "application/zip",
    content = function(file) {
      selection <- selected_countries()
      direction <- input$dep_direction
      prefix    <- if (direction == "import") "GeoDep_M" else "GeoDep_X"
      
      zip_filename <- if (length(selection) == 2) {
        paste0(prefix, "_dependencies_from_", selection[2], "_to_", selection[1], "_",
               Sys.Date(), ".zip")
      } else if (length(selection) == 1) {
        paste0(prefix, "_dependencies_", selection[1], "_", direction, "_",
               Sys.Date(), ".zip")
      } else {
        paste0(prefix, "_dependencies_", Sys.Date(), ".zip")
      }
      
      tmp_dir <- tempfile("geodep_export_")
      dir.create(tmp_dir)
      on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
      
      csv_name  <- paste0(prefix, "_selection.csv")
      csv_path  <- file.path(tmp_dir, csv_name)
      
      write_excel_csv2(selection_full_data(), csv_path) 
      included_ref_files <- copy_reference_files(tmp_dir)
      
      size_kb_unzipped <- format_kb(file.info(csv_path)$size)
      
      readme_path <- file.path(tmp_dir, "README.txt")
      
      readme_text_draft <- generate_readme(
        direction        = direction,
        selection        = selection,
        sector_filter    = input$sector_filter,
        map_metric       = input$map_metric,
        zip_filename     = zip_filename,
        size_kb_unzipped = size_kb_unzipped,
        size_kb_zipped   = NULL
      )
      writeLines(readme_text_draft, readme_path, useBytes = TRUE)
      
      tmp_zip_path <- file.path(tmp_dir, "__probe.zip")
      zip::zip(
        zipfile = tmp_zip_path,
        files   = c(basename(csv_path), basename(readme_path), included_ref_files),
        root    = tmp_dir
      )
      size_kb_zipped <- format_kb(file.info(tmp_zip_path)$size)
      file.remove(tmp_zip_path)
      
      readme_text_final <- generate_readme(
        direction        = direction,
        selection        = selection,
        sector_filter    = input$sector_filter,
        map_metric       = input$map_metric,
        zip_filename     = zip_filename,
        size_kb_unzipped = size_kb_unzipped,
        size_kb_zipped   = size_kb_zipped
      )
      writeLines(readme_text_final, readme_path, useBytes = TRUE)
      
      zip::zip(
        zipfile = file,
        files   = c(basename(csv_path), basename(readme_path), included_ref_files),
        root    = tmp_dir
      )
    }
  )
  
  output$download_full <- downloadHandler(
    filename = function() {
      prefix <- if (input$dep_direction == "import") "GeoDep_M" else "GeoDep_X"
      paste0(prefix, "_full_", Sys.Date(), ".zip")
    },
    contentType = "application/zip",
    content = function(file) {
      withProgress(message = "Preparing full dataset", value = 0, {
        
        direction <- input$dep_direction
        prefix    <- if (direction == "import") "GeoDep_M" else "GeoDep_X"
        
        incProgress(0.1, detail = "Loading data")
        
        imports_final <- imports_final |>
          select(-sect_strategic,-Description)
        
        exports_final <- exports_final |>
          select(-Description)
        
        full_data <- if (direction == "import") imports_final else exports_final
        
        zip_filename <- paste0(prefix, "_full_", Sys.Date(), ".zip")
        
        tmp_dir <- tempfile("geodep_full_export_")
        dir.create(tmp_dir)
        on.exit(unlink(tmp_dir, recursive = TRUE), add = TRUE)
        
        csv_name <- paste0(prefix, "_full.csv")
        csv_path <- file.path(tmp_dir, csv_name)
        
        incProgress(0.2, detail = "Writing CSV")
        write_excel_csv2(full_data, csv_path)
        included_ref_files <- copy_reference_files(tmp_dir)
        
        size_kb_unzipped <- format_kb(file.info(csv_path)$size)
        
        incProgress(0.2, detail = "Generating README")
        readme_path <- file.path(tmp_dir, "README.txt")
        
        readme_text_draft <- generate_readme(
          direction        = direction,
          selection        = character(),
          sector_filter    = "all",
          map_metric       = input$map_metric,
          zip_filename     = zip_filename,
          size_kb_unzipped = size_kb_unzipped,
          size_kb_zipped   = NULL,
          is_full          = TRUE
        )
        writeLines(readme_text_draft, readme_path, useBytes = TRUE)
        
        incProgress(0.2, detail = "Compressing (pass 1/2)")
        tmp_zip_path <- file.path(tmp_dir, "__probe.zip")
        zip::zip(
          zipfile = tmp_zip_path,
          files   = c(basename(csv_path), basename(readme_path), included_ref_files),
          root    = tmp_dir
        )
        size_kb_zipped <- format_kb(file.info(tmp_zip_path)$size)
        file.remove(tmp_zip_path)
        
        readme_text_final <- generate_readme(
          direction        = direction,
          selection        = character(),
          sector_filter    = "all",
          map_metric       = input$map_metric,
          zip_filename     = zip_filename,
          size_kb_unzipped = size_kb_unzipped,
          size_kb_zipped   = size_kb_zipped,
          is_full          = TRUE
        )
        writeLines(readme_text_final, readme_path, useBytes = TRUE)
        
        incProgress(0.2, detail = "Compressing (pass 2/2)")
        zip::zip(
          zipfile = file,
          files   = c(basename(csv_path), basename(readme_path), included_ref_files),
          root    = tmp_dir
        )
        
        incProgress(0.1, detail = "Done")
      })
    }
  )
}

shinyApp(ui = ui, server = server)
