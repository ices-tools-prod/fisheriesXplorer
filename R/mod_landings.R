landings_about_box <- function() {
  tab_about_box(
    title = "About this module",

    shiny::p(
      style = "margin: 0;",
      "Landings data are shown for all reported taxa, including but not limited to those assessed by ICES. ",
      "Data shown are a combination of historical nominal catches and official nominal catches",
      shiny::tags$a(
        href = "https://www.ices.dk/data/dataset-collections/Pages/Fish-catch-and-stock-assessment.aspx",
        target = "_blank",
        rel = "noopener noreferrer",
        "ICES Catch statistics"
      ), br(),
      "Landings are reported by ICES areas and divisions and assigned to an ecoregion according to the procedure described in ", 
      shiny::tags$a(
        href = "https://doi.org/10.17895/ices.pub.22134020",
        target = "_blank",
        rel = "noopener noreferrer",
        "ICES (2023)."
      ), br(),
      "Taxa displayed represent the largest fraction of reported landings in the official nominal catches (i.e. since 2006);",
      "the remainder are labelled as “other”, with taxa contributions displayed when hovering over the line."
    )
  )
}

discards_about_box <- function() {
  tab_about_box(
    title = "About this module",

    shiny::p(
      style = "margin: 0;",
      "Discard trends are shown only for stocks assessed by ICES. Discards rates are calculated for each fisheries guild as", 
      "the sum of all discards divided by the sum of all reported catches (i.e. landings plus discards), ", 
      "combining all stocks in that guild, and weighted by the proportion of each stock's landings occurring", 
      "within the ecoregion, based on the RDBES assignment procedure ",
      shiny::tags$a(
        href = "https://github.com/ices-taf/fisheriesXplorer_ecoregion_landings_shares_method",
        target = "_blank",
        rel = "noopener noreferrer",
        "TAF repository."
      ),
      "Note that not all countries fishing on these stocks have reported data to RDBES.", br(),
      "Detailed annual information on stocks included or not included in discard rate calculations can be displayed ",
      "Note that discards are not differentiated between dead discards and live discards; ",
      "for some fisheries guilds, e.g. shellfish and elasmobranchs, discard survival rates may be high."
    )
  )
}

#' Landings and discards UI module
#'
#' This module UI creates the main \emph{Landings} section of the app,
#' with two tabs: aggregated landings by different categories and
#' discards trends/coverage. It uses a side panel for explanatory text
#' and cards for plots and controls.
#'
#' @param id A character string used as the module namespace. Passed to
#'   \code{shiny::NS()} and used to namespace all UI elements within the
#'   module.
#'
#' @details
#' The UI contains:
#' \itemize{
#'   \item A header created by \code{mod_flex_header_ui()} showing the
#'     current ecoregion label and date.
#'   \item A \code{tabsetPanel} with two tabs:
#'     \itemize{
#'       \item \code{"Landings"}: a sidebar with dynamic explanatory text
#'         (\code{landings_text}) and a main card containing:
#'         \itemize{
#'           \item A radio-button selector for the landings layer
#'             (main landed species, fisheries guild, country).
#'           \item A CSV download link for landings data
#'             (\code{download_landings_data}).
#'           \item A dynamic output for the selected layer
#'             (\code{landings_layer}), typically a map or plot, wrapped
#'             in \code{shinycssloaders::withSpinner()}.
#'         }
#'       \item \code{"Discards"}: a sidebar with dynamic text
#'         (\code{discards_text}) and a viewport-height card containing:
#'         \itemize{
#'           \item An HTML discard trends plot
#'             (\code{discard_trends_html}) with a CSV download link
#'             (\code{download_discard_data}).
#'         }
#'     }
#' }
#'
#' The corresponding server logic is implemented in
#' \code{mod_landings_server()}.
#'
#' @return A \code{shiny.tag.list} representing the landings and discards
#'   UI, suitable for inclusion in a Shiny app.
#'
#' @importFrom shiny NS tagList tabsetPanel tabPanel sidebarLayout uiOutput radioButtons downloadLink
#' @importFrom bslib layout_sidebar sidebar card card_header card_body
#' @importFrom shinycssloaders withSpinner
#' @importFrom plotly plotlyOutput
#'
#' @examples
#' \dontrun{
#'   library(shiny)
#'   ui <- fluidPage(mod_landings_ui("landings"))
#'   server <- function(input, output, session) {
#'     mod_landings_server("landings")
#'   }
#'   shinyApp(ui, server)
#' }
#'
#' @export
mod_landings_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_flex_header_ui(
      ns,
      "ecoregion_label",
      "current_date"
    ),

    tabsetPanel(
      id = ns("main_tabset"),

      # ------------------------------------------------------------
      # Landings
      # ------------------------------------------------------------
      tabPanel(
        title = "Landings",
        value = "landings",

        layout_sidebar(
          bg = "white",
          fg = "black",

          # Let the whole layout grow when the about box expands.
          fill = FALSE,
          fillable = TRUE,
          gap = "12px",

          sidebar = sidebar(
            width = "20vw",
            bg = "white",
            fg = "black",
            open = TRUE,

            uiOutput(ns("landings_text"))
          ),

          landings_about_box(),

          card(
            # This height belongs to the plot card only.
            height = "85vh",
            fill = FALSE,

            card_header(
              radioButtons(
                ns("landings_layer_selector"),
                "Select grouping:",
                inline = TRUE,
                choices = c(
                  "Main landed species" = "Common name",
                  "Fisheries Guild" = "Fisheries guild",
                  "Country" = "Country"
                )
              ),

              download_icon_label(
                text = "Download data",
                outputId = ns("download_landings_data"),
                hover_text = "Landings (.csv, takes a few seconds)",
                size = "large"
              )
            ),

            card_body(
              min_height = 0,

              uiOutput(ns("guild_selector_ui")),

              bslib::as_fill_carrier(
                shinycssloaders::withSpinner(
                  plotlyOutput(
                    ns("landings_layer"),
                    height = "100%"
                  )
                )
              )
            )
          )
        )
      ),

      # ------------------------------------------------------------
      # Discards
      # ------------------------------------------------------------
      tabPanel(
        title = "Discards",
        value = "discards",

        layout_sidebar(
          bg = "white",
          fg = "black",

          # Let the whole layout grow when the about box expands.
          fill = FALSE,
          fillable = TRUE,
          gap = "12px",

          sidebar = sidebar(
            width = "20vw",
            bg = "white",
            fg = "black",
            open = TRUE,

            uiOutput(ns("discards_text"))
          ),

          discards_about_box(),

          card(
            # This height belongs to the plot card only.
            height = "85vh",
            fill = FALSE,

            card_header(
              "Discard trends",

              download_icon_label(
                text = "Download data",
                outputId = ns("download_discard_data"),
                hover_text = "Full annual ecoregion data (.csv in .zip)",
                size = "large"
              )
            ),

            card_body(
              min_height = 0,

              uiOutput(
                ns("discard_trends_html"),
                fill = TRUE
              )
            )
          )
        )
      )
    )
  )
}

#' Server logic for landings and discards module
#'
#' This module server powers the \emph{Landings} section of the app. It
#' manages bookmarking of the main tabset, renders header labels and a
#' floating glossary, generates landings and discards plots, and provides
#' downloadable data bundles for both landings and discards.
#'
#' @param id Module id, matching the id used in \code{mod_landings_ui()}.
#' @param cap_year Optional numeric year indicating the data capture or
#'   reference year for landings/discards (reserved for use in labels).
#' @param cap_month Optional numeric month indicating the data capture or
#'   reference month (reserved for use in labels).
#' @param selected_ecoregion A reactive expression returning the currently
#'   selected ecoregion name (character). Used for labelling and to select
#'   the appropriate cached data file.
#' @param shared Shared app data supplied by the parent server.
#' @param bookmark_qs A reactive returning a list of bookmark query-string
#'   parameters (including \code{subtab}) or \code{NULL}. Used to restore
#'   the selected main tab (\code{"landings"} or \code{"discards"}) on load.
#' @param set_subtab A callback function taking a single character argument
#'   (the current subtab id). Used to inform the parent app of tab changes
#'   for bookmarking or other side effects.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Restores the active tab (\code{main_tabset}) from
#'     \code{bookmark_qs()} on first load and reports it via
#'     \code{set_subtab()}.
#'   \item Renders a header showing the current ecoregion and a "Last data
#'     update" label that depends on the active tab.
#'   \item Attaches a floating glossary via
#'     \code{mod_glossary_float_server()}, using entries from the
#'     \code{texts} table (type \code{"glossary"}).
#'   \item Renders explanatory text blocks for landings and discards from
#'     \code{texts} (type \code{"landings_discards"}).
#'   \item For landings:
#'     \itemize{
#'       \item Loads an ecoregion-specific \code{.rda} file (by acronym).
#'       \item Calls \code{plot_catch_trends_plotly()} with parameters
#'         derived from the radio-button selection and cleans series names
#'         for display.
#'       \item Provides a ZIP bundle download (\code{download_landings_data})
#'         containing a CSV export, a disclaimer, and external ICES ZIP
#'         files, using \code{safe_download()} where needed.
#'     }
#'   \item For discards:
#'     \itemize{
#'       \item Reads the selected ecoregion's complete annual RDS snapshot
#'         from \code{app/plot_data} when a download is requested.
#'       \item Displays a pre-generated HTML time-series trends plot in an
#'         iframe that fills the available card body.
#'       \item Provides a ZIP bundle download
#'         (\code{download_discard_data}) containing every row and column
#'         of that snapshot as a CSV, plus a disclaimer.
#'     }
#' }
#'
#' @return No direct return value; the function is called for the side
#'   effects of registering observers, outputs, and download handlers in a
#'   Shiny server context.
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel renderUI
#'   downloadHandler reactive req tags tagList
#' @importFrom plotly renderPlotly ggplotly
#' @importFrom utils write.csv
#' @importFrom icesUtils select_text
#'
#' @export
mod_landings_server <- function(
    id, cap_year, cap_month, selected_ecoregion, shared,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    plot_dir <- app_sys("app/plots")
    plot_index <- utils::read.csv(
      file.path(plot_dir, "plot_index.csv"),
      colClasses = "character",
      na.strings = character()
    )
    plot_report <- utils::read.csv(
      file.path(plot_dir, "plot_export_report.csv"),
      colClasses = "character",
      na.strings = character()
    )
    required_plot_columns <- c("plot_type", "ecoregion_id", "guild_id", "file")
    required_report_columns <- c(required_plot_columns, "status", "reason")
    if (!all(required_plot_columns %in% names(plot_index)) ||
        !all(required_report_columns %in% names(plot_report))) {
      stop("The HTML plot index or export report is missing required columns.")
    }

    discard_index <- plot_index[plot_index$plot_type == "discards", , drop = FALSE]
    discard_report <- plot_report[plot_report$plot_type == "discards", , drop = FALSE]
    if (any(discard_index$guild_id != "") ||
        any(discard_report$guild_id != "") ||
        anyDuplicated(discard_index$ecoregion_id) ||
        anyDuplicated(discard_report$ecoregion_id) ||
        any(!discard_report$status %in% c("exported", "no_data")) ||
        any(discard_report$status == "no_data" & !nzchar(discard_report$reason))) {
      stop("The discard plot index or export report contains invalid selections.")
    }

    exported_discards <- discard_report[discard_report$status == "exported", , drop = FALSE]
    if (nrow(discard_index) != nrow(exported_discards) ||
        !setequal(discard_index$ecoregion_id, exported_discards$ecoregion_id) ||
        !all(discard_index$file[match(exported_discards$ecoregion_id, discard_index$ecoregion_id)] ==
          exported_discards$file)) {
      stop("The discard plot index does not match the successful exports.")
    }

    ################################## bookmarking #########################################
    # This module participates in the global bookmarking via two hooks:
    # - `bookmark_qs`: a reactive list provided by the main server with the
    #   parsed query-string (including $subtab).
    # - `set_subtab()`: a callback into the main server to report *user-driven*
    #   changes of the internal tab state.
    #
    # Restore path:
    # - On first non-null bookmark_qs(), we read the desired subtab.
    # - If it is valid for this module, we wait for the UI to flush, then
    #   select the corresponding tabsetPanel value.
    # - We also call set_subtab() once so the main server can see that the
    #   module has accepted the requested subtab.
    #
    # Report path:
    # - Any later changes to input$tabs_overview (ignoring the initial) are
    #   forwarded upstream via set_subtab(), so the main server can update
    #   the URL hash / desired() state.
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("landings", "discards")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "main_tabset", selected = wanted)
          isolate(set_subtab(wanted))
        }, once = TRUE)
      }
    })

    # REPORT on user changes, skip initial default
    observeEvent(input$main_tabset,
      {
        set_subtab(input$main_tabset)
      },
      ignoreInit = TRUE
    )


    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())
      tags$span(tags$b("ICES ecoregion:"), " ", paste0(selected_ecoregion(), " (", get_ecoregion_acronym(selected_ecoregion()), ")"))
    })

    ################################## header + glossary #########################################
    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "landings"

      date_text <- switch(tab,
        "landings" = "October, 2026",
        "discards" = format(Sys.Date(), "%B %d, %Y"),
        ""
      )

      tagList(
        tags$span(tags$b("Last data update:"), " ", date_text),
        tags$span(" \u00B7 "),
        mod_glossary_float_ui(ns("app_glossary"), link_text = "Glossary", panel_title = "Glossary")
      )
    })

    mod_glossary_float_server(
     "app_glossary",
     terms = reactive({
       df <- select_text(texts, "glossary", NULL) # your texts.rda table
       df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
     })
   )
    
    ################################## Landings plots #########################################

landings_data <- reactive({
  ecoregion <- selected_ecoregion()
  acronym <- get_ecoregion_acronym(ecoregion)
  rda_path <- file.path("data", paste0(acronym, ".rda"))

  e <- new.env(parent = emptyenv())
  load(rda_path, envir = e)
  get(acronym, envir = e)
})

available_guilds <- reactive({
  dat <- landings_data()
  names(dat) <- c(
    "Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
    "Species name", "Species code", "Common name", "Value"
  )

  dat %>%
    dplyr::filter(
      !is.na(`Fisheries guild`),
      `Fisheries guild` != "",
      `Fisheries guild` != "undefined"
    ) %>%
    dplyr::pull(`Fisheries guild`) %>%
    unique() %>%
    sort()
})

output$guild_selector_ui <- renderUI({
  req(input$landings_layer_selector)

  if (identical(input$landings_layer_selector, "Common name")) {
    guilds <- available_guilds()

    selectizeInput(
      ns("selected_guild"),
      "Select fisheries guild:",
      choices = guilds,
      selected = guilds[1],
      multiple = FALSE,
      options = list(placeholder = "Choose a fisheries guild")
    )
  }
})

output$landings_layer <- renderPlotly({
  req(input$landings_layer_selector)

  plotting_params <- list(
    "Common name" = list(n = 10),
    "Fisheries guild" = list(n = 6),
    "Country" = list(n = 8)
  )

  params <- plotting_params[[input$landings_layer_selector]]
  ecoregion <- selected_ecoregion()
  acronym <- get_ecoregion_acronym(ecoregion)

  guild_filter <- NULL
  if (identical(input$landings_layer_selector, "Common name")) {
    req(input$selected_guild)
    guild_filter <- input$selected_guild
  }

  fig <- plot_catch_trends_plotly(
    x = landings_data(),
    type = input$landings_layer_selector,
    line_count = params$n,
    selected_guild = guild_filter,
    dataUpdated = "October, 2026",
    session = session,
    ecoregion = acronym
  )

  for (i in seq_along(fig$x$data)) {
  nm <- fig$x$data[[i]]$name
  if (!is.null(nm) && nm != "Total") {
    fig$x$data[[i]]$name <- sub(",.*$", "", nm)
    fig$x$data[[i]]$name <- gsub("\\(", "", fig$x$data[[i]]$name)
  }
}

  fig
})

    ############################### Download landings data bundle ###############################
    output$download_landings_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("landings_trends_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("landings_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)


        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # ---- Build CSV from cached RDA (include acronym + date) ----
        rda_path <- file.path("data", paste0(acronym, ".rda"))
        e <- new.env(parent = emptyenv())
        load(rda_path, envir = e)
        dat <- get(acronym, envir = e)

        csv_name <- paste0("landings_trends_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # ---- Disclaimer.txt (fixed name, no acronym/date) ----
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer landings trends data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # ---- Extra ICES ZIPs (keep original filenames) ----
        extra_urls <- c(
          "https://www.ices.dk/data/Documents/CatchStats/OfficialNominalCatches.zip",
          "https://www.ices.dk/data/Documents/CatchStats/HistoricalLandings1950-2010.zip"
        )

        downloaded_paths <- character(0)
        for (u in extra_urls) {
          dest_path <- file.path(td, basename(u)) # original filename
          if (safe_download(u, dest_path)) {
            downloaded_paths <- c(downloaded_paths, dest_path)
          } else {
            note <- file.path(td, paste0("MISSING_", tools::file_path_sans_ext(basename(u)), ".txt"))
            writeLines(c(
              paste0("Could not download: ", u),
              "This file was unavailable at the time of packaging."
            ), con = note)
            downloaded_paths <- c(downloaded_paths, note)
          }
        }

        # ---- Zip everything ----
        files_to_zip <- c(csv_path, disc_path, downloaded_paths)

        if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
          zip::zipr(zipfile = file, files = files_to_zip, root = td)
        } else {
          owd <- setwd(td)
          on.exit(setwd(owd), add = TRUE)
          zip::zip(zipfile = file, files = basename(files_to_zip))
        }
      },
      contentType = "application/zip"
    )
    ############### Discards plots ##########################################################

    output$discard_trends_html <- renderUI({
      ecoregion_id <- get_ecoregion_acronym(selected_ecoregion())
      report_row <- discard_report[
        discard_report$ecoregion_id == ecoregion_id,
        ,
        drop = FALSE
      ]

      validate(need(
        nrow(report_row) == 1L,
        "The discard plot selection is missing or ambiguous in the export report."
      ))

      if (report_row$status == "no_data") {
        return(tags$div(
          style = paste(
            "height: 100%; display: flex; align-items: center;",
            "justify-content: center;"
          ),
          "No discards available"
        ))
      }

      index_row <- discard_index[
        discard_index$ecoregion_id == ecoregion_id,
        ,
        drop = FALSE
      ]
      validate(need(
        nrow(index_row) == 1L &&
          identical(index_row$file, report_row$file) &&
          grepl("^discards__[A-Za-z0-9_-]+\\.html$", index_row$file),
        "The discard plot selection is invalid in the export index."
      ))

      plot_path <- file.path(plot_dir, index_row$file)
      validate(need(
        isTRUE(file.exists(plot_path) && file.info(plot_path)$size > 0),
        "The exported discard plot is missing or empty."
      ))

      tags$iframe(
        src = paste0("plots/", index_row$file),
        title = paste("Discard trends for", selected_ecoregion()),
        loading = "lazy",
        style = "width: 100%; height: 100%; border: 0; display: block;"
      )
    })

    ############################### Download discard data bundle ###############################
    output$download_discard_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("discard_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("discard_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)


        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # ---- Build CSV (include acronym + date) ----
        dat <- readRDS(app_sys(paste0("app/plot_data/annual_", acronym, ".rds")))

        csv_name <- paste0("discard_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # ---- Disclaimer.txt (fixed name, no acronym/date) ----
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer discard data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # ---- Zip CSV + Disclaimer ----
        files_to_zip <- c(csv_path, disc_path)

        if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
          zip::zipr(zipfile = file, files = files_to_zip, root = td)
        } else {
          owd <- setwd(td)
          on.exit(setwd(owd), add = TRUE)
          zip::zip(zipfile = file, files = basename(files_to_zip))
        }
      },
      contentType = "application/zip"
    )

    ################################## Sidebar texts #########################################

    output$landings_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("landings_discards_", get_ecoregion_acronym(selected_ecoregion())), "landings"))
      )
    })

    output$discards_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("landings_discards_", get_ecoregion_acronym(selected_ecoregion())), "discards"))
      )
    })
  })
}

    
## To be copied in the UI
# mod_landings_ui("landings_1")
    
## To be copied in the server
# mod_landings_server("landings_1")
