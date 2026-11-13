status_about_box <- function() {
  tab_about_box(
    title = "About this module",

    shiny::p(
      style = "margin: 0;",
      "Fishing pressure is assessed as fishing mortality (F) or F proxies, relative to the lowest F ",
      "reference point or proxy reference point (Fref) available in the latest ICES stock assessment. ",
      "Stock status is assessed using biomass indices (B) or proxies relative to the highest B reference point ",
      "or proxy reference point (Bref) available in the latest ICES stock assessment. ",
      "For stocks for which only Blim is defined, Bref is considered unknown. For details ",
      "and examples related to the definition of Fref and Bref, see [WKBoBFO report]. ",br(),
      "Status information is provided for ICES assessed stocks fished within the ecoregion, ",
      "including stocks that are also fished in other ecoregions. All data shown are based on the ",
      "latest ICES assessments."
    )
  )
}
#' Stock status UI module
#'
#' This module UI creates the main \emph{Stock status} section of the app.
#' It shows summary status plots, trends by functional group, Kobe/CLD
#' views, and a stock-level lookup table, all for the currently selected
#' ecoregion.
#'
#' @param id A character string used as the module namespace. Passed to
#'   \code{shiny::NS()} and used to namespace all UI elements within the
#'   module.
#'
#' @details
#' The layout consists of:
#' \itemize{
#'   \item A flexible header created by \code{mod_flex_header_ui()},
#'     displaying the ecoregion label and current date.
#'   \item A \code{bslib::navset_tab()} (\code{main_tabset}) with four
#'     \code{nav_panel()} views:
#'     \itemize{
#'       \item \strong{Status Summary} (\code{"status_summary"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text1}).
#'           \item One full-width HTML plot (\code{status_summary_html}),
#'             selected with \code{status_summary_selector}: Stocks
#'             (stock summary) or Catches (GES).
#'           \item The selected view's full current-data download
#'             (\code{download_clean_status_data},
#'             \code{download_status_catch_data}).
#'         }
#'       \item \strong{Trends by group} (\code{"trends_by_group"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text2}).
#'           \item A card with a radio-button selector
#'             (\code{status_trend_selector}) for functional groups
#'             (elasmobranch, benthic, shellfish, demersal, pelagic),
#'             a full annual-data download link (\code{download_trends_data}), and a
#'             full-width HTML trends plot (\code{status_trends}).
#'         }
#'       \item \strong{Kobe-CLD} (\code{"kobe_cld"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text3}).
#'           \item Controls for group selection
#'             (\code{status_kobe_cld_selector}), including All Stocks,
#'             plus a full current-data download link
#'             (\code{download_CLD_data}).
#'           \item A full-width HTML Kobe diagram
#'             (\code{status_kobe_html}) for the selected ecoregion and guild.
#'         }
#'       \item \strong{Stock status Lookup} (\code{"status_lookup"}):
#'         \itemize{
#'           \item Sidebar with explanatory text (\code{status_text4}).
#'           \item A card with a \code{reactable} table
#'             (\code{stock_status_table_reactable}) and a CSV download
#'             link (\code{download_status_table}).
#'         }
#'     }
#' }
#'
#' The corresponding server logic is implemented in
#' \code{mod_stock_status_server()}.
#'
#' @return A \code{shiny.tag.list} representing the stock status UI,
#'   suitable for inclusion in a Shiny application.
#'
#' @importFrom shiny NS tagList fluidRow column uiOutput downloadLink
#'   radioButtons plotOutput
#' @importFrom bslib navset_tab nav_panel layout_sidebar sidebar card
#'   card_header card_body
#' @importFrom shinycssloaders withSpinner
#' @importFrom reactable reactableOutput
#'
#' @export
mod_stock_status_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

    # Give the navset an id; give each nav_panel a stable value
    navset_tab(
      id = ns("main_tabset"),
      nav_panel(
        "Summary",
        value = "status_summary",
        layout_sidebar(
          sidebar = sidebar(
            width = "20vw", bg = "white", fg = "black",
            open = TRUE,
            uiOutput(ns("status_text_summary"))
          ),

          # Main area: introduction first, then the plot card.
          status_about_box(),

          card(
            height = "85vh", full_screen = TRUE,
            card_header(
              radioButtons(
                ns("status_summary_selector"), "Show:",
                choices = c("Stocks" = "stock_summary", "Catches" = "ges"),
                selected = "stock_summary", inline = TRUE
              ),
              shiny::conditionalPanel(
                condition = "input.status_summary_selector === 'stock_summary'",
                ns = ns,
                download_icon_label(
                  text = "Download data",
                  outputId = ns("download_clean_status_data"),
                  hover_text = "Full current ecoregion data (.csv in .zip)",
                  size = "large"
                )
              ),
              shiny::conditionalPanel(
                condition = "input.status_summary_selector === 'ges'",
                ns = ns,
                download_icon_label(
                  text = "Download data",
                  outputId = ns("download_status_catch_data"),
                  hover_text = "Full current ecoregion data (.csv in .zip)",
                  size = "large"
                )
              )
            ),
            card_body(
              style = "overflow-y: hidden;",
              uiOutput(ns("status_summary_html"), fill = TRUE)
            )
          )
        )
      ),
      nav_panel(
        "Trends",
        value = "trends_by_group",
        layout_sidebar(
          sidebar = sidebar(
            width = "20vw", bg = "white", fg = "black",
            open = TRUE,
            uiOutput(ns("status_text_trends"))
          ),
          # column(
            # 12,
            status_about_box(),
            card(
              height = "100vh", full_screen = TRUE,
              card_header(
                radioButtons(ns("status_trend_selector"), "Select a fisheries guild:",
                  inline = TRUE,
                  choices = c(
                    "All guilds" = "All",
                    "Benthic" = "benthic",
                    "Demersal" = "demersal",
                    "Elasmobranchs" = "elasmobranch",
                    "Pelagic" = "pelagic",
                    "Shellfish" = "shellfish"
                  )
                ),
                download_icon_label(
                  text = "Download data",
                  outputId = ns("download_trends_data"),
                  hover_text = "Full annual ecoregion data (.csv in .zip)",
                  size = "large"
                )
              ),
              card_body(
                style = "overflow-y: hidden;",
                uiOutput(ns("status_trends"), fill = TRUE)
              )
            )
          # )
        )
      ),
      nav_panel(
        "Kobe plot",
        value = "kobe_cld",
        layout_sidebar(
          sidebar = sidebar(
            width = "20vw", bg = "white", fg = "black", open = TRUE,
            uiOutput(ns("status_cld_trends"))
          ),
          status_about_box(),
          card(
            height = "85vh", full_screen = TRUE,
            card_header(
              radioButtons(ns("status_kobe_cld_selector"), "Select a fisheries guild:",
                inline = TRUE,
                choices = c(
                  "All Stocks" = "All",
                  "Benthic" = "benthic",
                  "Demersal" = "demersal",
                  "Elasmobranchs" = "elasmobranch",
                  "Pelagic" = "pelagic",
                  "Shellfish" = "shellfish"
                ),
                selected = "All"
              ),
              download_icon_label(
                text = "Download data",
                outputId = ns("download_CLD_data"),
                hover_text = "Full current ecoregion data (.csv in .zip)",
                size = "large"
              )
            ),
            card_body(
              style = "overflow-y: hidden;",
              uiOutput(ns("status_kobe_html"), fill = TRUE)
            )
          )
        )
      ),
      nav_panel(
        "Stock list",
        value = "status_lookup", # <-- NEW value
        layout_sidebar(
          sidebar = sidebar(
            width = "20vw", 
            bg = "white", 
            fg = "black",
            open = FALSE,
            uiOutput(ns("status_text4"))
          ),
          status_about_box(),
          card(
            card_header(
              "Stock status table",
              download_icon_label(
                text = "Download data",
                outputId = ns("download_status_table"),
                hover_text = "Stock status list (.csv)",
                size = "large"
              )
            ),
            card_body(withSpinner(reactableOutput(ns("stock_status_table_reactable")))),
            tagList(
              tags$span(
                style = "display:none;",
                textOutput(ns("selected_stock_js"))
              ),
              conditionalPanel(
                condition = sprintf("output['%s'] && output['%s'] !== ''", ns("selected_stock_js"), ns("selected_stock_js")),
                actionLink(ns("clear_stock"), "Clear stock filter", class = "fx-actionlink")
              )
            )
          )
        )
      )
    )
  )
}

        
    
#' Server logic for stock status module
#'
#' This module server powers the \emph{Stock status} section of the app. It
#' manages bookmarking of the main status tabs, renders header labels and the
#' floating glossary, computes derived status objects, generates plots for all
#' four views (summary, trends, Kobe/CLD, lookup), and provides downloadable
#' ZIP bundles for each type of status output.
#'
#' @param id Module id, matching the id used in \code{mod_stock_status_ui()}.
#' @param cap_year Integer (optional) capture/reference year for the status
#'   data, used in captions and filenames where available.
#' @param cap_month Integer (optional) capture/reference month for the status
#'   data, used in captions and filenames where available.
#' @param selected_ecoregion A reactive expression returning the currently
#'   selected ecoregion name (character). Used for labelling, filtering, and
#'   in file naming tokens.
#' @param shared A list or environment containing shared data objects, typically
#'   including \code{SID}, which supplies descriptive stock metadata to
#'   \code{format_annex_table()}.
#' @param bookmark_qs A reactive returning a list of bookmark query-string
#'   parameters (including \code{subtab}) or \code{NULL}. Used to restore the
#'   selected main tab (\code{"status_summary"}, \code{"trends_by_group"},
#'   \code{"kobe_cld"}, \code{"status_lookup"}) on load.
#' @param set_subtab A callback function taking a single character argument
#'   (the current subtab id). Used to inform the parent app of tab changes for
#'   bookmarking or other side effects.
#'
#' @details
#' The module:
#' \itemize{
#'   \item Restores the active tab from \code{bookmark_qs()} on first load,
#'     using \code{bslib::nav_select()} when available, and reports it via
#'     \code{set_subtab()}.
#'   \item Observes changes to \code{input$main_tabset} (ignoring the initial
#'     default) and calls \code{set_subtab()} whenever the user switches tab.
#'   \item Renders a header showing:
#'     \itemize{
#'       \item The current ecoregion label (\code{ecoregion_label}).
#'       \item A "Last data update" timestamp (current system date).
#'       \item A floating glossary trigger wired up via
#'         \code{mod_glossary_float_server()}, populated from the
#'         \code{texts} table (type \code{"glossary"}).
#'     }
#'   \item Uses a common sidebar text block for all four tabs, taken from
#'     \code{select_text(texts, "status", "sidebar")}.
#'   \item For the \strong{Status Summary} tab:
#'     \itemize{
#'       \item Displays an indexed stock-summary or GES HTML in a full-width
#'         iframe, according to \code{status_summary_selector}, only when
#'         the Summary tab is active. Documented no-data selections show
#'         the export report's reason.
#'       \item Provides ZIP bundles for clean status data
#'         (\code{download_clean_status_data}) and GES/catch status
#'         (\code{download_status_catch_data}), each containing the selected
#'         ecoregion's complete current RDS data as a CSV and a disclaimer
#'         fetched via \code{safe_download()}.
#'     }
#'   \item For the \strong{Trends by group} tab:
#'     \itemize{
#'       \item Displays an indexed HTML trends plot for the selected ecoregion
#'         and guild when the tab is active, mapping All guilds to
#'         \code{all_guilds}. Documented no-data selections show their reason.
#'       \item Exposes a ZIP download (\code{download_trends_data}) with the
#'         selected ecoregion's complete annual RDS data as a CSV and a
#'         disclaimer.
#'     }
#'   \item For the \strong{Kobe-CLD} tab:
#'     \itemize{
#'       \item Displays an indexed HTML Kobe plot for the selected ecoregion
#'         and guild when the tab is active, mapping All Stocks to
#'         \code{all_stocks}. Documented no-data selections show their reason.
#'       \item Provides a ZIP bundle (\code{download_CLD_data}) containing
#'         the selected ecoregion's complete current RDS data as a CSV and
#'         a disclaimer.
#'     }
#'   \item For the \strong{Stock status lookup} tab:
#'     \itemize{
#'       \item Enriches the current regional TAF snapshot with SID metadata
#'         and saved annual assessment identities via \code{format_annex_table()}.
#'       \item Renders a \code{reactable} table with stock links, illustrations,
#'         and reference-point names coloured by \code{FStatus} and \code{BStatus}.
#'       \item Exposes a ZIP bundle (\code{download_status_table}) with the
#'         enriched regional data as a CSV and a disclaimer.
#'     }
#' }
#'
#' @return No direct return value; the function is called for its side
#'   effects of registering observers, reactives, outputs, and download
#'   handlers in a Shiny server context.
#'
#' @importFrom shiny moduleServer observeEvent updateTabsetPanel renderUI
#'   reactive req tags tagList downloadHandler
#' @importFrom bslib nav_select
#' @importFrom reactable renderReactable
#' @importFrom utils write.csv
#' @importFrom dplyr filter slice_max mutate select
#' @importFrom tidyr pivot_wider
#' @importFrom icesUtils select_text
#'
#' @export 
mod_stock_status_server <- function(
    id, cap_year, 
    cap_month, 
    selected_ecoregion, 
    shared,
    selected_stock,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    summary_catalog <- html_plot_catalog()

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
      valid <- c("status_summary", "trends_by_group", "kobe_cld", "status_lookup")
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          if (utils::packageVersion("bslib") >= "0.5.0") {
            bslib::nav_select(id = "main_tabset", selected = wanted, session = session)
          } else {
            updateTabsetPanel(session, "main_tabset", selected = wanted)
          }
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


    ################################## header + glossary #########################################
    
    output$ecoregion_label <- renderUI({
      eco <- selected_ecoregion()
      req(eco)

      acr <- get_ecoregion_acronym(eco)

      if (is.na(acr)) {
        tags$span(
          tags$b("ICES ecoregion:"),
          " ",
          eco,
          tags$span(" (not available in fisheriesXplorer)", class = "text-danger")
        )
      } else {
        tags$span(
          tags$b("ICES ecoregion:"),
          " ",
          paste0(eco, " (", acr, ")")
        )
      }
    })

    
    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "landings"
      
      date_text <- format(Sys.Date(), "%B %d, %Y")

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
    

    

    ######################### Status summary tab #################################################


    output$status_summary_html <- renderUI({
      req(input$main_tabset == "status_summary", selected_ecoregion())
      plot_type <- input$status_summary_selector
      req(plot_type %in% c("stock_summary", "ges"))
      region <- get_ecoregion_acronym(selected_ecoregion())
      validate(need(
        length(region) == 1L && !is.na(region),
        "The selected ecoregion has no HTML plot identifier."
      ))
      html_plot_frame(
        summary_catalog, plot_type, region,
        paste(if (plot_type == "stock_summary") "Stock summary for" else "Catch status for",
          selected_ecoregion())
      )
    })

    
    output$download_clean_status_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
       

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (with acronym + date)
        csv_name <- paste0("status_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        dat <- readRDS(app_sys(paste0("app/plot_data/current_", acronym, ".rds")))
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip everything
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

    ############################### GES download ##################################################
    output$download_status_catch_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_catch_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # Temp workspace
        td <- tempfile("status_catch_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # 1) CSV (includes acronym + date)
        dat <- readRDS(app_sys(paste0("app/plot_data/current_", acronym, ".rds")))
        csv_name <- paste0("status_catch_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # 2) Disclaimer.txt (fixed name)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status & catch data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # Zip bundle
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


    ##################### Stock trends tab ###############################################

    output$status_trends <- renderUI({
      req(input$main_tabset == "trends_by_group", selected_ecoregion())
      guild <- input$status_trend_selector
      req(guild %in% c("All", "benthic", "demersal", "elasmobranch", "pelagic", "shellfish"))
      region <- get_ecoregion_acronym(selected_ecoregion())
      validate(need(
        length(region) == 1L && !is.na(region),
        "The selected ecoregion has no HTML plot identifier."
      ))
      html_plot_frame(
        summary_catalog, "status_trends", region,
        paste("Status trends for", selected_ecoregion(), "-", if (guild == "All") "All guilds" else guild),
        guild_id = if (guild == "All") "all_guilds" else guild
      )
    })

    ######################### Download stock trends data ##########################################
    output$download_trends_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_trends_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_trends_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (includes acronym + date)
        dat <- readRDS(app_sys(paste0("app/plot_data/annual_", acronym, ".rds")))
        csv_name <- paste0("status_trends_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer trends data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip bundle
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

    ######################### Kobe-CLD tab ################################################
    output$status_kobe_html <- renderUI({
      req(input$main_tabset == "kobe_cld", selected_ecoregion())
      guild <- input$status_kobe_cld_selector
      req(guild %in% c("All", "benthic", "demersal", "elasmobranch", "pelagic", "shellfish"))
      region <- get_ecoregion_acronym(selected_ecoregion())
      validate(need(
        length(region) == 1L && !is.na(region),
        "The selected ecoregion has no HTML plot identifier."
      ))
      html_plot_frame(
        summary_catalog, "kobe", region,
        paste("Kobe plot for", selected_ecoregion(), "-", if (guild == "All") "All Stocks" else guild),
        guild_id = if (guild == "All") "all_stocks" else guild
      )
    })

    ######################### CLD/Kobe download ################################################
    output$download_CLD_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_CLD_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_CLD_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (with acronym + date)
        dat <- readRDS(app_sys(paste0("app/plot_data/current_", acronym, ".rds")))
        csv_name <- paste0("status_CLD_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer CLD/Kobe data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip everything
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
    ##################### Stock status lookup tab ######################################################

    stock_table_data <- reactive({
      req(selected_ecoregion(), is.data.frame(shared$SID))
      acronym <- get_ecoregion_acronym(selected_ecoregion())
      current <- readRDS(app_sys(paste0("app/plot_data/current_", acronym, ".rds")))
      annual <- readRDS(app_sys(paste0("app/plot_data/annual_", acronym, ".rds")))
      format_annex_table(current = current, sid = shared$SID, annual = annual)
    })

    processed_data_reactable <- reactive({
      req(input$main_tabset == "status_lookup")
      annex_data <- stock_table_data()

      sk <- selected_stock() %|?% ""
      if (nzchar(sk)) {
        annex_data <- annex_data %>% dplyr::filter(
          as.character(F_AssessmentKey) == sk | as.character(B_AssessmentKey) == sk
        )
      }
      annex_data %>%
        dplyr::mutate(
          icon = paste0("<img src='", paste0("www/fish/", match_stockcode_to_illustration(StockKeyLabel, .)), "' height=30>"),
          stock_url = paste0(
            "https://ices-taf.shinyapps.io/advicexplorer/?assessmentkey=",
            utils::URLencode(dplyr::coalesce(AssessmentKey, ""), reserved = TRUE),
            "&assessmentcomponent=",
            utils::URLencode(dplyr::coalesce(AssessmentComponent, ""), reserved = TRUE)
          ),
          StockKeyLabel = dplyr::if_else(
            !is.na(AssessmentKey) & nzchar(AssessmentKey),
            paste0(
              "<a href='", htmltools::htmlEscape(stock_url, attribute = TRUE),
              "' target='_blank' rel='noopener noreferrer'>",
              htmltools::htmlEscape(StockKeyLabel), "</a>"
            ),
            htmltools::htmlEscape(StockKeyLabel)
          )
        ) %>%
        dplyr::select(
          "Stock code (_component)" = StockKeyLabel,
          " " = icon,
          "Stock Description" = StockKeyDescription,
          "Scientific Name" = SpeciesScientificName,
          "Common Name" = SpeciesCommonName,
          "Fisheries Guild" = FisheriesGuild,
          "Data Category" = DataCategory,
          "Assessment Year" = AssessmentYear,
          "Advice Category" = AdviceCategory,
          Fref_name, Bref_name, FStatus, BStatus
        )
    })

    
    ##################################### Stock status table display #################################
    output$stock_status_table_reactable <- renderReactable({      
      req(input$main_tabset == "status_lookup")
      df <- processed_data_reactable()

      if (is.null(df) || nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(Note = "No stocks available for the current selection."),
          searchable = FALSE, filterable = FALSE, pagination = FALSE
        ))
      }

      status_colours <- c(GREEN = "#00B26D", RED = "#d93b1c", GREY = "#d3d3d3")
      status_style <- function(status) {
        background <- status_colours[status]
        if (is.na(background)) background <- status_colours["GREY"]
        list(
          backgroundColor = unname(background),
          color = if (isTRUE(status == "RED")) "#FFFFFF" else "#000000"
        )
      }

      reactable::reactable(
        df,
        filterable = TRUE,
        defaultPageSize = 150,
        resizable = TRUE,
        wrap = TRUE,
        bordered = TRUE,
        columns = list(
          "Stock code (_component)" = reactable::colDef(html = TRUE, filterable = TRUE),
          " " = reactable::colDef(html = TRUE, filterable = FALSE, style = list(textAlign = "center")),
          Fref_name = reactable::colDef(
            name = "Fishing pressure (Fref)", na = "Unknown",
            align = "center", minWidth = 150,
            style = function(value, index) status_style(df$FStatus[[index]])
          ),
          Bref_name = reactable::colDef(
            name = "Stock status (Bref)", na = "Unknown",
            align = "center", minWidth = 150,
            style = function(value, index) status_style(df$BStatus[[index]])
          ),
          FStatus = reactable::colDef(show = FALSE),
          BStatus = reactable::colDef(show = FALSE)
        )
      )
    })

    output$selected_stock_js <- renderText({
      selected_stock() %|?% ""
    })
    outputOptions(output, "selected_stock_js", suspendWhenHidden = FALSE)

    observeEvent(input$clear_stock, {
      selected_stock("") # show full list again
    })

    ######################### Stock status table download ##############################################
    output$download_status_table <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("status_table_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("status_table_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (with acronym + date)
        csv_name <- paste0("status_table_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        dat <- stock_table_data()
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer status table data.",
            "The official disclaimer could not be fetched automatically.",
            paste("Please see:", disc_url)
          ), con = disc_path)
        }

        # --- Zip everything
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
 
    output$status_text_summary <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("status_", get_ecoregion_acronym(selected_ecoregion())), "summary"))
      )
    })
    output$status_text_trends <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("status_", get_ecoregion_acronym(selected_ecoregion())), "trends"))    
      )
    })
    output$status_cld_trends <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("status_", get_ecoregion_acronym(selected_ecoregion())), "cld"))    
      )
    })
  })


  
}

    
## To be copied in the UI
# mod_stock_status_ui("stock_status_1")
    
## To be copied in the server
# mod_stock_status_server("stock_status_1")



