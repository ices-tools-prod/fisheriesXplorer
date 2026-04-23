mod_bycatch_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),
    tabsetPanel(
      id = ns("bycatch_tabset"),

      tabPanel(
        title = "Total bycatch",
        value = "total_bycatch",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw",
            bg = "white",
            fg = "black",
            open = FALSE,
            uiOutput(ns("total_bycatch_text"))
          ),
          column(
            12,
            card(
              height = "85vh",
              full_screen = TRUE,

              card_header(
                div(
                  style = "display:flex; justify-content:space-between; align-items:center; gap:12px; width:100%; flex-wrap:wrap;",
                  radioButtons(
                    ns("bycatch_taxa_selector"),
                    "Select taxon:",
                    choices = c("Fish", "Elasmobranchs", "Seabirds", "Turtles", "Mammals"),
                    selected = "Mammals",
                    inline = TRUE
                  ),
                  download_icon_label(
                    text = "Download data",
                    outputId = ns("download_bycatch_data"),
                    hover_text = "Total bycatch data (.csv)",
                    size = "large"
                  )
                )
              ),

              card_body(
                fillable = TRUE,

                div(
                  style = "margin-bottom: 0px; display: flex; gap: 4px; align-items: flex-start; flex-wrap: wrap;",
                  div(
                    style = "width: 320px;",
                    selectizeInput(
                      ns("bycatch_metier_filter"),
                      "Filter metier:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(placeholder = "All metiers")
                    )
                  ),
                  div(
                    style = "width: 320px;",
                    selectizeInput(
                      ns("bycatch_species_filter"),
                      "Filter species:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(placeholder = "All species")
                    )
                  )
                ),

                withSpinner(
                  plotlyOutput(ns("total_bycatch_plot"), height = "75vh"),
                  caption = "Getting total bycatch data..."
                )
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Bycatch per unit effort (BPUE)",
        value = "bpue",
        layout_sidebar(
          sidebar = sidebar(
            width = "33vw",
            bg = "white",
            fg = "black",
            open = FALSE,
            uiOutput(ns("bpue_text_summary"))
          ),
          column(
            12,
            card(
              height = "85vh",
              full_screen = TRUE,

              card_header(
                div(
                  style = "display:flex; justify-content:space-between; align-items:center; gap:12px; width:100%; flex-wrap:wrap;",
                  radioButtons(
                    ns("bpue_taxa_selector"),
                    "Select taxon:",
                    choices = c("Fish", "Elasmobranchs", "Seabirds", "Turtles", "Mammals"),
                    selected = "Mammals",
                    inline = TRUE
                  ),
                  download_icon_label(
                    text = "Download data",
                    outputId = ns("download_bpue_data"),
                    hover_text = "BPUE data (.csv & plot)",
                    size = "large"
                  )
                )
              ),

              card_body(
                fillable = TRUE,

                div(
                  style = "margin-bottom: 0px; display: flex; gap: 4px; align-items: flex-start; flex-wrap: wrap;",
                  div(
                    style = "width: 320px;",
                    selectizeInput(
                      ns("bpue_metier_filter"),
                      "Filter metier:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(placeholder = "All metiers")
                    )
                  ),
                  div(
                    style = "width: 320px;",
                    selectizeInput(
                      ns("bpue_species_filter"),
                      "Filter species:",
                      choices = NULL,
                      selected = NULL,
                      multiple = TRUE,
                      width = "100%",
                      options = list(placeholder = "All species")
                    )
                  )
                  
                ),
                
                  uiOutput(ns("bpue_plot_note")),
                  

                withSpinner(
                  plotlyOutput(ns("bpue_plot"), height = "75vh"),
                  caption = "Getting BPUE data..."
                )
              )
            )
          )
        )
      )
    )
  )
}


mod_bycatch_server <- function(
    id,
    selected_ecoregion,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # `%||%` <- function(x, y) {
    #   if (is.null(x)) y else x
    # }

    ################################## bookmarking #########################################

    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("total_bycatch", "bpue")

      # if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
      #   session$onFlushed(function() {
      #     if (utils::packageVersion("bslib") >= "0.5.0") {
      #       bslib::nav_select(
      #         id = "bycatch_tabset",
      #         selected = wanted,
      #         session = session
      #       )
      #     } else {
      #       updateTabsetPanel(session, "bycatch_tabset", selected = wanted)
      #     }
      #     isolate(set_subtab(wanted))
      #   }, once = TRUE)
      # }
      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "bycatch_tabset", selected = wanted)
          isolate(set_subtab(wanted))
        }, once = TRUE)
      }
    })

    observeEvent(input$bycatch_tabset,
      {
        set_subtab(input$bycatch_tabset)
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
      date_text <- format(Sys.Date(), "%B %d, %Y")

      tagList(
        tags$span(tags$b("Last data update:"), " ", date_text),
        tags$span(" \u00B7 "),
        mod_glossary_float_ui(
          ns("app_glossary"),
          link_text = "Glossary",
          panel_title = "Glossary"
        )
      )
    })

    mod_glossary_float_server(
      "app_glossary",
      terms = reactive({
        df <- select_text(texts, "glossary", NULL)
        df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
      })
    )

    ################################## base data #########################################

    bycatch_data <- reactive({
      eco <- selected_ecoregion()
      req(eco)

      out <- tryCatch(
        {
          raw <- get_bycatch_ecoregion(eco)
          clean_bycatch_data(raw)
        },
        error = function(e) {
          empty_bycatch_data()
        }
      )

      if (!is.data.frame(out) || !"taxon" %in% names(out)) {
        return(empty_bycatch_data())
      }

      out
    })

    ################################## selected taxa #########################################

    bpue_taxa <- reactive({
      req(input$bpue_taxa_selector)
      input$bpue_taxa_selector
    })

    bycatch_taxa <- reactive({
      req(input$bycatch_taxa_selector)
      input$bycatch_taxa_selector
    })

    ################################## tab-specific base subsets #########################################

    bpue_base_data <- reactive({
      req(bycatch_data(), bpue_taxa())

      bycatch_data() %>%
        dplyr::filter(
          taxon %in% bpue_taxa(),
          !is.na(bpuE_Numeric)
        )
    })

    total_bycatch_base_data <- reactive({
      req(bycatch_data(), bycatch_taxa())

      bycatch_data() %>%
        dplyr::filter(
          taxon %in% bycatch_taxa(),
          !is.na(bycatch_2024)
        )
    })

    ################################## update filter dropdowns #########################################

    observeEvent(bpue_base_data(),
      {
        dat <- bpue_base_data()

        valid_metiers <- sort(unique(dat$metier_L4))
        valid_species <- sort(unique(dat$common_name))

        updateSelectizeInput(
          session = session,
          inputId = "bpue_metier_filter",
          choices = valid_metiers,
          selected = isolate(input$bpue_metier_filter[input$bpue_metier_filter %in% valid_metiers]),
          server = TRUE
        )

        updateSelectizeInput(
          session = session,
          inputId = "bpue_species_filter",
          choices = valid_species,
          selected = isolate(input$bpue_species_filter[input$bpue_species_filter %in% valid_species]),
          server = TRUE
        )
      },
      ignoreInit = FALSE
    )

    observeEvent(total_bycatch_base_data(),
      {
        dat <- total_bycatch_base_data()

        valid_metiers <- sort(unique(dat$metier_L4))
        valid_species <- sort(unique(dat$common_name))

        updateSelectizeInput(
          session = session,
          inputId = "bycatch_metier_filter",
          choices = valid_metiers,
          selected = isolate(input$bycatch_metier_filter[input$bycatch_metier_filter %in% valid_metiers]),
          server = TRUE
        )

        updateSelectizeInput(
          session = session,
          inputId = "bycatch_species_filter",
          choices = valid_species,
          selected = isolate(input$bycatch_species_filter[input$bycatch_species_filter %in% valid_species]),
          server = TRUE
        )
      },
      ignoreInit = FALSE
    )

    ################################## fully filtered data #########################################

    bpue_filtered_data <- reactive({
      dat <- bpue_base_data()

      metier_sel <- input$bpue_metier_filter %||% character(0)
      species_sel <- input$bpue_species_filter %||% character(0)

      if (length(metier_sel) > 0) {
        dat <- dat %>%
          dplyr::filter(metier_L4 %in% metier_sel)
      }

      if (length(species_sel) > 0) {
        dat <- dat %>%
          dplyr::filter(common_name %in% species_sel)
      }

      dat
    })

    total_bycatch_filtered_data <- reactive({
      dat <- total_bycatch_base_data()

      metier_sel <- input$bycatch_metier_filter %||% character(0)
      species_sel <- input$bycatch_species_filter %||% character(0)

      if (length(metier_sel) > 0) {
        dat <- dat %>%
          dplyr::filter(metier_L4 %in% metier_sel)
      }

      if (length(species_sel) > 0) {
        dat <- dat %>%
          dplyr::filter(common_name %in% species_sel)
      }

      dat
    })

    ################################## plots #########################################

    output$bpue_plot <- renderPlotly({
      req(bpue_filtered_data(), bpue_taxa())

      validate(
        need(nrow(bpue_filtered_data()) > 0, "No BPUE data available for the current filters.")
      )

      plot_bpue_interactive(
        df = bpue_filtered_data(),
        taxon = bpue_taxa(),
        ecoregion = selected_ecoregion()
      )
    })

    output$total_bycatch_plot <- renderPlotly({
      req(total_bycatch_filtered_data(), bycatch_taxa())

      validate(
        need(nrow(total_bycatch_filtered_data()) > 0, "No bycatch data available for the current filters.")
      )

      plot_bycatch_interactive(
        df = total_bycatch_filtered_data(),
        taxon = bycatch_taxa(),
        ecoregion = selected_ecoregion()
      )
    })

    ################################## text summaries #########################################
    output$bpue_plot_note <- renderUI({
      tagList(
        tags$p(
          "Note: Consider filtering to resolve better the y-axis scale."
        )
      )
    })
    
    output$total_bycatch_text <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("bycatch_", get_ecoregion_acronym(selected_ecoregion())), "total_bycatch"))
      )
    })

    output$bpue_text_summary <- renderUI({
      div(
        class = "sidebar-text",
      HTML(select_text(texts, paste0("bycatch_", get_ecoregion_acronym(selected_ecoregion())), "bpue"))
      )
    })

    
    ######################### Download bycatch data ##########################################
    output$download_bycatch_data <- output$download_bpue_data <- downloadHandler(
      filename = function() {
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("bycatch_data_bundle_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        # --- Temp workspace
        td <- tempfile("bycatch_data_bundle_")
        dir.create(td, showWarnings = FALSE)
        on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

        

        # --- Naming tokens
        ecoregion <- selected_ecoregion()
        acronym <- get_ecoregion_acronym(ecoregion)
        date_tag <- format(Sys.Date(), "%d-%b-%y")

        # --- 1) CSV (includes acronym + date)
        dat <- bycatch_data()
        csv_name <- paste0("bycatch_data_", acronym, "_", date_tag, ".csv")
        csv_path <- file.path(td, csv_name)
        utils::write.csv(dat, csv_path, row.names = FALSE)

        # --- 2) Disclaimer.txt (fixed name; no acronym/date)
        disc_path <- file.path(td, "Disclaimer.txt")
        disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
        if (!safe_download(disc_url, disc_path)) {
          writeLines(c(
            "Disclaimer for fisheriesXplorer bycatch data.",
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
    # output$download_bpue_data <- downloadHandler(
    #   filename = function() {
    #     paste0(
    #       "bpue_",
    #       gsub("[^A-Za-z0-9]+", "_", selected_ecoregion()),
    #       "_",
    #       Sys.Date(),
    #       ".csv"
    #     )
    #   },
    #   content = function(file) {
    #     readr::write_csv(bpue_filtered_data(), file)
    #   }
    # )

    # output$download_bycatch_data <- downloadHandler(
    #   filename = function() {
    #     paste0(
    #       "bycatch_",
    #       gsub("[^A-Za-z0-9]+", "_", selected_ecoregion()),
    #       "_",
    #       Sys.Date(),
    #       ".csv"
    #     )
    #   },
    #   content = function(file) {
    #     readr::write_csv(total_bycatch_filtered_data(), file)
    #   }
    # )
  })
}