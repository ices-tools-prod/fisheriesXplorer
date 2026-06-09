#' Mixed fisheries UI module
#'
#' This module UI creates the Mixed Fisheries section of fisheriesXplorer.
#' It uses a sidebar for explanatory text and a main card containing:
#' plot selection, case-study selection where relevant, dynamic filters,
#' and the selected plot.
#'
#' @param id A character string used as the module namespace.
#'
#' @return A Shiny UI tag list.
#'
#' @export
mod_mixfish_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

    layout_sidebar(
      bg = "white",
      fg = "black",

      sidebar = sidebar(
        width = "33vw",
        bg = "white",
        fg = "black",
        open = FALSE,
        uiOutput(ns("mixfish_text"))
      ),

      card(
        height = "85vh",
        full_screen = TRUE,
        fill = FALSE,

        card_header("Mixed fisheries forecasts"),

        card_body(
          fillable = TRUE,
          fill = TRUE,
          class = "p-1",

          uiOutput(ns("subregion_ui")),

          selectizeInput(
            inputId = ns("plot_selected"),
            label = "Select plot:",
            choices = list(
              "Data" = c(
                "Landings by métier & stock" = "plot3",
                "Landings by stock" = "plot4",
                "Landings composition by fleet" = "plot5"
              ),
              "Analysis" = c(
                "Scenarios" = "plot1",
                "Effort by fleet & stock" = "plot2",
                "Variation of effort by fleet & stock" = "plot6"
              )
            ),
            selected = "plot1",
            multiple = FALSE,
            options = list(
              placeholder = "Choose a plot"
            )
          ),

          uiOutput(ns("filter_ui")),

          withSpinner(
            plotlyOutput(ns("plot"), height = "75vh"),
            caption = "Getting mix-fish results..."
          )
        )
      )
    )
  )
}



#' Server logic for the mixed fisheries module
#'
#' This module server manages:
#' \itemize{
#'   \item ecoregion and case-study selection;
#'   \item grouped plot selection;
#'   \item plot-specific filter UI;
#'   \item data filtering;
#'   \item plot rendering.
#' }
#'
#' @param id Module id, matching the id used in \code{mod_mixfish_ui()}.
#' @param selected_ecoregion A reactive returning the selected ICES ecoregion.
#'
#' @return No direct return value.
#'
#' @export
mod_mixfish_server <- function(
    id, 
    selected_ecoregion,
    bookmark_qs = reactive(NULL),
    set_subtab = function(...) {}) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

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
        "landings" = "October, 2025",
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

    ################################## Sidebar text ##################################

    output$mixfish_text <- renderUI({
      req(selected_ecoregion())

      div(
        class = "sidebar-text",
        HTML(
          select_text(
            texts,
            paste0("mixfish_", get_ecoregion_acronym(selected_ecoregion())),
            "overview"
          )
        )
      )
    })

    ################################## Plot and subregion selection ##################################

    selected_subRegion <- reactiveVal(NULL)

    plot_name <- reactive({
      req(input$plot_selected)
      input$plot_selected
    })

    output$subregion_ui <- renderUI({
      req(selected_ecoregion())

      acr <- get_ecoregion_acronym(selected_ecoregion())

      if (acr %in% c("CS", "BI")) {
        new_choices <- switch(
          acr,
          "CS" = c("Celtic Sea", "Irish Sea"),
          "BI" = c("Bay of Biscay", "Iberian Waters")
        )

        selected_subRegion(new_choices[1])

        selectInput(
          inputId = ns("subRegion"),
          label = "Select case study:",
          choices = new_choices,
          selected = new_choices[1]
        )
      } else {
        selected_subRegion(NULL)
        NULL
      }
    })

    observeEvent(input$subRegion, {
      selected_subRegion(input$subRegion)
    })

    ################################## Data filtering ##################################

    data_reactive_all <- reactive({
      req(selected_ecoregion())

      eco_acronym <- get_active_region_acronym(
        selected_subRegion(),
        selected_ecoregion()
      )

      validate(
        need(
          eco_acronym %in% catchScenarioStk$ecoregion,
          "Invalid ecoregion filter."
        )
      )

      list(
        catchScenarioStk_filtered =
          catchScenarioStk %>%
          dplyr::filter(ecoregion == eco_acronym),

        catchRange_filtered =
          catchRange %>%
          dplyr::filter(ecoregion == eco_acronym),

        EffortByFleetStock_filtered =
          EffortByFleetStock %>%
          dplyr::filter(ecoregion == eco_acronym),

        MetierStockLandings_filtered =
          MetierStockLandings %>%
          dplyr::filter(ecoregion == eco_acronym),

        StockLandings_filtered =
          StockLandings %>%
          dplyr::filter(ecoregion == eco_acronym),

        refTable_filtered =
          refTable %>%
          dplyr::filter(ecoregion == eco_acronym)
      )
    })

    dataComp <- reactive({
      req(plot_name())

      data(stfMtStkSum, package = "mixfishtools")

      list(
        stfMtStkSum = stfMtStkSum
      )
    })

    ################################## Dynamic filter UI ##################################

    ui_rendered <- reactiveVal(FALSE)
    data_filter_module <- reactiveVal(NULL)

    observeEvent(plot_name(), {
      ui_rendered(FALSE)
      data_filter_module(NULL)
    })

    output$filter_ui <- renderUI({
      req(plot_name())

      ui <- switch(
        plot_name(),

        "plot1" = datamods::select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            scenario = list(
              inputId = "scenario",
              label = "Management scenario:",
              placeholder = "Select scenario"
            ),
            stock = list(
              inputId = "stock",
              label = "Fish stock:",
              placeholder = "Select stock"
            )
          )
        ),

        "plot2" = datamods::select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            fleet = list(
              inputId = "fleet",
              label = "Fleet:",
              placeholder = "Select fleet"
            )
          )
        ),

        "plot3" = datamods::select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            stock = list(
              inputId = "stock",
              label = "Fish stock:",
              placeholder = "Select stock"
            ),
            metier = list(
              inputId = "metier",
              label = "Métier:",
              placeholder = "Select métier"
            )
          )
        ),

        "plot4" = NULL,

        "plot5" = datamods::select_group_ui(
          label = NULL,
          id = ns("my-filters-mixfish"),
          params = list(
            year = list(
              inputId = "year",
              label = "Year:",
              placeholder = "Select year"
            ),
            fleet = list(
              inputId = "fleet",
              label = "Fleet:",
              placeholder = "Select fleet"
            )
          )
        ),
        "plot6" =  NULL
      )

      if (!identical(plot_name(), "plot4")) {
        shinyjs::delay(100, ui_rendered(TRUE))
      }

      ui
    })

    observeEvent(ui_rendered(), {
      req(ui_rendered())
      req(plot_name())

      data_filter_module(
        datamods::select_group_server(
          id = "my-filters-mixfish",

          data_r = reactive({
            req(plot_name())

            switch(
              plot_name(),

              "plot1" = data_reactive_all()$catchScenarioStk_filtered,

              "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

              "plot3" = data_reactive_all()$MetierStockLandings_filtered,

              "plot5" = dataComp()$stfMtStkSum,

              "plot6" = data_reactive_all()$EffortByFleetStock_filtered
            )
          }),

          vars_r = reactive({
            req(plot_name())

            switch(
              plot_name(),

              "plot1" = c("scenario", "stock"),

              "plot2" = c("fleet"),

              "plot3" = c("stock", "metier"),

              "plot5" = c("year", "fleet"),

              "plot6" = c("fleet")
            )
          })
        )
      )
    })

    ################################## Plot rendering ##################################

    output$plot <- renderPlotly({
      req(plot_name())

      if (!identical(plot_name(), "plot4")) {
        req(data_filter_module())
      }

      switch(
        plot_name(),

        "plot1" = plot_catchScenStk_plotly(
          data = data_filter_module()(),
          adv = data_reactive_all()$catchRange_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),

        "plot2" = plot_effortFltStk_plotly(
          data = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered
        ),

        "plot3" = plot_landByMetStock_plotly(
          data = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered
        ),

        "plot4" = plot_landByStock_plotly(
          data = data_reactive_all()$StockLandings_filtered,
          refTable = data_reactive_all()$refTable_filtered
        ),

        "plot5" = plot_catchComp_plotly(
          dataComposition = data_filter_module()(),
          refTable = data_reactive_all()$refTable_filtered,
          filters = NULL,
          selectors = "year",
          divider = "fleet",
          yvar = "catch"
        ),
        "plot6" = plot_relEffortFltStk_plotly(
          data = data_filter_module()()
      )
      )
    })
  })
}