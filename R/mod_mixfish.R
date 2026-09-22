#' Mixed fisheries UI module (tabbed: History of fleet activity / Forecast)
#'
#' This module UI creates the Mixed Fisheries section of fisheriesXplorer,
#' split into two top-level tabs: "History of fleet activity" (previously
#' the "Data" plot group) and "Forecast" (previously the "Analysis" plot
#' group). The case-study (sub-region) selector is available at the top of
#' both tabs.
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

    tabsetPanel(
      id = ns("main_tabset"),

      tabPanel(
        title = "History of fleet activity", value = "history",

        layout_sidebar(
          bg = "white",
          fg = "black",

          sidebar = sidebar(
            width = "33vw",
            bg = "white",
            fg = "black",
            open = FALSE,
            uiOutput(ns("mixfish_text_history"))
          ),

          card(
            height = "85vh",
            full_screen = TRUE,
            fill = FALSE,

            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; gap:12px; width:100%; flex-wrap:wrap;",
                # tags$span("Mixed fisheries - history of fleet activity"),
                radioButtons(
                  ns("subRegion_history"),
                  "Select case study:",
                  choices = character(0),
                  selected = character(0),
                  inline = TRUE
                ),
                download_icon_label(
                    text = "Download data",
                    outputId = ns("download_mixfish_data"),
                    hover_text = "Total mix-fish data (.csv)",
                    size = "large"
                  )
              )
            ),

            card_body(
              fillable = TRUE,
              fill = TRUE,
              class = "p-1",

              selectizeInput(
                inputId = ns("plot_selected_history"),
                label = "Select plot:",
                choices = c(
                  "Landings by métier & stock" = "plot3",
                  "Landings by stock" = "plot4",
                  "Landings composition by fleet" = "plot5"
                ),
                selected = "plot3",
                multiple = FALSE,
                options = list(
                  placeholder = "Choose a plot"
                )
              ),

              uiOutput(ns("filter_ui_history")),

              withSpinner(
                plotlyOutput(ns("plot_history"), height = "75vh"),
                caption = "Getting mix-fish results..."
              )
            )
          )
        )
      ),

      tabPanel(
        title = "Forecast", value = "forecast",

        layout_sidebar(
          bg = "white",
          fg = "black",

          sidebar = sidebar(
            width = "33vw",
            bg = "white",
            fg = "black",
            open = FALSE,
            uiOutput(ns("mixfish_text_forecast"))
          ),

          card(
            height = "85vh",
            full_screen = TRUE,
            fill = FALSE,

            card_header(
              div(
                style = "display:flex; justify-content:space-between; align-items:center; gap:12px; width:100%; flex-wrap:wrap;",
                # tags$span("Mixed fisheries forecasts"),
                radioButtons(
                  ns("subRegion_forecast"),
                  "Select case study:",
                  choices = character(0),
                  selected = character(0),
                  inline = TRUE
                ),
                download_icon_label(
                    text = "Download data",
                    outputId = ns("download_mixfish_data"),
                    hover_text = "Total mix-fish data (.csv)",
                    size = "large"
                  )
              )
            ),

            card_body(
              fillable = TRUE,
              fill = TRUE,
              class = "p-1",

              selectizeInput(
                inputId = ns("plot_selected_forecast"),
                label = "Select plot:",
                choices = c(
                  "Scenarios" = "plot1",
                  "Effort by fleet & stock" = "plot2",
                  "Variation of effort by fleet & stock" = "plot6"
                ),
                selected = "plot1",
                multiple = FALSE,
                options = list(
                  placeholder = "Choose a plot"
                )
              ),

              uiOutput(ns("filter_ui_forecast")),

              withSpinner(
                plotlyOutput(ns("plot_forecast"), height = "75vh"),
                caption = "Getting mix-fish results..."
              )
            )
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
#'   \item ecoregion and case-study selection, shared across both tabs;
#'   \item independent plot selection per tab (History of fleet activity /
#'     Forecast);
#'   \item plot-specific filter UI, per tab;
#'   \item data filtering;
#'   \item plot rendering, per tab.
#' }
#'
#' @param id Module id, matching the id used in \code{mod_mixfish_ui()}.
#' @param selected_ecoregion A reactive returning the selected ICES ecoregion.
#' @param bookmark_qs A reactive list containing query-string values.
#' @param set_subtab Callback used to update the selected subtab upstream.
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

    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()
      wanted <- qs$subtab
      valid <- c("history", "forecast")

      if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
        session$onFlushed(function() {
          updateTabsetPanel(session, "main_tabset", selected = wanted)
          isolate(set_subtab(wanted))
        }, once = TRUE)
      }
    })

    observeEvent(
      input$main_tabset,
      {
        set_subtab(input$main_tabset)
      },
      ignoreInit = TRUE
    )

    ################################## header + glossary #########################################

    output$ecoregion_label <- renderUI({
      req(selected_ecoregion())

      tags$span(
        tags$b("ICES ecoregion:"),
        " ",
        paste0(
          selected_ecoregion(),
          " (",
          get_ecoregion_acronym(selected_ecoregion()),
          ")"
        )
      )
    })

    output$current_date <- renderUI({
      tab <- input$main_tabset
      if (is.null(tab)) tab <- "history"

      date_text <- switch(
        tab,
        "history" = "October, 2025",
        "forecast" = "October, 2025",
        ""
      )

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

        df[
          ,
          intersect(names(df), c("term", "definition", "source")),
          drop = FALSE
        ]
      })
    )

    ################################## Sidebar text (shared, shown in both tabs) ##################################

    mixfish_text_content <- reactive({
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

    output$mixfish_text_history <- renderUI({
      mixfish_text_content()
    })

    output$mixfish_text_forecast <- renderUI({
      mixfish_text_content()
    })



    ################################## Plot selection (independent per tab) ##################################

    filtered_plot_names <- c("plot1", "plot2", "plot3", "plot5")

    plot_name_history <- reactive({
      req(input$plot_selected_history)
      input$plot_selected_history
    })

    plot_name_forecast <- reactive({
      req(input$plot_selected_forecast)
      input$plot_selected_forecast
    })

    ################################## Case-study (sub-region) selection, shared across tabs ##################################

    selected_subRegion <- reactiveVal(NULL)
    region_ready <- reactiveVal(FALSE)

    subregion_choices <- reactive({
      req(selected_ecoregion())

      acr <- get_ecoregion_acronym(selected_ecoregion())

      switch(
        acr,
        "CS" = c("Celtic Sea", "Irish Sea"),
        "BI" = c("Bay of Biscay", "Iberian Waters"),
        selected_ecoregion()
      )
    })

    # keep both card-header radio buttons populated with the available case studies
    observeEvent(
      subregion_choices(),
      {
        choices <- subregion_choices()

        if (is.null(choices)) {
          choices <- character(0)
        }

        sel <- if (!is.null(selected_subRegion()) && selected_subRegion() %in% choices) {
          selected_subRegion()
        } else if (length(choices) > 0) {
          choices[1]
        } else {
          character(0)
        }

        updateRadioButtons(session, "subRegion_history", choices = choices, selected = sel, inline = TRUE)
        updateRadioButtons(session, "subRegion_forecast", choices = choices, selected = sel, inline = TRUE)
      },
      ignoreInit = FALSE
    )

    observeEvent(
      selected_ecoregion(),
      {
        region_ready(FALSE)

        choices <- subregion_choices()

        selected_subRegion(choices[1])

        session$onFlushed(function() {
          region_ready(TRUE)
        }, once = TRUE)
      },
      ignoreInit = FALSE
    )

    # keep the two case-study selectors (one per tab) in sync with each other
    observeEvent(
      input$subRegion_history,
      {
        req(input$subRegion_history)

        if (!identical(input$subRegion_history, selected_subRegion())) {
          selected_subRegion(input$subRegion_history)
          updateRadioButtons(session, "subRegion_forecast", selected = input$subRegion_history)
        }

        region_ready(TRUE)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$subRegion_forecast,
      {
        req(input$subRegion_forecast)

        if (!identical(input$subRegion_forecast, selected_subRegion())) {
          selected_subRegion(input$subRegion_forecast)
          updateRadioButtons(session, "subRegion_history", selected = input$subRegion_forecast)
        }

        region_ready(TRUE)
      },
      ignoreInit = TRUE
    )

    ################################## Data preparation (shared across tabs) ##################################

    data_reactive_all <- reactive({
      req(selected_ecoregion())
      req(region_ready())

      eco_acronym <- get_active_region_acronym(
        selected_subRegion(),
        selected_ecoregion()
      )

      validate(
        need(
          eco_acronym %in% mixfish_data$catchScenarioStk$ecoregion,
          "Invalid ecoregion filter."
        )
      )

      list(
        catchScenarioStk_filtered =
          mixfish_data$catchScenarioStk %>%
          dplyr::filter(ecoregion == eco_acronym),

        catchRange_filtered =
          mixfish_data$catchRange %>%
          dplyr::filter(ecoregion == eco_acronym),

        EffortByFleetStock_filtered =
          mixfish_data$EffortByFleetStock %>%
          dplyr::filter(ecoregion == eco_acronym),

        MetierStockLandings_filtered =
          mixfish_data$MetierStockLandings %>%
          dplyr::filter(ecoregion == eco_acronym),

        StockLandings_filtered =
          mixfish_data$StockLandings %>%
          dplyr::filter(ecoregion == eco_acronym),

        refTable_filtered =
          mixfish_data$refTable %>%
          dplyr::filter(ecoregion == eco_acronym)
      )
    })

    dataComp <- reactive({
      data(stfMtStkSum, package = "mixfishtools")

      list(
        stfMtStkSum = stfMtStkSum
      )
    })

    ################################## Filter helpers ##################################


    filter_source_data_impl <- function(name) {
      switch(
        name,

        "plot1" = data_reactive_all()$catchScenarioStk_filtered,

        "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

        "plot3" = data_reactive_all()$MetierStockLandings_filtered,

        "plot5" = dataComp()$stfMtStkSum,

        NULL
      )
    }

    filter_source_data_history <- reactive({
      req(plot_name_history())
      req(region_ready())

      filter_source_data_impl(plot_name_history())
    })

    filter_source_data_forecast <- reactive({
      req(plot_name_forecast())
      req(region_ready())

      filter_source_data_impl(plot_name_forecast())
    })

    safe_choices <- function(data, var) {
      if (is.null(data)) {
        return(character(0))
      }

      if (!var %in% names(data)) {
        return(character(0))
      }

      choices <- sort(unique(data[[var]]))
      choices <- choices[!is.na(choices)]

      as.character(choices)
    }

    filter_selectize <- function(input_id, label, choices) {
      selectizeInput(
        inputId = ns(input_id),
        label = label,
        choices = choices,
        selected = NULL,
        multiple = TRUE,
        width = "100%",
        options = list(
          plugins = list("remove_button"),
          placeholder = "All"
        )
      )
    }

    apply_optional_filter <- function(data, var, selected_values) {
      if (is.null(selected_values) || length(selected_values) == 0) {
        return(data)
      }

      data %>%
        dplyr::filter(as.character(.data[[var]]) %in% as.character(selected_values))
    }

    filter_has_selection <- function(...) {
      values <- list(...)

      any(vapply(
        values,
        function(x) !is.null(x) && length(x) > 0,
        logical(1)
      ))
    }



    ################################## Dynamic filter UI (History tab) ##################################

    output$filter_ui_history <- renderUI({
      req(plot_name_history())
      req(region_ready())

      df <- filter_source_data_history()

      switch(plot_name_history(),
        "plot3" = div(
          style = "
        margin-bottom: 0px;
        display: flex;
        gap: 4px;
        align-items: flex-start;
        flex-wrap: wrap;
      ",
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot3_stock_filter",
              label = "Fish stock:",
              choices = safe_choices(df, "stock")
            )
          ),
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot3_metier_filter",
              label = "Métier:",
              choices = safe_choices(df, "metier")
            )
          )
        ),
        "plot4" = NULL,
        "plot5" = div(
          style = "
        margin-bottom: 0px;
        display: flex;
        gap: 4px;
        align-items: flex-start;
        flex-wrap: wrap;
      ",
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot5_year_filter",
              label = "Year:",
              choices = safe_choices(df, "year")
            )
          ),
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot5_fleet_filter",
              label = "Fleet:",
              choices = safe_choices(df, "fleet")
            )
          )
        )
      )
    })

    ################################## Dynamic filter UI (Forecast tab) ##################################

    output$filter_ui_forecast <- renderUI({
      req(plot_name_forecast())
      req(region_ready())

      df <- filter_source_data_forecast()

      switch(plot_name_forecast(),
        "plot1" = div(
          style = "
        margin-bottom: 0px;
        display: flex;
        gap: 4px;
        align-items: flex-start;
        flex-wrap: wrap;
      ",
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot1_stock_filter",
              label = "Fish stock:",
              choices = safe_choices(df, "stock")
            )
          )
        ),
        "plot2" = div(
          style = "
        margin-bottom: 0px;
        display: flex;
        gap: 4px;
        align-items: flex-start;
        flex-wrap: wrap;
      ",
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot2_country_filter",
              label = "Country:",
              choices = safe_choices(
                df %>%
                  dplyr::mutate(
                    country = substr(fleet, 1, 2)
                  ),
                "country"
              )
            )
          ),
          div(
            style = "width: 320px;",
            filter_selectize(
              input_id = "plot2_fleet_filter",
              label = "Fleet:",
              choices = safe_choices(df, "fleet")
            )
          )
        ),
        "plot6" = NULL
      )
    })

    ################################## Current plot data (per tab) ##################################

    current_plot_data_history <- reactiveVal(NULL)
    current_plot_data_forecast <- reactiveVal(NULL)

    filter_touched_history <- reactiveValues(
      plot3 = FALSE,
      plot5 = FALSE
    )

    filter_touched_forecast <- reactiveValues(
      plot1 = FALSE,
      plot2 = FALSE
    )

    reset_current_plot_data_history <- function() {
      df <- filter_source_data_history()

      if (!is.null(df) && NROW(df) > 0) {
        current_plot_data_history(df)
      } else {
        current_plot_data_history(NULL)
      }
    }

    reset_current_plot_data_forecast <- function() {
      df <- filter_source_data_forecast()

      if (!is.null(df) && NROW(df) > 0) {
        current_plot_data_forecast(df)
      } else {
        current_plot_data_forecast(NULL)
      }
    }

    observeEvent(
      {
        list(
          plot_name_history(),
          region_ready(),
          selected_subRegion()
        )
      },
      {
        req(plot_name_history())
        req(region_ready())

        filter_touched_history$plot3 <- FALSE
        filter_touched_history$plot5 <- FALSE

        if (plot_name_history() %in% filtered_plot_names) {
          reset_current_plot_data_history()
        } else {
          current_plot_data_history(NULL)
        }
      },
      ignoreInit = FALSE
    )

    observeEvent(
      {
        list(
          plot_name_forecast(),
          region_ready(),
          selected_subRegion()
        )
      },
      {
        req(plot_name_forecast())
        req(region_ready())

        filter_touched_forecast$plot1 <- FALSE
        filter_touched_forecast$plot2 <- FALSE

        if (plot_name_forecast() %in% filtered_plot_names) {
          reset_current_plot_data_forecast()
        } else {
          current_plot_data_forecast(NULL)
        }
      },
      ignoreInit = FALSE
    )

    ################################## Filter observers (History tab) ##################################

    observeEvent(
      {
        list(
          input$plot3_stock_filter,
          input$plot3_metier_filter
        )
      },
      {
        req(plot_name_history() == "plot3")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot3_stock_filter,
          input$plot3_metier_filter
        )

        if (!has_selection && !isTRUE(filter_touched_history$plot3)) {
          return()
        }

        if (has_selection) {
          filter_touched_history$plot3 <- TRUE
        }

        df <- filter_source_data_history()

        df <- df %>%
          apply_optional_filter("stock", input$plot3_stock_filter) %>%
          apply_optional_filter("metier", input$plot3_metier_filter)

        current_plot_data_history(df)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      {
        list(
          input$plot5_year_filter,
          input$plot5_fleet_filter
        )
      },
      {
        req(plot_name_history() == "plot5")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot5_year_filter,
          input$plot5_fleet_filter
        )

        if (!has_selection && !isTRUE(filter_touched_history$plot5)) {
          return()
        }

        if (has_selection) {
          filter_touched_history$plot5 <- TRUE
        }

        df <- filter_source_data_history()

        df <- df %>%
          apply_optional_filter("year", input$plot5_year_filter) %>%
          apply_optional_filter("fleet", input$plot5_fleet_filter)

        current_plot_data_history(df)
      },
      ignoreInit = TRUE
    )

    ################################## Filter observers (Forecast tab) ##################################

    observeEvent(
      {
        list(
          # input$plot1_scenario_filter,
          input$plot1_stock_filter
        )
      },
      {
        req(plot_name_forecast() == "plot1")
        req(region_ready())

        has_selection <- filter_has_selection(
          # input$plot1_scenario_filter,
          input$plot1_stock_filter
        )

        if (!has_selection && !isTRUE(filter_touched_forecast$plot1)) {
          return()
        }

        if (has_selection) {
          filter_touched_forecast$plot1 <- TRUE
        }

        df <- filter_source_data_forecast()

        df <- df %>%
          # apply_optional_filter("scenario", input$plot1_scenario_filter) %>%
          apply_optional_filter("stock", input$plot1_stock_filter)

        current_plot_data_forecast(df)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      {
        list(
          input$plot2_country_filter,
          input$plot2_fleet_filter
        )
      },
      {
        req(plot_name_forecast() == "plot2")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot2_country_filter,
          input$plot2_fleet_filter
          )

        if (!has_selection && !isTRUE(filter_touched_forecast$plot2)) {
          return()
        }

        if (has_selection) {
          filter_touched_forecast$plot2 <- TRUE
        }

        df <- filter_source_data_forecast()

        df <- df %>% dplyr::mutate(country = substr(fleet, 1, 2))

        df <- df %>%
          apply_optional_filter("country", input$plot2_country_filter) %>%
          apply_optional_filter("fleet", input$plot2_fleet_filter)

        current_plot_data_forecast(df)
      },
      ignoreInit = TRUE
    )

    ################################## Plot rendering (History tab) ##################################

    output$plot_history <- renderPlotly({
      req(plot_name_history())
      req(region_ready())

      switch(
        plot_name_history(),

        "plot3" = {
          req(current_plot_data_history())

          plot_landByMetStock_plotly(
            data = current_plot_data_history(),
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot4" = {
          plot_landByStock_plotly(
            data = data_reactive_all()$StockLandings_filtered,
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot5" = {
          req(current_plot_data_history())

          plot_catchComp_plotly(
            dataComposition = current_plot_data_history(),
            refTable = data_reactive_all()$refTable_filtered,
            filters = NULL,
            selectors = "year",
            divider = "fleet",
            yvar = "catch"
          )
        }
      )
    })

    ################################## Plot rendering (Forecast tab) ##################################

    output$plot_forecast <- renderPlotly({
      req(plot_name_forecast())
      req(region_ready())

      switch(
        plot_name_forecast(),

        "plot1" = {
          req(current_plot_data_forecast())

          plot_catchScenStk_plotly(
            data = current_plot_data_forecast(),
            adv = data_reactive_all()$catchRange_filtered,
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot2" = {
          req(current_plot_data_forecast())

          plot_effortFltStk_plotly(
            data = current_plot_data_forecast(),
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot6" = {
          plot_relEffortFltStk_plotly(
            data = data_reactive_all()$EffortByFleetStock_filtered
          )
        }
      )
    })
  })
}