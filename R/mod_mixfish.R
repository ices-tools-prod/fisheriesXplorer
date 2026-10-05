

mixfish_about_box <- function() {
  div(
    # Same wrapper + label as a Shiny input, so the box starts where the dropdown starts
    class = "form-group shiny-input-container",
    style = "flex: 1 1 auto; min-width: 0; width: auto;",

    tags$label(
      class = "control-label",
      style = "visibility: hidden;",
      `aria-hidden` = "true",
      HTML("&nbsp;")
    ),

    tags$details(
      style = "
        width: 100%;
        padding: 0;
        background-color: #eef4f8;
        border: 1px solid #d5e2ea;
        border-radius: 6px;
        overflow: hidden;
      ",
      # 6 + 20 + 6 + 2px border = 34px, same height as the dropdown
      tags$summary(
        style = "
          margin: 0;
          padding: 6px 12px;
          line-height: 20px;
          font-weight: 600;
          cursor: pointer;
          background-color: #dceaf2;
          list-style-position: inside;
        ",
        "About this module"
      ),
      div(
        style = "padding: 12px 16px; background-color: #eef4f8;",
        p(
          style = "margin: 0;",
          "In mixed fisheries, a fishing gear catches multiple species at the same time. ",
          "This can result in unwanted bycatch, as vessels often catch species that they are not targeting. ",
          "Within <ecoregion>, each species has a catch limit (or quota). Fishing must stop when that quota is met. ",
          "Mixed fisheries can become limited by the species with the most restrictive quota, referred to as a choke species. ",
          "In this case, the Maximum Sustainable Yield will not be reached for other species, leading to missed fishing opportunities. ",
          "Mixed fisheries models are used to explore scenarios and estimate how the quota for one species might limit fishing on another across <ecoregion>."
        )
      )
    )
  )
}

mixfish_scenarios_box <- function(ns) {

  tags$details(
    style = "
      width: 100%;
      margin-bottom: 8px;
      padding: 0;
      background-color: #eef4f8;
      border: 1px solid #d5e2ea;
      border-radius: 6px;
      overflow: hidden;
    ",

    tags$summary(
      style = "
        margin: 0;
        padding: 6px 12px;
        line-height: 20px;
        font-weight: 600;
        cursor: pointer;
        background-color: #dceaf2;
        list-style-position: inside;
      ",
      "Scenario definitions"
    ),

    div(
      style = "
        padding: 12px 16px;
        background-color: #eef4f8;
        overflow-x: auto;
      ",

      tableOutput(ns("mixfish_scenarios_table"))
    )
  )
}


mod_mixfish_ui <- function(id) {
  ns <- NS(id)

  tagList(
    mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

    tabsetPanel(
      id = ns("mixfish_tabset"),

      ################################## History ##################################

      tabPanel(
        title = "History of fleet activity",
        value = "history",

        layout_sidebar(
          bg = "white",
          fg = "black",

          sidebar = sidebar(
            width = "20vw",
            bg = "white",
            fg = "black",
            open = TRUE,
            uiOutput(ns("mixfish_text_history"))
          ),

          card(
            height = "85vh",
            full_screen = TRUE,
            fill = FALSE,

            card_header(
              div(
                style = "
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  gap: 12px;
                  width: 100%;
                  flex-wrap: wrap;
                ",

                radioButtons(
                  ns("subRegion_history"),
                  "Select case study:",
                  choices = character(0),
                  selected = character(0),
                  inline = TRUE
                ),

                # download_icon_label(
                #   text = "Download data",
                #   outputId = ns("download_mixfish_data"),
                #   hover_text = "Total mix-fish data (.csv)",
                #   size = "large"
                # )
                download_icon_label(
                  text = "Download data",
                  outputId = ns("download_mixfish_data_history"),
                  hover_text = "Selected plot data (.csv)",
                  size = "large"
                )
              )
            ),

            card_body(
              fillable = TRUE,
              fill = TRUE,
              class = "p-1",

              div(
                style = "
                  display: flex;
                  gap: 12px;
                  align-items: flex-start;
                  width: 100%;
                  margin-bottom: 8px;
                ",
                
                mixfish_about_box(),
                
                # Plot selector
                div(
                  style = "flex: 0 0 300px; width: 300px;",

                  selectizeInput(
                    inputId = ns("plot_selected_history"),
                    label = "Select plot:",
                    choices = c(
                      "Landings by métier & stock" = "plot3",
                      "Landings by stock" = "plot4",
                      "Landings composition by fleet" = "plot5",
                      "Landings alluvial by stock" = "plot6"
                    ),
                    selected = "plot3",
                    multiple = FALSE,
                    width = "100%"
                  )
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

      ################################## Forecast ##################################

      tabPanel(
        title = "Forecast",
        value = "forecast",

        layout_sidebar(
          bg = "white",
          fg = "black",

          sidebar = sidebar(
            width = "20vw",
            bg = "white",
            fg = "black",
            open = TRUE,
            uiOutput(ns("mixfish_text_forecast"))
          ),

          card(
            height = "85vh",
            full_screen = TRUE,
            fill = FALSE,

            card_header(
              div(
                style = "
                  display: flex;
                  justify-content: space-between;
                  align-items: center;
                  gap: 12px;
                  width: 100%;
                  flex-wrap: wrap;
                ",

                radioButtons(
                  ns("subRegion_forecast"),
                  "Select case study:",
                  choices = character(0),
                  selected = character(0),
                  inline = TRUE
                ),

                # download_icon_label(
                #   text = "Download data",
                #   outputId = ns("download_mixfish_data"),
                #   hover_text = "Total mix-fish data (.csv)",
                #   size = "large"
                # )
                download_icon_label(
                  text = "Download data",
                  outputId = ns("download_mixfish_data_forecast"),
                  hover_text = "Selected plot data (.csv)",
                  size = "large"
                )
              )
            ),

            card_body(
              fillable = TRUE,
              fill = TRUE,
              class = "p-1",

              div(
                style = "
                  display: flex;
                  gap: 12px;
                  align-items: flex-start;
                  width: 100%;
                  margin-bottom: 8px;
                ",

                mixfish_about_box(),

                # Plot selector
                div(
                  style = "flex: 0 0 300px; width: 300px;",

                  selectizeInput(
                    inputId = ns("plot_selected_forecast"),
                    label = "Select plot:",
                    choices = c(
                      "Scenarios" = "plot1",
                      "Effort by fleet & stock" = "plot2"
                    ),
                    selected = "plot1",
                    multiple = FALSE,
                    width = "100%",
                    options = list(
                      placeholder = "Choose a plot"
                    )
                  )
                )
              ),
              mixfish_scenarios_box(ns),

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
    set_subtab = function(...) {},
    set_plot = function(...) {}) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns


    ################################## Mapping plots from URL ##################################
    mixfish_plot_from_url <- function(subtab, plot) {
      mappings <- list(
        history = c(
          landings_by_metier_stock = "plot3",
          landings_by_stock = "plot4",
          landings_composition_by_fleet = "plot5",
          landings_alluvial_by_stock = "plot6"
        ),
        forecast = c(
          scenarios = "plot1",
          effort_by_fleet_stock = "plot2"
        )
      )

      x <- mappings[[subtab]][[plot]]

      if (is.null(x)) NULL else x
    }

    ################################## Bookmarking ##################################

    # observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
    #   qs <- bookmark_qs()
    #   wanted <- qs$subtab
    #   valid <- c("history", "forecast")

    #   if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
    #     session$onFlushed(function() {
    #       updateTabsetPanel(session, "main_tabset", selected = wanted)
    #       isolate(set_subtab(wanted))
    #     }, once = TRUE)
    #   }
    # })

    # observeEvent(input$main_tabset, {
    #   set_subtab(input$main_tabset)
    # }, ignoreInit = TRUE)
    observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
      qs <- bookmark_qs()

      wanted_subtab <- qs$subtab
      wanted_plot <- qs$plot
      wanted_subregion <- qs$subregion

      if (wanted_subtab %in% c("history", "forecast")) {
        session$onFlushed(function() {
          updateTabsetPanel(
            session,
            "mixfish_tabset",
            selected = wanted_subtab
          )

          if (!is.null(wanted_subregion) && nzchar(wanted_subregion)) {
            choices <- subregion_choices()

            if (wanted_subregion %in% choices) {
              selected_subRegion(wanted_subregion)

              updateRadioButtons(
                session,
                "subRegion_history",
                selected = wanted_subregion
              )

              updateRadioButtons(
                session,
                "subRegion_forecast",
                selected = wanted_subregion
              )
            }
          }


          plot_id <- NULL

          if (!is.null(wanted_plot) && nzchar(wanted_plot)) {
            plot_id <- mixfish_plot_from_url(
              wanted_subtab,
              wanted_plot
            )
          }

          if (!is.null(plot_id)) {
            if (wanted_subtab == "history") {
              updateSelectizeInput(
                session,
                "plot_selected_history",
                selected = plot_id
              )
            }

            if (wanted_subtab == "forecast") {
              updateSelectizeInput(
                session,
                "plot_selected_forecast",
                selected = plot_id
              )
            }
          }

          isolate(set_subtab(wanted_subtab))
        }, once = TRUE)
      }
    })


    ################################## Header + glossary ##################################

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
        tags$span(" · "),
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

    ################################## Sidebar text ##################################

    # New approach: one row per plot in the texts sheet (section = "plot1", "plot2", ...),
    # looked up using whichever plot is currently selected on each subtab.
    # Uses get_active_region_acronym() (not get_ecoregion_acronym() alone) so
    # that case studies with their own acronym (e.g. Celtic Sea "CSx" vs Irish
    # Sea "IrS") get their own text instead of sharing the parent CS/BI sheet.
    mixfish_text_for_plot <- function(plot_name) {
      req(selected_ecoregion())
      req(region_ready())
      req(plot_name)

      div(
        class = "sidebar-text",
        HTML(
          select_text(
            texts,
            paste0(
              "mixfish_",
              get_active_region_acronym(selected_subRegion(), selected_ecoregion())
            ),
            plot_name
          )
        )
      )
    }

    output$mixfish_text_history <- renderUI({
      mixfish_text_for_plot(plot_name_history())
    })

    output$mixfish_text_forecast <- renderUI({
      mixfish_text_for_plot(plot_name_forecast())
    })

    ################################## Plot selection ##################################

    filtered_plot_names <- c("plot1", "plot2", "plot3")

    plot_name_history <- reactive({
      req(input$plot_selected_history)
      input$plot_selected_history
    })

    plot_name_forecast <- reactive({
      req(input$plot_selected_forecast)
      input$plot_selected_forecast
    })

    ################################## Case-study selection ##################################

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

    observeEvent(subregion_choices(), {
      choices <- subregion_choices()
      if (is.null(choices)) choices <- character(0)

      sel <- if (!is.null(selected_subRegion()) && selected_subRegion() %in% choices) {
        selected_subRegion()
      } else if (length(choices) > 0) {
        choices[1]
      } else {
        character(0)
      }

      updateRadioButtons(
        session, "subRegion_history",
        choices = choices, selected = sel, inline = TRUE
      )

      updateRadioButtons(
        session, "subRegion_forecast",
        choices = choices, selected = sel, inline = TRUE
      )
    }, ignoreInit = FALSE)

    observeEvent(selected_ecoregion(), {
      region_ready(FALSE)

      choices <- subregion_choices()
      selected_subRegion(choices[1])

      session$onFlushed(function() {
        region_ready(TRUE)
      }, once = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$subRegion_history, {
      req(input$subRegion_history)

      if (!identical(input$subRegion_history, selected_subRegion())) {
        selected_subRegion(input$subRegion_history)
        updateRadioButtons(
          session,
          "subRegion_forecast",
          selected = input$subRegion_history
        )
      }

      region_ready(TRUE)
    }, ignoreInit = TRUE)

    observeEvent(input$subRegion_forecast, {
      req(input$subRegion_forecast)

      if (!identical(input$subRegion_forecast, selected_subRegion())) {
        selected_subRegion(input$subRegion_forecast)
        updateRadioButtons(
          session,
          "subRegion_history",
          selected = input$subRegion_forecast
        )
      }

      region_ready(TRUE)
    }, ignoreInit = TRUE)



    ################################## Forecast scenario definitions ##################################

    output$mixfish_scenarios_table <- renderTable({

      req(selected_subRegion())

      df <- select_text(
        texts,
        "mixfish_scenarios",
        NULL
      )

      validate(
        need(
          all(
            c(
              "Scenario",
              "Mixed fisheries effort assumption",
              "Basis for catch limit in forecast year",
              "case_study"
            ) %in% names(df)
          ),
          "Scenario definitions are not available."
        )
      )

      df <- df %>%
        dplyr::filter(
          as.character(case_study) ==
            as.character(selected_subRegion())
        ) %>%
        dplyr::select(
          Scenario,
          `Mixed fisheries effort assumption`,
          `Basis for catch limit in forecast year`
        )

      validate(
        need(
          nrow(df) > 0,
          "No scenario definitions are available for this case study."
        )
      )

      df

    },
    rownames = FALSE,
    striped = TRUE,
    hover = TRUE,
    bordered = FALSE,
    spacing = "s"
    )

    ################################## Data preparation ##################################

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

    ################################## Catch-composition data ##################################

    dataComp <- reactive({
      data(stfMtStkSum, package = "mixfishtools")

      stfMtStkSum <- stfMtStkSum %>%
        dplyr::mutate(
          country = sub("_.*$", "", fleet),
          area = sub("^.*\\.", "", metier),
          area = ifelse(grepl("\\.", metier), area, NA_character_)
        )

      list(stfMtStkSum = stfMtStkSum)
    })

    ################################## Filter helpers ##################################

    filter_source_data_impl <- function(name) {
      switch(
        name,
        "plot1" = data_reactive_all()$catchScenarioStk_filtered,
        "plot2" = data_reactive_all()$EffortByFleetStock_filtered,
        "plot3" = data_reactive_all()$MetierStockLandings_filtered,
        "plot5" = dataComp()$stfMtStkSum,
        "plot6" = dataComp()$stfMtStkSum,
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
      if (is.null(data)) return(character(0))
      if (!var %in% names(data)) return(character(0))

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
      if (is.null(selected_values) || length(selected_values) == 0) return(data)

      data %>%
        dplyr::filter(
          as.character(.data[[var]]) %in% as.character(selected_values)
        )
    }

    filter_has_selection <- function(...) {
      values <- list(...)

      any(vapply(
        values,
        function(x) !is.null(x) && length(x) > 0,
        logical(1)
      ))
    }

    ################################## Catch-composition variables ##################################

    available_comp_vars <- c(
      "Year" = "year",
      "Country" = "country",
      "Fleet" = "fleet",
      "Métier" = "metier",
      "Area" = "area"
    )

    ################################## Dynamic filter UI: History ##################################

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
            gap: 8px;
            align-items: flex-start;
            flex-wrap: wrap;
          ",
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot5_selectors"),
              "X-axis variable(s):",
              choices = available_comp_vars,
              selected = "year",
              multiple = FALSE,
              width = "100%",
              options = list(
                plugins = list("remove_button"),
                placeholder = "Choose variable(s)"
              )
            )
          ),
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot5_divider"),
              "Divide plot by:",
              choices = c("None" = "", available_comp_vars),
              selected = "fleet",
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot5_year_filter"),
              "Filter year:",
              choices = safe_choices(df, "year"),
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                plugins = list("remove_button"),
                placeholder = "All years"
              )
            )
          ),
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot5_country_filter"),
              "Filter country:",
              choices = safe_choices(df, "country"),
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                plugins = list("remove_button"),
                placeholder = "All countries"
              )
            )
          )
        ),
        "plot6" = div(
          style = "
            margin-bottom: 0px;
            display: flex;
            gap: 8px;
            align-items: flex-start;
            flex-wrap: wrap;
          ",
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot6_year_filter"),
              "Year:",
              choices = safe_choices(df, "year"),
              selected = tail(safe_choices(df, "year"), 1),
              multiple = FALSE,
              width = "100%"
            )
          ),
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot6_country_filter"),
              "Country:",
              choices = safe_choices(df, "country"),
              selected = NULL,
              multiple = TRUE,
              width = "100%",
              options = list(
                plugins = list("remove_button"),
                placeholder = "All countries"
              )
            )
          ),
          div(
            style = "width: 260px;",
            selectizeInput(
              ns("plot6_scenario_filter"),
              "Scenario:",
              choices = safe_choices(df, "scenario"),
              selected = if ("min" %in% safe_choices(df, "scenario")) {
                "min"
              } else {
                safe_choices(df, "scenario")[1]
              },
              multiple = FALSE,
              width = "100%"
            )
          )
        )
      )
    })

    ################################## Plot 5 structural inputs ##################################

    plot5_selectors <- reactive({
      req(plot_name_history() == "plot5")
      req(input$plot5_selectors)
      req(length(input$plot5_selectors) > 0)

      input$plot5_selectors
    })

    plot5_divider <- reactive({
      req(plot_name_history() == "plot5")

      divider <- input$plot5_divider

      if (is.null(divider) || length(divider) == 0 || identical(divider, "")) {
        return(NULL)
      }

      divider
    })

    plot5_filters <- reactive({
      req(plot_name_history() == "plot5")

      filters <- list()

      if (!is.null(input$plot5_year_filter) &&
          length(input$plot5_year_filter) > 0) {
        filters$year <- input$plot5_year_filter
      }

      if (!is.null(input$plot5_country_filter) &&
          length(input$plot5_country_filter) > 0) {
        filters$country <- input$plot5_country_filter
      }

      if (length(filters) == 0) return(NULL)

      filters
    })

    ################################## Keep divider different from selectors ##################################

    observeEvent(input$plot5_selectors, {
      req(plot_name_history() == "plot5")

      selectors <- input$plot5_selectors
      if (is.null(selectors)) selectors <- character(0)

      divider_choices <- available_comp_vars[
        !available_comp_vars %in% selectors
      ]

      current_divider <- isolate(input$plot5_divider)

      if (is.null(current_divider) ||
          current_divider %in% selectors ||
          !current_divider %in% divider_choices) {
        current_divider <- ""
      }

      updateSelectizeInput(
        session,
        "plot5_divider",
        choices = c("None" = "", divider_choices),
        selected = current_divider
      )
    }, ignoreInit = TRUE)


    ################################## Plot 6 alluvial data ##################################

    plot6_data <- reactive({
      req(plot_name_history() == "plot6")
      req(input$plot6_year_filter)
      req(input$plot6_scenario_filter)

      df <- dataComp()$stfMtStkSum

      # Year
      df <- df %>%
        dplyr::filter(as.character(year) == as.character(input$plot6_year_filter))

      # Scenario
      df <- df %>%
        dplyr::filter(as.character(scenario) == as.character(input$plot6_scenario_filter))

      # Country: optional; no selection means all countries
      if (!is.null(input$plot6_country_filter) &&
        length(input$plot6_country_filter) > 0) {
        df <- df %>%
          dplyr::filter(country %in% input$plot6_country_filter)
      }

      validate(
        need(nrow(df) > 0, "No data available for the selected filters.")
      )

      # plot_alluvial_plotly expects fleet, metier, stock and value
      df <- df %>%
        dplyr::select(fleet, metier, stock, landings)

      # stfMtStkSum uses the short stock code.
      # Convert it to the stock identifier used by refTable.
      ref <- data_reactive_all()$refTable_filtered

      if ("stock_short" %in% names(ref)) {
        df$stock <- ref$stock[match(df$stock, ref$stock_short)]
      }

      df <- df %>%
        dplyr::filter(!is.na(stock)) %>%
        dplyr::rename(value = landings)

      df
    })

    ################################## Dynamic filter UI: Forecast ##################################

    output$filter_ui_forecast <- renderUI({
      req(plot_name_forecast())
      req(region_ready())

      df <- filter_source_data_forecast()

      switch(
        plot_name_forecast(),

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
        )
      )
    })

    ################################## Current plot data ##################################

    current_plot_data_history <- reactiveVal(NULL)
    current_plot_data_forecast <- reactiveVal(NULL)

    filter_touched_history <- reactiveValues(
      plot3 = FALSE
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

    ################################## Reset current data ##################################

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

    ################################## Filter observers: History ##################################

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

        if (!has_selection &&
            !isTRUE(filter_touched_history$plot3)) {
          return()
        }

        if (has_selection) {
          filter_touched_history$plot3 <- TRUE
        }

        df <- filter_source_data_history()

        df <- df %>%
          apply_optional_filter(
            "stock",
            input$plot3_stock_filter
          ) %>%
          apply_optional_filter(
            "metier",
            input$plot3_metier_filter
          )

        current_plot_data_history(df)
      },
      ignoreInit = TRUE
    )

    # No plot5 observer here.
    # plot5 filters are passed directly to plot_catchComp_plotly()
    # through plot5_filters(), plot5_selectors(), and plot5_divider().

    ################################## Filter observers: Forecast ##################################

    observeEvent(
      {
        list(
          input$plot1_stock_filter
        )
      },
      {
        req(plot_name_forecast() == "plot1")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot1_stock_filter
        )

        if (!has_selection &&
            !isTRUE(filter_touched_forecast$plot1)) {
          return()
        }

        if (has_selection) {
          filter_touched_forecast$plot1 <- TRUE
        }

        df <- filter_source_data_forecast()

        df <- df %>%
          apply_optional_filter(
            "stock",
            input$plot1_stock_filter
          )

        current_plot_data_forecast(df)
      },
      ignoreInit = TRUE
    )

    ################################## Plot 2: conditional Fleet choices ##################################

    observeEvent(input$plot2_country_filter, {
      req(plot_name_forecast() == "plot2")
      req(region_ready())

      df <- filter_source_data_forecast() %>%
        dplyr::mutate(
          country = substr(fleet, 1, 2)
        )

      if (!is.null(input$plot2_country_filter) &&
          length(input$plot2_country_filter) > 0) {
        df <- df %>%
          dplyr::filter(
            country %in% input$plot2_country_filter
          )
      }

      fleet_choices <- sort(unique(df$fleet))
      fleet_choices <- fleet_choices[!is.na(fleet_choices)]

      current_selection <- input$plot2_fleet_filter

      if (!is.null(current_selection)) {
        current_selection <- intersect(
          current_selection,
          fleet_choices
        )
      }

      updateSelectizeInput(
        session,
        "plot2_fleet_filter",
        choices = fleet_choices,
        selected = current_selection,
        server = TRUE
      )
    }, ignoreInit = TRUE)

    ################################## Plot 2: filter plotted data ##################################

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

        if (!has_selection &&
            !isTRUE(filter_touched_forecast$plot2)) {
          return()
        }

        if (has_selection) {
          filter_touched_forecast$plot2 <- TRUE
        }

        df <- filter_source_data_forecast() %>%
          dplyr::mutate(
            country = substr(fleet, 1, 2)
          )

        df <- df %>%
          apply_optional_filter(
            "country",
            input$plot2_country_filter
          ) %>%
          apply_optional_filter(
            "fleet",
            input$plot2_fleet_filter
          )

        current_plot_data_forecast(df)
      },
      ignoreInit = TRUE
    )

    ################################## Plot rendering: History ##################################

    output$plot_history <- renderPlotly({
      req(plot_name_history())
      req(region_ready())

      switch(plot_name_history(),
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
          df <- filter_source_data_history()

          req(!is.null(df))
          req(NROW(df) > 0)

          plot_catchComp_plotly(
            dataComposition = df,
            refTable = data_reactive_all()$refTable_filtered,
            filters = plot5_filters(),
            selectors = plot5_selectors(),
            divider = plot5_divider(),
            yvar = "catch"
          )
        },
        "plot6" = {
          df <- plot6_data()

          plot_alluvial_plotly(
            data = df,
            refTable = data_reactive_all()$refTable_filtered,
            group_vars = c("fleet", "metier", "stock"),
            fill_var = "stock",
            group_labs = c("Fleet", "Métier", "Stock"),
            ylab = "Landings [t]",
            fillLegendTitle = "Stock",
            addLegend = TRUE
          )
        }
      )
    })

    ################################## Plot rendering: Forecast ##################################

    output$plot_forecast <- renderPlotly({
      req(plot_name_forecast())
      req(region_ready())

      switch(
        plot_name_forecast(),

        "plot1" = {
          req(current_plot_data_forecast())
          req(nrow(current_plot_data_forecast()) > 0)

          plot_catchScenStk_plotly(
            data = current_plot_data_forecast(),
            adv = data_reactive_all()$catchRange_filtered,
            refTable = data_reactive_all()$refTable_filtered,
            title = "Mixed-fisheries projections of potential 2026 catches"
          )
        },

        "plot2" = {
          req(current_plot_data_forecast())

          plot_effortFltStk_plotly(
            data = current_plot_data_forecast(),
            refTable = data_reactive_all()$refTable_filtered
          )
        }
      )
    })

    ################################## Download bundles: per-plot data ##################################

    # Resolves the data frame + a short label for whichever plot is currently
    # selected on the History tab, falling back to the unfiltered source data
    # if the user has not touched the optional filters yet.
    export_data_history <- reactive({
      req(plot_name_history())
      req(region_ready())

      switch(
        plot_name_history(),
        "plot3" = {
          df <- current_plot_data_history()
          if (is.null(df)) df <- filter_source_data_history()
          list(data = df, label = "landings_by_metier_stock")
        },
        "plot4" = list(
          data = data_reactive_all()$StockLandings_filtered,
          label = "landings_by_stock"
        ),
        "plot5" = list(
          data = filter_source_data_history(),
          label = "landings_composition_by_fleet"
        ),
        "plot6" = list(
          data = plot6_data(),
          label = "landings_alluvial_by_stock"
        ),
        list(data = NULL, label = "mixfish_history")
      )
    })

    # Same idea for the Forecast tab.
    export_data_forecast <- reactive({
      req(plot_name_forecast())
      req(region_ready())

      switch(
        plot_name_forecast(),
        "plot1" = {
          df <- current_plot_data_forecast()
          if (is.null(df)) df <- filter_source_data_forecast()
          list(data = df, label = "catch_scenarios")
        },
        "plot2" = {
          df <- current_plot_data_forecast()
          if (is.null(df)) df <- filter_source_data_forecast()
          list(data = df, label = "effort_by_fleet_stock")
        },
        list(data = NULL, label = "mixfish_forecast")
      )
    })

    build_mixfish_bundle <- function(file, exp) {
      td <- tempfile("mixfish_bundle_")
      dir.create(td, showWarnings = FALSE)
      on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)

      acronym <- get_ecoregion_acronym(selected_ecoregion())
      date_tag <- format(Sys.Date(), "%d-%b-%y")

      dat <- exp$data
      validate(need(!is.null(dat) && NROW(dat) > 0, "No data available for download."))

      csv_name <- paste0("mixfish_", exp$label, "_", acronym, "_", date_tag, ".csv")
      csv_path <- file.path(td, csv_name)
      utils::write.csv(dat, csv_path, row.names = FALSE)

      disc_path <- file.path(td, "Disclaimer.txt")
      disc_url <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
      if (!safe_download(disc_url, disc_path)) {
        writeLines(c(
          "Disclaimer for fisheriesXplorer mixed-fisheries data.",
          "The official disclaimer could not be fetched automatically.",
          paste("Please see:", disc_url)
        ), con = disc_path)
      }

      files_to_zip <- c(csv_path, disc_path)
      if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
        zip::zipr(zipfile = file, files = files_to_zip, root = td)
      } else {
        owd <- setwd(td)
        on.exit(setwd(owd), add = TRUE)
        zip::zip(zipfile = file, files = basename(files_to_zip))
      }
    }

    output$download_mixfish_data_history <- downloadHandler(
      filename = function() {
        acronym <- get_ecoregion_acronym(selected_ecoregion())
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("mixfish_history_", export_data_history()$label, "_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        build_mixfish_bundle(file, export_data_history())
      },
      contentType = "application/zip"
    )

    output$download_mixfish_data_forecast <- downloadHandler(
      filename = function() {
        acronym <- get_ecoregion_acronym(selected_ecoregion())
        date_tag <- format(Sys.Date(), "%d-%b-%y")
        paste0("mixfish_forecast_", export_data_forecast()$label, "_", acronym, "_", date_tag, ".zip")
      },
      content = function(file) {
        build_mixfish_bundle(file, export_data_forecast())
      },
      contentType = "application/zip"
    )
  })
}