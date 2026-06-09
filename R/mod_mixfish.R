# #' Mixed fisheries UI module
# #'
# #' This module UI creates the Mixed Fisheries section of fisheriesXplorer.
# #' It uses a sidebar for explanatory text and a main card containing:
# #' plot selection, case-study selection where relevant, dynamic filters,
# #' and the selected plot.
# #'
# #' @param id A character string used as the module namespace.
# #'
# #' @return A Shiny UI tag list.
# #'
# #' @export
# mod_mixfish_ui <- function(id) {
#   ns <- NS(id)

#   tagList(
#     mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

#     layout_sidebar(
#       bg = "white",
#       fg = "black",

#       sidebar = sidebar(
#         width = "33vw",
#         bg = "white",
#         fg = "black",
#         open = FALSE,
#         uiOutput(ns("mixfish_text"))
#       ),

#       card(
#         height = "85vh",
#         full_screen = TRUE,
#         fill = FALSE,

#         card_header("Mixed fisheries forecasts"),

#         card_body(
#           fillable = TRUE,
#           fill = TRUE,
#           class = "p-1",

#           uiOutput(ns("subregion_ui")),

#           selectizeInput(
#             inputId = ns("plot_selected"),
#             label = "Select plot:",
#             choices = list(
#               "Data" = c(
#                 "Landings by métier & stock" = "plot3",
#                 "Landings by stock" = "plot4",
#                 "Landings composition by fleet" = "plot5"
#               ),
#               "Analysis" = c(
#                 "Scenarios" = "plot1",
#                 "Effort by fleet & stock" = "plot2",
#                 "Variation of effort by fleet & stock" = "plot6"
#               )
#             ),
#             selected = "plot1",
#             multiple = FALSE,
#             options = list(
#               placeholder = "Choose a plot"
#             )
#           ),

#           uiOutput(ns("filter_ui")),

#           withSpinner(
#             plotlyOutput(ns("plot"), height = "75vh"),
#             caption = "Getting mix-fish results..."
#           )
#         )
#       )
#     )
#   )
# }



# #' Server logic for the mixed fisheries module
# #'
# #' This module server manages:
# #' \itemize{
# #'   \item ecoregion and case-study selection;
# #'   \item grouped plot selection;
# #'   \item plot-specific filter UI;
# #'   \item data filtering;
# #'   \item plot rendering.
# #' }
# #'
# #' @param id Module id, matching the id used in \code{mod_mixfish_ui()}.
# #' @param selected_ecoregion A reactive returning the selected ICES ecoregion.
# #'
# #' @return No direct return value.
# #'
# #' @export
# mod_mixfish_server <- function(
#     id, 
#     selected_ecoregion,
#     bookmark_qs = reactive(NULL),
#     set_subtab = function(...) {}) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     ################################## bookmarking #########################################
#     # This module participates in the global bookmarking via two hooks:
#     # - `bookmark_qs`: a reactive list provided by the main server with the
#     #   parsed query-string (including $subtab).
#     # - `set_subtab()`: a callback into the main server to report *user-driven*
#     #   changes of the internal tab state.
#     #
#     # Restore path:
#     # - On first non-null bookmark_qs(), we read the desired subtab.
#     # - If it is valid for this module, we wait for the UI to flush, then
#     #   select the corresponding tabsetPanel value.
#     # - We also call set_subtab() once so the main server can see that the
#     #   module has accepted the requested subtab.
#     #
#     # Report path:
#     # - Any later changes to input$tabs_overview (ignoring the initial) are
#     #   forwarded upstream via set_subtab(), so the main server can update
#     #   the URL hash / desired() state.
#     observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
#       qs <- bookmark_qs()
#       wanted <- qs$subtab
#       valid <- c("landings", "discards")
#       if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
#         session$onFlushed(function() {
#           updateTabsetPanel(session, "main_tabset", selected = wanted)
#           isolate(set_subtab(wanted))
#         }, once = TRUE)
#       }
#     })

#     # REPORT on user changes, skip initial default
#     observeEvent(input$main_tabset,
#       {
#         set_subtab(input$main_tabset)
#       },
#       ignoreInit = TRUE
#     )


#     output$ecoregion_label <- renderUI({
#       req(selected_ecoregion())
#       tags$span(tags$b("ICES ecoregion:"), " ", paste0(selected_ecoregion(), " (", get_ecoregion_acronym(selected_ecoregion()), ")"))
#     })

#     ################################## header + glossary #########################################
#     output$current_date <- renderUI({
#       tab <- input$main_tabset
#       if (is.null(tab)) tab <- "landings"

#       date_text <- switch(tab,
#         "landings" = "October, 2025",
#         "discards" = format(Sys.Date(), "%B %d, %Y"),
#         ""
#       )

#       tagList(
#         tags$span(tags$b("Last data update:"), " ", date_text),
#         tags$span(" \u00B7 "),
#         mod_glossary_float_ui(ns("app_glossary"), link_text = "Glossary", panel_title = "Glossary")
#       )
#     })

#     mod_glossary_float_server(
#      "app_glossary",
#      terms = reactive({
#        df <- select_text(texts, "glossary", NULL) # your texts.rda table
#        df[, intersect(names(df), c("term", "definition", "source")), drop = FALSE]
#      })
#    )

#     ################################## Sidebar text ##################################

#     output$mixfish_text <- renderUI({
#       req(selected_ecoregion())

#       div(
#         class = "sidebar-text",
#         HTML(
#           select_text(
#             texts,
#             paste0("mixfish_", get_ecoregion_acronym(selected_ecoregion())),
#             "overview"
#           )
#         )
#       )
#     })

#     ################################## Plot and subregion selection ##################################

#     selected_subRegion <- reactiveVal(NULL)

#     plot_name <- reactive({
#       req(input$plot_selected)
#       input$plot_selected
#     })

#     output$subregion_ui <- renderUI({
#       req(selected_ecoregion())

#       acr <- get_ecoregion_acronym(selected_ecoregion())

#       if (acr %in% c("CS", "BI")) {
#         new_choices <- switch(
#           acr,
#           "CS" = c("Celtic Sea", "Irish Sea"),
#           "BI" = c("Bay of Biscay", "Iberian Waters")
#         )

#         selected_subRegion(new_choices[1])

#         selectInput(
#           inputId = ns("subRegion"),
#           label = "Select case study:",
#           choices = new_choices,
#           selected = new_choices[1]
#         )
#       } else {
#         selected_subRegion(NULL)
#         NULL
#       }
#     })

#     observeEvent(input$subRegion, {
#       selected_subRegion(input$subRegion)
#     })

#     ################################## Data filtering ##################################

#     data_reactive_all <- reactive({
#       req(selected_ecoregion())

#       eco_acronym <- get_active_region_acronym(
#         selected_subRegion(),
#         selected_ecoregion()
#       )

#       validate(
#         need(
#           eco_acronym %in% catchScenarioStk$ecoregion,
#           "Invalid ecoregion filter."
#         )
#       )

#       list(
#         catchScenarioStk_filtered =
#           catchScenarioStk %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         catchRange_filtered =
#           catchRange %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         EffortByFleetStock_filtered =
#           EffortByFleetStock %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         MetierStockLandings_filtered =
#           MetierStockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         StockLandings_filtered =
#           StockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         refTable_filtered =
#           refTable %>%
#           dplyr::filter(ecoregion == eco_acronym)
#       )
#     })

#     dataComp <- reactive({
#       req(plot_name())

#       data(stfMtStkSum, package = "mixfishtools")

#       list(
#         stfMtStkSum = stfMtStkSum
#       )
#     })

#     ################################## Dynamic filter UI ##################################

#     ui_rendered <- reactiveVal(FALSE)
#     data_filter_module <- reactiveVal(NULL)

#     observeEvent(plot_name(), {
#       ui_rendered(FALSE)
#       data_filter_module(NULL)
#     })

#     output$filter_ui <- renderUI({
#       req(plot_name())

#       ui <- switch(
#         plot_name(),

#         "plot1" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             scenario = list(
#               inputId = "scenario",
#               label = "Management scenario:",
#               placeholder = "Select scenario"
#             ),
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             )
#           )
#         ),

#         "plot2" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),

#         "plot3" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             ),
#             metier = list(
#               inputId = "metier",
#               label = "Métier:",
#               placeholder = "Select métier"
#             )
#           )
#         ),

#         "plot4" = NULL,

#         "plot5" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             year = list(
#               inputId = "year",
#               label = "Year:",
#               placeholder = "Select year"
#             ),
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),
#         "plot6" =  NULL
#       )

#       if (!identical(plot_name(), "plot4")) {
#         shinyjs::delay(100, ui_rendered(TRUE))
#       }

#       ui
#     })

#     observeEvent(ui_rendered(), {
#       req(ui_rendered())
#       req(plot_name())

#       data_filter_module(
#         datamods::select_group_server(
#           id = "my-filters-mixfish",

#           data_r = reactive({
#             req(plot_name())

#             switch(
#               plot_name(),

#               "plot1" = data_reactive_all()$catchScenarioStk_filtered,

#               "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

#               "plot3" = data_reactive_all()$MetierStockLandings_filtered,

#               "plot5" = dataComp()$stfMtStkSum,

#               "plot6" = data_reactive_all()$EffortByFleetStock_filtered
#             )
#           }),

#           vars_r = reactive({
#             req(plot_name())

#             switch(
#               plot_name(),

#               "plot1" = c("scenario", "stock"),

#               "plot2" = c("fleet"),

#               "plot3" = c("stock", "metier"),

#               "plot5" = c("year", "fleet"),

#               "plot6" = c("fleet")
#             )
#           })
#         )
#       )
#     })

#     ################################## Plot rendering ##################################

#     output$plot <- renderPlotly({
#       req(plot_name())

#       if (!identical(plot_name(), "plot4")) {
#         req(data_filter_module())
#       }

#       switch(
#         plot_name(),

#         "plot1" = plot_catchScenStk_plotly(
#           data = data_filter_module()(),
#           adv = data_reactive_all()$catchRange_filtered,
#           refTable = data_reactive_all()$refTable_filtered
#         ),

#         "plot2" = plot_effortFltStk_plotly(
#           data = data_filter_module()(),
#           refTable = data_reactive_all()$refTable_filtered
#         ),

#         "plot3" = plot_landByMetStock_plotly(
#           data = data_filter_module()(),
#           refTable = data_reactive_all()$refTable_filtered
#         ),

#         "plot4" = plot_landByStock_plotly(
#           data = data_reactive_all()$StockLandings_filtered,
#           refTable = data_reactive_all()$refTable_filtered
#         ),

#         "plot5" = plot_catchComp_plotly(
#           dataComposition = data_filter_module()(),
#           refTable = data_reactive_all()$refTable_filtered,
#           filters = NULL,
#           selectors = "year",
#           divider = "fleet",
#           yvar = "catch"
#         ),
#         "plot6" = plot_relEffortFltStk_plotly(
#           data = data_filter_module()()
#       )
#       )
#     })
#   })
# }


# #' Mixed fisheries UI module
# #'
# #' This module UI creates the Mixed Fisheries section of fisheriesXplorer.
# #' It uses a sidebar for explanatory text and a main card containing:
# #' plot selection, case-study selection where relevant, dynamic filters,
# #' and the selected plot.
# #'
# #' @param id A character string used as the module namespace.
# #'
# #' @return A Shiny UI tag list.
# #'
# #' @export
# mod_mixfish_ui <- function(id) {
#   ns <- NS(id)

#   tagList(
#     mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

#     layout_sidebar(
#       bg = "white",
#       fg = "black",

#       sidebar = sidebar(
#         width = "33vw",
#         bg = "white",
#         fg = "black",
#         open = FALSE,
#         uiOutput(ns("mixfish_text"))
#       ),

#       card(
#         height = "85vh",
#         full_screen = TRUE,
#         fill = FALSE,

#         card_header("Mixed fisheries forecasts"),

#         card_body(
#           fillable = TRUE,
#           fill = TRUE,
#           class = "p-1",

#           uiOutput(ns("subregion_ui")),

#           selectizeInput(
#             inputId = ns("plot_selected"),
#             label = "Select plot:",
#             choices = list(
#               "Data" = c(
#                 "Landings by métier & stock" = "plot3",
#                 "Landings by stock" = "plot4",
#                 "Landings composition by fleet" = "plot5"
#               ),
#               "Analysis" = c(
#                 "Scenarios" = "plot1",
#                 "Effort by fleet & stock" = "plot2",
#                 "Variation of effort by fleet & stock" = "plot6"
#               )
#             ),
#             selected = "plot1",
#             multiple = FALSE,
#             options = list(
#               placeholder = "Choose a plot"
#             )
#           ),

#           uiOutput(ns("filter_ui")),

#           withSpinner(
#             plotlyOutput(ns("plot"), height = "75vh"),
#             caption = "Getting mix-fish results..."
#           )
#         )
#       )
#     )
#   )
# }



# #' Server logic for the mixed fisheries module
# #'
# #' This module server manages:
# #' \itemize{
# #'   \item ecoregion and case-study selection;
# #'   \item grouped plot selection;
# #'   \item plot-specific filter UI;
# #'   \item data filtering;
# #'   \item plot rendering.
# #' }
# #'
# #' @param id Module id, matching the id used in \code{mod_mixfish_ui()}.
# #' @param selected_ecoregion A reactive returning the selected ICES ecoregion.
# #' @param bookmark_qs A reactive list containing query-string values.
# #' @param set_subtab Callback used to update the selected subtab upstream.
# #'
# #' @return No direct return value.
# #'
# #' @export
# mod_mixfish_server <- function(
#     id,
#     selected_ecoregion,
#     bookmark_qs = reactive(NULL),
#     set_subtab = function(...) {}) {

#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     ################################## bookmarking #########################################

#     observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
#       qs <- bookmark_qs()
#       wanted <- qs$subtab
#       valid <- c("landings", "discards")

#       if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
#         session$onFlushed(function() {
#           updateTabsetPanel(session, "main_tabset", selected = wanted)
#           isolate(set_subtab(wanted))
#         }, once = TRUE)
#       }
#     })

#     observeEvent(
#       input$main_tabset,
#       {
#         set_subtab(input$main_tabset)
#       },
#       ignoreInit = TRUE
#     )

#     ################################## header + glossary #########################################

#     output$ecoregion_label <- renderUI({
#       req(selected_ecoregion())

#       tags$span(
#         tags$b("ICES ecoregion:"),
#         " ",
#         paste0(
#           selected_ecoregion(),
#           " (",
#           get_ecoregion_acronym(selected_ecoregion()),
#           ")"
#         )
#       )
#     })

#     output$current_date <- renderUI({
#       tab <- input$main_tabset
#       if (is.null(tab)) tab <- "landings"

#       date_text <- switch(
#         tab,
#         "landings" = "October, 2025",
#         "discards" = format(Sys.Date(), "%B %d, %Y"),
#         ""
#       )

#       tagList(
#         tags$span(tags$b("Last data update:"), " ", date_text),
#         tags$span(" \u00B7 "),
#         mod_glossary_float_ui(
#           ns("app_glossary"),
#           link_text = "Glossary",
#           panel_title = "Glossary"
#         )
#       )
#     })

#     mod_glossary_float_server(
#       "app_glossary",
#       terms = reactive({
#         df <- select_text(texts, "glossary", NULL)

#         df[
#           ,
#           intersect(names(df), c("term", "definition", "source")),
#           drop = FALSE
#         ]
#       })
#     )

#     ################################## Sidebar text ##################################

#     output$mixfish_text <- renderUI({
#       req(selected_ecoregion())

#       div(
#         class = "sidebar-text",
#         HTML(
#           select_text(
#             texts,
#             paste0("mixfish_", get_ecoregion_acronym(selected_ecoregion())),
#             "overview"
#           )
#         )
#       )
#     })

#     ################################## Plot and subregion selection ##################################

#     selected_subRegion <- reactiveVal(NULL)

#     filtered_plot_names <- c("plot1", "plot2", "plot3", "plot5")
#     unfiltered_plot_names <- c("plot4", "plot6")

#     plot_name <- reactive({
#       req(input$plot_selected)
#       input$plot_selected
#     })

#     subregion_choices <- reactive({
#       req(selected_ecoregion())

#       acr <- get_ecoregion_acronym(selected_ecoregion())

#       switch(
#         acr,
#         "CS" = c("Celtic Sea", "Irish Sea"),
#         "BI" = c("Bay of Biscay", "Iberian Waters"),
#         NULL
#       )
#     })

#     output$subregion_ui <- renderUI({
#       choices <- subregion_choices()

#       if (is.null(choices)) {
#         return(NULL)
#       }

#       selectInput(
#         inputId = ns("subRegion"),
#         label = "Select case study:",
#         choices = choices,
#         selected = choices[1]
#       )
#     })

#     observeEvent(
#       selected_ecoregion(),
#       {
#         choices <- subregion_choices()

#         if (is.null(choices)) {
#           selected_subRegion(NULL)
#         } else {
#           selected_subRegion(choices[1])
#         }
#       },
#       ignoreInit = FALSE
#     )

#     observeEvent(
#       input$subRegion,
#       {
#         selected_subRegion(input$subRegion)
#       },
#       ignoreInit = TRUE
#     )

#     ################################## Data filtering ##################################

#     data_reactive_all <- reactive({
#       req(selected_ecoregion())

#       eco_acronym <- get_active_region_acronym(
#         selected_subRegion(),
#         selected_ecoregion()
#       )

#       validate(
#         need(
#           eco_acronym %in% catchScenarioStk$ecoregion,
#           "Invalid ecoregion filter."
#         )
#       )

#       list(
#         catchScenarioStk_filtered =
#           catchScenarioStk %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         catchRange_filtered =
#           catchRange %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         EffortByFleetStock_filtered =
#           EffortByFleetStock %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         MetierStockLandings_filtered =
#           MetierStockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         StockLandings_filtered =
#           StockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         refTable_filtered =
#           refTable %>%
#           dplyr::filter(ecoregion == eco_acronym)
#       )
#     })

#     dataComp <- reactive({
#       req(plot_name())

#       data(stfMtStkSum, package = "mixfishtools")

#       list(
#         stfMtStkSum = stfMtStkSum
#       )
#     })

#     ################################## Dynamic filter UI ##################################

#     filters_ready <- reactiveVal(FALSE)

#     observeEvent(
#       plot_name(),
#       {
#         filters_ready(FALSE)
#       },
#       ignoreInit = FALSE
#     )

#     output$filter_ui <- renderUI({
#       req(plot_name())

#       ui <- switch(
#         plot_name(),

#         "plot1" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             scenario = list(
#               inputId = "scenario",
#               label = "Management scenario:",
#               placeholder = "Select scenario"
#             ),
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             )
#           )
#         ),

#         "plot2" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),

#         "plot3" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             ),
#             metier = list(
#               inputId = "metier",
#               label = "Métier:",
#               placeholder = "Select métier"
#             )
#           )
#         ),

#         "plot4" = NULL,

#         "plot5" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             year = list(
#               inputId = "year",
#               label = "Year:",
#               placeholder = "Select year"
#             ),
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),

#         "plot6" = NULL
#       )

#       if (plot_name() %in% filtered_plot_names) {
#         session$onFlushed(function() {
#           filters_ready(TRUE)
#         }, once = TRUE)
#       } else {
#         filters_ready(TRUE)
#       }

#       ui
#     })

#     filtered_data <- datamods::select_group_server(
#       id = "my-filters-mixfish",

#       data_r = reactive({
#         req(plot_name())

#         switch(
#           plot_name(),

#           "plot1" = data_reactive_all()$catchScenarioStk_filtered,

#           "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

#           "plot3" = data_reactive_all()$MetierStockLandings_filtered,

#           "plot5" = dataComp()$stfMtStkSum,

#           data.frame()
#         )
#       }),

#       vars_r = reactive({
#         req(plot_name())

#         switch(
#           plot_name(),

#           "plot1" = c("scenario", "stock"),

#           "plot2" = c("fleet"),

#           "plot3" = c("stock", "metier"),

#           "plot5" = c("year", "fleet"),

#           character(0)
#         )
#       })
#     )

#     plot_data <- reactive({
#       req(plot_name())

#       if (plot_name() %in% filtered_plot_names) {
#         req(filters_ready())

#         df <- filtered_data()

#         req(!is.null(df))
#         req(NROW(df) > 0)

#         return(df)
#       }

#       NULL
#     })

#     ################################## Plot rendering ##################################

#     output$plot <- renderPlotly({
#       req(plot_name())

#       switch(
#         plot_name(),

#         "plot1" = {
#           plot_catchScenStk_plotly(
#             data = plot_data(),
#             adv = data_reactive_all()$catchRange_filtered,
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot2" = {
#           plot_effortFltStk_plotly(
#             data = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot3" = {
#           plot_landByMetStock_plotly(
#             data = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot4" = {
#           plot_landByStock_plotly(
#             data = data_reactive_all()$StockLandings_filtered,
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot5" = {
#           plot_catchComp_plotly(
#             dataComposition = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered,
#             filters = NULL,
#             selectors = "year",
#             divider = "fleet",
#             yvar = "catch"
#           )
#         },

#         "plot6" = {
#           plot_relEffortFltStk_plotly(
#             data = data_reactive_all()$EffortByFleetStock_filtered
#           )
#         }
#       )
#     })
#   })
# }






























# #' Mixed fisheries UI module
# #'
# #' This module UI creates the Mixed Fisheries section of fisheriesXplorer.
# #' It uses a sidebar for explanatory text and a main card containing:
# #' plot selection, case-study selection where relevant, dynamic filters,
# #' and the selected plot.
# #'
# #' @param id A character string used as the module namespace.
# #'
# #' @return A Shiny UI tag list.
# #'
# #' @export
# mod_mixfish_ui <- function(id) {
#   ns <- NS(id)

#   tagList(
#     mod_flex_header_ui(ns, "ecoregion_label", "current_date"),

#     layout_sidebar(
#       bg = "white",
#       fg = "black",

#       sidebar = sidebar(
#         width = "33vw",
#         bg = "white",
#         fg = "black",
#         open = FALSE,
#         uiOutput(ns("mixfish_text"))
#       ),

#       card(
#         height = "85vh",
#         full_screen = TRUE,
#         fill = FALSE,

#         card_header("Mixed fisheries forecasts"),

#         card_body(
#           fillable = TRUE,
#           fill = TRUE,
#           class = "p-1",

#           uiOutput(ns("subregion_ui")),

#           selectizeInput(
#             inputId = ns("plot_selected"),
#             label = "Select plot:",
#             choices = list(
#               "Data" = c(
#                 "Landings by métier & stock" = "plot3",
#                 "Landings by stock" = "plot4",
#                 "Landings composition by fleet" = "plot5"
#               ),
#               "Analysis" = c(
#                 "Scenarios" = "plot1",
#                 "Effort by fleet & stock" = "plot2",
#                 "Variation of effort by fleet & stock" = "plot6"
#               )
#             ),
#             selected = "plot1",
#             multiple = FALSE,
#             options = list(
#               placeholder = "Choose a plot"
#             )
#           ),

#           uiOutput(ns("filter_ui")),

#           withSpinner(
#             plotlyOutput(ns("plot"), height = "75vh"),
#             caption = "Getting mix-fish results..."
#           )
#         )
#       )
#     )
#   )
# }



# #' Server logic for the mixed fisheries module
# #'
# #' This module server manages:
# #' \itemize{
# #'   \item ecoregion and case-study selection;
# #'   \item grouped plot selection;
# #'   \item plot-specific filter UI;
# #'   \item data filtering;
# #'   \item plot rendering.
# #' }
# #'
# #' @param id Module id, matching the id used in \code{mod_mixfish_ui()}.
# #' @param selected_ecoregion A reactive returning the selected ICES ecoregion.
# #' @param bookmark_qs A reactive list containing query-string values.
# #' @param set_subtab Callback used to update the selected subtab upstream.
# #'
# #' @return No direct return value.
# #'
# #' @export
# mod_mixfish_server <- function(
#     id,
#     selected_ecoregion,
#     bookmark_qs = reactive(NULL),
#     set_subtab = function(...) {}) {

#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns

#     ################################## bookmarking #########################################

#     observeEvent(bookmark_qs(), once = TRUE, ignoreInit = TRUE, {
#       qs <- bookmark_qs()
#       wanted <- qs$subtab
#       valid <- c("landings", "discards")

#       if (!is.null(wanted) && nzchar(wanted) && wanted %in% valid) {
#         session$onFlushed(function() {
#           updateTabsetPanel(session, "main_tabset", selected = wanted)
#           isolate(set_subtab(wanted))
#         }, once = TRUE)
#       }
#     })

#     observeEvent(
#       input$main_tabset,
#       {
#         set_subtab(input$main_tabset)
#       },
#       ignoreInit = TRUE
#     )

#     ################################## header + glossary #########################################

#     output$ecoregion_label <- renderUI({
#       req(selected_ecoregion())

#       tags$span(
#         tags$b("ICES ecoregion:"),
#         " ",
#         paste0(
#           selected_ecoregion(),
#           " (",
#           get_ecoregion_acronym(selected_ecoregion()),
#           ")"
#         )
#       )
#     })

#     output$current_date <- renderUI({
#       tab <- input$main_tabset
#       if (is.null(tab)) tab <- "landings"

#       date_text <- switch(
#         tab,
#         "landings" = "October, 2025",
#         "discards" = format(Sys.Date(), "%B %d, %Y"),
#         ""
#       )

#       tagList(
#         tags$span(tags$b("Last data update:"), " ", date_text),
#         tags$span(" \u00B7 "),
#         mod_glossary_float_ui(
#           ns("app_glossary"),
#           link_text = "Glossary",
#           panel_title = "Glossary"
#         )
#       )
#     })

#     mod_glossary_float_server(
#       "app_glossary",
#       terms = reactive({
#         df <- select_text(texts, "glossary", NULL)

#         df[
#           ,
#           intersect(names(df), c("term", "definition", "source")),
#           drop = FALSE
#         ]
#       })
#     )

#     ################################## Sidebar text ##################################

#     output$mixfish_text <- renderUI({
#       req(selected_ecoregion())

#       div(
#         class = "sidebar-text",
#         HTML(
#           select_text(
#             texts,
#             paste0("mixfish_", get_ecoregion_acronym(selected_ecoregion())),
#             "overview"
#           )
#         )
#       )
#     })

#     ################################## Plot and subregion selection ##################################

#     filtered_plot_names <- c("plot1", "plot2", "plot3", "plot5")
#     unfiltered_plot_names <- c("plot4", "plot6")

#     selected_subRegion <- reactiveVal(NULL)

#     region_ready <- reactiveVal(FALSE)
#     filters_ready <- reactiveVal(FALSE)

#     plot_name <- reactive({
#       req(input$plot_selected)
#       input$plot_selected
#     })

#     subregion_choices <- reactive({
#       req(selected_ecoregion())

#       acr <- get_ecoregion_acronym(selected_ecoregion())

#       switch(
#         acr,
#         "CS" = c("Celtic Sea", "Irish Sea"),
#         "BI" = c("Bay of Biscay", "Iberian Waters"),
#         NULL
#       )
#     })

#     output$subregion_ui <- renderUI({
#       choices <- subregion_choices()

#       if (is.null(choices)) {
#         return(NULL)
#       }

#       selectInput(
#         inputId = ns("subRegion"),
#         label = "Select case study:",
#         choices = choices,
#         selected = choices[1]
#       )
#     })

#     observeEvent(
#       selected_ecoregion(),
#       {
#         region_ready(FALSE)

#         choices <- subregion_choices()

#         if (is.null(choices)) {
#           selected_subRegion(NULL)
#         } else {
#           selected_subRegion(choices[1])
#         }

#         session$onFlushed(function() {
#           region_ready(TRUE)
#         }, once = TRUE)
#       },
#       ignoreInit = FALSE
#     )

#     observeEvent(
#       input$subRegion,
#       {
#         selected_subRegion(input$subRegion)
#         region_ready(TRUE)
#       },
#       ignoreInit = TRUE
#     )

#     observeEvent(
#       plot_name(),
#       {
#         filters_ready(FALSE)
#       },
#       ignoreInit = FALSE
#     )

#     ################################## Data filtering ##################################

#     data_reactive_all <- reactive({
#       req(selected_ecoregion())
#       req(region_ready())

#       eco_acronym <- get_active_region_acronym(
#         selected_subRegion(),
#         selected_ecoregion()
#       )

#       validate(
#         need(
#           eco_acronym %in% catchScenarioStk$ecoregion,
#           "Invalid ecoregion filter."
#         )
#       )

#       list(
#         catchScenarioStk_filtered =
#           catchScenarioStk %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         catchRange_filtered =
#           catchRange %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         EffortByFleetStock_filtered =
#           EffortByFleetStock %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         MetierStockLandings_filtered =
#           MetierStockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         StockLandings_filtered =
#           StockLandings %>%
#           dplyr::filter(ecoregion == eco_acronym),

#         refTable_filtered =
#           refTable %>%
#           dplyr::filter(ecoregion == eco_acronym)
#       )
#     })

#     dataComp <- reactive({
#       req(plot_name())

#       data(stfMtStkSum, package = "mixfishtools")

#       list(
#         stfMtStkSum = stfMtStkSum
#       )
#     })

#     ################################## Dynamic filter UI ##################################

#     output$filter_ui <- renderUI({
#       req(plot_name())
#       req(region_ready())

#       ui <- switch(
#         plot_name(),

#         "plot1" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             scenario = list(
#               inputId = "scenario",
#               label = "Management scenario:",
#               placeholder = "Select scenario"
#             ),
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             )
#           )
#         ),

#         "plot2" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),

#         "plot3" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             stock = list(
#               inputId = "stock",
#               label = "Fish stock:",
#               placeholder = "Select stock"
#             ),
#             metier = list(
#               inputId = "metier",
#               label = "Métier:",
#               placeholder = "Select métier"
#             )
#           )
#         ),

#         "plot4" = NULL,

#         "plot5" = datamods::select_group_ui(
#           label = NULL,
#           id = ns("my-filters-mixfish"),
#           params = list(
#             year = list(
#               inputId = "year",
#               label = "Year:",
#               placeholder = "Select year"
#             ),
#             fleet = list(
#               inputId = "fleet",
#               label = "Fleet:",
#               placeholder = "Select fleet"
#             )
#           )
#         ),

#         "plot6" = NULL
#       )

#       if (plot_name() %in% filtered_plot_names) {
#         session$onFlushed(function() {
#           filters_ready(TRUE)
#         }, once = TRUE)
#       } else {
#         filters_ready(TRUE)
#       }

#       ui
#     })

#     filtered_data <- datamods::select_group_server(
#       id = "my-filters-mixfish",

#       data_r = reactive({
#         req(plot_name())
#         req(region_ready())

#         switch(
#           plot_name(),

#           "plot1" = data_reactive_all()$catchScenarioStk_filtered,

#           "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

#           "plot3" = data_reactive_all()$MetierStockLandings_filtered,

#           "plot5" = dataComp()$stfMtStkSum,

#           data.frame()
#         )
#       }),

#       vars_r = reactive({
#         req(plot_name())

#         switch(
#           plot_name(),

#           "plot1" = c("scenario", "stock"),

#           "plot2" = c("fleet"),

#           "plot3" = c("stock", "metier"),

#           "plot5" = c("year", "fleet"),

#           character(0)
#         )
#       })
#     )

#     plot_data_raw <- reactive({
#       req(plot_name())
#       req(region_ready())

#       if (plot_name() %in% filtered_plot_names) {
#         req(filters_ready())

#         df <- filtered_data()

#         req(!is.null(df))
#         req(NROW(df) > 0)

#         return(df)
#       }

#       NULL
#     })

#     plot_data <- shiny::debounce(plot_data_raw, millis = 300)

#     ################################## Plot rendering ##################################

#     output$plot <- renderPlotly({
#       req(plot_name())
#       req(region_ready())

#       switch(
#         plot_name(),

#         "plot1" = {
#           plot_catchScenStk_plotly(
#             data = plot_data(),
#             adv = data_reactive_all()$catchRange_filtered,
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot2" = {
#           plot_effortFltStk_plotly(
#             data = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot3" = {
#           plot_landByMetStock_plotly(
#             data = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot4" = {
#           plot_landByStock_plotly(
#             data = data_reactive_all()$StockLandings_filtered,
#             refTable = data_reactive_all()$refTable_filtered
#           )
#         },

#         "plot5" = {
#           plot_catchComp_plotly(
#             dataComposition = plot_data(),
#             refTable = data_reactive_all()$refTable_filtered,
#             filters = NULL,
#             selectors = "year",
#             divider = "fleet",
#             yvar = "catch"
#           )
#         },

#         "plot6" = {
#           plot_relEffortFltStk_plotly(
#             data = data_reactive_all()$EffortByFleetStock_filtered
#           )
#         }
#       )
#     })
#   })
# }


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
      valid <- c("landings", "discards")

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
      if (is.null(tab)) tab <- "landings"

      date_text <- switch(
        tab,
        "landings" = "October, 2025",
        "discards" = format(Sys.Date(), "%B %d, %Y"),
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

    filtered_plot_names <- c("plot1", "plot2", "plot3", "plot5")

    selected_subRegion <- reactiveVal(NULL)
    region_ready <- reactiveVal(FALSE)

    plot_name <- reactive({
      req(input$plot_selected)
      input$plot_selected
    })

    subregion_choices <- reactive({
      req(selected_ecoregion())

      acr <- get_ecoregion_acronym(selected_ecoregion())

      switch(
        acr,
        "CS" = c("Celtic Sea", "Irish Sea"),
        "BI" = c("Bay of Biscay", "Iberian Waters"),
        NULL
      )
    })

    output$subregion_ui <- renderUI({
      choices <- subregion_choices()

      if (is.null(choices)) {
        return(NULL)
      }

      selectInput(
        inputId = ns("subRegion"),
        label = "Select case study:",
        choices = choices,
        selected = choices[1]
      )
    })

    observeEvent(
      selected_ecoregion(),
      {
        region_ready(FALSE)

        choices <- subregion_choices()

        if (is.null(choices)) {
          selected_subRegion(NULL)
        } else {
          selected_subRegion(choices[1])
        }

        session$onFlushed(function() {
          region_ready(TRUE)
        }, once = TRUE)
      },
      ignoreInit = FALSE
    )

    observeEvent(
      input$subRegion,
      {
        selected_subRegion(input$subRegion)
        region_ready(TRUE)
      },
      ignoreInit = TRUE
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
      data(stfMtStkSum, package = "mixfishtools")

      list(
        stfMtStkSum = stfMtStkSum
      )
    })

    ################################## Filter helpers ##################################

    filter_source_data <- reactive({
      req(plot_name())
      req(region_ready())

      switch(
        plot_name(),

        "plot1" = data_reactive_all()$catchScenarioStk_filtered,

        "plot2" = data_reactive_all()$EffortByFleetStock_filtered,

        "plot3" = data_reactive_all()$MetierStockLandings_filtered,

        "plot5" = dataComp()$stfMtStkSum,

        NULL
      )
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

    ################################## Dynamic filter UI ##################################

    output$filter_ui <- renderUI({
      req(plot_name())
      req(region_ready())

      df <- filter_source_data()

      switch(
        plot_name(),

        "plot1" = tagList(
          filter_selectize(
            input_id = "plot1_scenario_filter",
            label = "Management scenario:",
            choices = safe_choices(df, "scenario")
          ),
          filter_selectize(
            input_id = "plot1_stock_filter",
            label = "Fish stock:",
            choices = safe_choices(df, "stock")
          )
        ),

        "plot2" = tagList(
          filter_selectize(
            input_id = "plot2_fleet_filter",
            label = "Fleet:",
            choices = safe_choices(df, "fleet")
          )
        ),

        "plot3" = tagList(
          filter_selectize(
            input_id = "plot3_stock_filter",
            label = "Fish stock:",
            choices = safe_choices(df, "stock")
          ),
          filter_selectize(
            input_id = "plot3_metier_filter",
            label = "Métier:",
            choices = safe_choices(df, "metier")
          )
        ),

        "plot4" = NULL,

        "plot5" = tagList(
          filter_selectize(
            input_id = "plot5_year_filter",
            label = "Year:",
            choices = safe_choices(df, "year")
          ),
          filter_selectize(
            input_id = "plot5_fleet_filter",
            label = "Fleet:",
            choices = safe_choices(df, "fleet")
          )
        ),

        "plot6" = NULL
      )
    })

    ################################## Current plot data ##################################

    current_plot_data <- reactiveVal(NULL)

    filter_touched <- reactiveValues(
      plot1 = FALSE,
      plot2 = FALSE,
      plot3 = FALSE,
      plot5 = FALSE
    )

    reset_filter_touched <- function() {
      filter_touched$plot1 <- FALSE
      filter_touched$plot2 <- FALSE
      filter_touched$plot3 <- FALSE
      filter_touched$plot5 <- FALSE
    }

    reset_current_plot_data <- function() {
      df <- filter_source_data()

      if (!is.null(df) && NROW(df) > 0) {
        current_plot_data(df)
      } else {
        current_plot_data(NULL)
      }
    }

    observeEvent(
      {
        list(
          plot_name(),
          region_ready(),
          selected_subRegion()
        )
      },
      {
        req(plot_name())
        req(region_ready())

        reset_filter_touched()

        if (plot_name() %in% filtered_plot_names) {
          reset_current_plot_data()
        } else {
          current_plot_data(NULL)
        }
      },
      ignoreInit = FALSE
    )

    ################################## Filter observers ##################################

    observeEvent(
      {
        list(
          input$plot1_scenario_filter,
          input$plot1_stock_filter
        )
      },
      {
        req(plot_name() == "plot1")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot1_scenario_filter,
          input$plot1_stock_filter
        )

        if (!has_selection && !isTRUE(filter_touched$plot1)) {
          return()
        }

        if (has_selection) {
          filter_touched$plot1 <- TRUE
        }

        df <- filter_source_data()

        df <- df %>%
          apply_optional_filter("scenario", input$plot1_scenario_filter) %>%
          apply_optional_filter("stock", input$plot1_stock_filter)

        current_plot_data(df)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      input$plot2_fleet_filter,
      {
        req(plot_name() == "plot2")
        req(region_ready())

        has_selection <- filter_has_selection(input$plot2_fleet_filter)

        if (!has_selection && !isTRUE(filter_touched$plot2)) {
          return()
        }

        if (has_selection) {
          filter_touched$plot2 <- TRUE
        }

        df <- filter_source_data()

        df <- df %>%
          apply_optional_filter("fleet", input$plot2_fleet_filter)

        current_plot_data(df)
      },
      ignoreInit = TRUE
    )

    observeEvent(
      {
        list(
          input$plot3_stock_filter,
          input$plot3_metier_filter
        )
      },
      {
        req(plot_name() == "plot3")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot3_stock_filter,
          input$plot3_metier_filter
        )

        if (!has_selection && !isTRUE(filter_touched$plot3)) {
          return()
        }

        if (has_selection) {
          filter_touched$plot3 <- TRUE
        }

        df <- filter_source_data()

        df <- df %>%
          apply_optional_filter("stock", input$plot3_stock_filter) %>%
          apply_optional_filter("metier", input$plot3_metier_filter)

        current_plot_data(df)
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
        req(plot_name() == "plot5")
        req(region_ready())

        has_selection <- filter_has_selection(
          input$plot5_year_filter,
          input$plot5_fleet_filter
        )

        if (!has_selection && !isTRUE(filter_touched$plot5)) {
          return()
        }

        if (has_selection) {
          filter_touched$plot5 <- TRUE
        }

        df <- filter_source_data()

        df <- df %>%
          apply_optional_filter("year", input$plot5_year_filter) %>%
          apply_optional_filter("fleet", input$plot5_fleet_filter)

        current_plot_data(df)
      },
      ignoreInit = TRUE
    )

    ################################## Plot rendering ##################################

    output$plot <- renderPlotly({
      req(plot_name())
      req(region_ready())

      switch(
        plot_name(),

        "plot1" = {
          req(current_plot_data())

          plot_catchScenStk_plotly(
            data = current_plot_data(),
            adv = data_reactive_all()$catchRange_filtered,
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot2" = {
          req(current_plot_data())

          plot_effortFltStk_plotly(
            data = current_plot_data(),
            refTable = data_reactive_all()$refTable_filtered
          )
        },

        "plot3" = {
          req(current_plot_data())

          plot_landByMetStock_plotly(
            data = current_plot_data(),
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
          req(current_plot_data())

          plot_catchComp_plotly(
            dataComposition = current_plot_data(),
            refTable = data_reactive_all()$refTable_filtered,
            filters = NULL,
            selectors = "year",
            divider = "fleet",
            yvar = "catch"
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