DEFAULT_ECOREGION <- "Greater North Sea"

#' Get active region acronym from subregion or ecoregion
#'
#' Returns the ecoregion acronym for the active region, preferring
#' \code{subregion} when it is not \code{NULL}, otherwise falling back
#' to \code{ecoregion}.
#'
#' @param subregion Optional character scalar giving the active
#'   subregion name. If non-\code{NULL}, this is used as the region
#'   passed to [get_ecoregion_acronym()].
#' @param ecoregion Character scalar giving the fallback ecoregion name
#'   to use when \code{subregion} is \code{NULL}.
#'
#' @return
#' A character scalar with the ecoregion acronym returned by
#' [get_ecoregion_acronym()].
#'
#' @details
#' This is a small convenience wrapper around [get_ecoregion_acronym()]
#' that encapsulates the logic of “use subregion if set, otherwise use
#' ecoregion” when determining which region is active in the UI.
#'
#' @examples
#' get_active_region_acronym(subregion = NULL, ecoregion = "Baltic Sea")
#' get_active_region_acronym(subregion = "Celtic Seas", ecoregion = "Baltic Sea")
#'
#' @export
get_active_region_acronym <- function(subregion, ecoregion) {
  region <- if (!is.null(subregion)) subregion else ecoregion
  get_ecoregion_acronym(region)
}


#' Prepare catch, landings and discards (CLD) data for trend plots
#'
#' Selects a subset of columns from an input data frame and replaces
#' zeros in the \code{Catches}, \code{Landings}, and \code{Discards}
#' columns with \code{NA}, making them easier to handle in trend plots
#' (e.g. avoiding plotting zeros where data are effectively missing).
#'
#' @param x A data frame containing at least the columns:
#'   \code{Year}, \code{StockKeyLabel}, \code{FisheriesGuild},
#'   \code{FishingPressure}, \code{FMSY}, \code{StockSize},
#'   \code{MSYBtrigger}, \code{Catches}, \code{Landings},
#'   and \code{Discards}.
#'
#' @return
#' A data frame with the columns:
#' \describe{
#'   \item{Year}{Year of the assessment or value.}
#'   \item{StockKeyLabel}{ICES stock identifier.}
#'   \item{FisheriesGuild}{Guild/category for the stock.}
#'   \item{FishingPressure}{Fishing pressure indicator.}
#'   \item{FMSY}{Fishing mortality at MSY.}
#'   \item{StockSize}{Stock size indicator.}
#'   \item{MSYBtrigger}{Biomass trigger reference point.}
#'   \item{Catches}{Catches, with zeros converted to \code{NA}.}
#'   \item{Landings}{Landings, with zeros converted to \code{NA}.}
#'   \item{Discards}{Discards, with zeros converted to \code{NA}.}
#' }
#'
#' @details
#' The function is mainly intended as a small preprocessing helper
#' before visualising CLD trends. Zero values in \code{Catches},
#' \code{Landings}, and \code{Discards} are interpreted as “no data” or
#' non-informative and replaced with \code{NA}; all other values are
#' left unchanged.
#'
#' @examples
#' \dontrun{
#' cld_df <- CLD_trends(sag_status_df)
#' }
#'
#' @export
CLD_trends <- function(x){
        df<- dplyr::select(x,Year,
                       StockKeyLabel,
                       FisheriesGuild,
                       FishingPressure,
                       FMSY,
                       StockSize,
                       MSYBtrigger,
                       Catches,
                       Landings,
                       Discards)     
        df["Discards"][df["Discards"] == 0] <- NA
        df["Catches"][df["Catches"] == 0] <- NA
        df["Landings"][df["Landings"] == 0] <- NA
      
        return(df)
}


################################################### plot functions ##########################################

#' Plot landings (catch) trends with plotly
#'
#' Creates interactive landings trend plots using \pkg{plotly}, either as:
#' \itemize{
#'   \item a set of small-multiple plots (one per fisheries guild) when
#'     \code{type = "Common name"}, or
#'   \item a single plot stratified by \code{type} (e.g. country or guild).
#' }
#' The function ranks categories by total landings, keeps the top
#' \code{line_count}, and aggregates the rest into an \code{"other"} group.
#'
#' @param x A data frame with columns in the following order:
#'   \code{Year}, \code{Country}, \code{iso3}, \code{Fisheries guild},
#'   \code{Ecoregion}, \code{Species name}, \code{Species code},
#'   \code{Common name}, \code{Value}. These are renamed internally and
#' interpreted as landings (in tonnes).
#' @param type Character scalar, one of \code{"Common name"},
#'   \code{"Country"}, or \code{"Fisheries guild"}. Determines the
#'   grouping variable for the lines.
#' @param line_count Integer; maximum number of categories (within each
#'   grouping) to show as separate lines. Remaining categories are
#'   aggregated into \code{"other"}. Default is \code{10}.
#' @param dataUpdated Optional character string appended to the caption
#'   (e.g. a “data updated” date or note).
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data used for plotting instead of the plotly object(s).
#'   When \code{type = "Common name"}, this is the combined data across
#'   fisheries guilds. Default is \code{FALSE}.
#' @param session Optional Shiny session object. If provided, the
#'   function reads \code{session$clientData[["output_landings_1-landings_layer_width"]]}
#'   to adapt font sizes to the available plot width. If \code{NULL}, a
#'   default width of 800px is assumed.
#' @param per_panel_height Numeric; height (in pixels) used for each
#'   panel when \code{type = "Common name"} and multiple guild plots are
#'   produced. Default is \code{380}.
#' @param ecoregion Optional character scalar used in plot titles and
#'   image file names (e.g. \code{"Greater North Sea"} or an acronym).
#'
#' @return
#' If \code{return_data = TRUE}, a data frame with yearly aggregated
#' landings (in thousand tonnes) by grouping variable is returned.
#'
#' If \code{return_data = FALSE}:
#' \itemize{
#'   \item for \code{type = "Common name"}, an \code{htmltools::tagList}
#'     of \pkg{plotly} objects (one per fisheries guild) is returned;
#'   \item for other \code{type} values, a single \pkg{plotly} object is
#'     returned.
#' }
#'
#' @details
#' For \code{type = "Common name"}, the function:
#' \enumerate{
#'   \item Normalises common names (e.g. collapsing variants such as
#'     “Sandeels …” to \code{"sandeel"}).
#'   \item Splits the data by \code{Fisheries guild}.
#'   \item Within each guild, ranks common names by total landings and
#'     keeps the top \code{line_count}, aggregating the rest into an
#'     \code{"other"} category.
#'   \item Aggregates landings by \code{Year} and \code{type_var} and
#'     converts them to thousand tonnes.
#'   \item Produces one plotly line chart per guild, using a discrete
#'     \code{hcl.colors(..., "Temps")} palette and a common caption that
#'     distinguishes historical (1950–2006) and official (2006–2023)
#'     catches.
#' }
#'
#' For other \code{type} values (e.g. \code{"Country"}, \code{"Fisheries guild"}),
#' the function aggregates across all guilds into a single data set and
#' produces one multi-line plot.
#'
#' In both cases, the y-axis shows landings in thousand tonnes, and the
#' plots are configured with a download-to-image button whose filename
#' includes \code{ecoregion} and the current date.
#'
#' @importFrom dplyr rename all_of filter group_by summarise arrange desc
#'   mutate inner_join pull
#' @importFrom grDevices hcl.colors
#' @importFrom plotly plot_ly add_trace layout highlight config attrs_selected
#' @importFrom htmltools tagList
#' @export
# plot_catch_trends_plotly <- function(
#   x,
#   type = c("Common name", "Country", "Fisheries guild"),
#   line_count = 10,
#   selected_guild = NULL,
#   dataUpdated = NULL,
#   return_data = FALSE,
#   session = NULL,
#   ecoregion = NULL
# ) {
#   type <- match.arg(type)

#   # --- Responsive font sizes
#   w <- tryCatch({
#     if (!is.null(session)) {
#       session$clientData[[paste0("output_", session$ns("landings_layer"), "_width")]]
#     } else {
#       NA_real_
#     }
#   }, error = function(e) NA_real_)

#   if (is.na(w) || is.null(w)) w <- 800

#   base_size         <- max(9,  min(18, round(w / 55)))
#   axis_title_size   <- max(10, min(20, round(w / 50)))
#   tick_size         <- max(9,  min(16, round(w / 55)))
#   legend_title_size <- max(10, min(18, round(w / 55)))
#   legend_text_size  <- max(9,  min(16, round(w / 65)))
#   title_annot_size  <- max(12, min(22, round(w / 40)))
#   caption_size      <- max(8,  min(14, round(w / 70)))

#   # --- Dynamic bottom margin for caption
#   caption_lines <- 3
#   bottom_margin <- max(100, 20 + caption_lines * (caption_size + 10))

#   # --- Expected columns
#   names(x) <- c(
#     "Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
#     "Species name", "Species code", "Common name", "Value"
#   )

#   cap_text <- paste0(
#     "Historical Nominal Catches 1950–2005.<br>",
#     "Official Nominal Catches 2006–2023.<br>",
#     dataUpdated, ", ICES, Copenhagen."
#   )

#   sanitize_stub <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
#   date_stamp <- format(Sys.Date(), "%d-%b-%y")
#   palette_vec <- function(n) grDevices::hcl.colors(max(n, 1), palette = "Temps")

#   df <- x %>%
#     dplyr::filter(!is.na(Year))

#   if (type == "Common name") {
#     if (!is.null(selected_guild) && nzchar(selected_guild)) {
#       df <- df %>% dplyr::filter(`Fisheries guild` == selected_guild)
#     }

#     df <- df %>%
#       dplyr::mutate(
#         type_var = `Common name`,
#         type_var = gsub("European ", "", type_var),
#         type_var = gsub("Sandeels.*", "sandeel", type_var),
#         type_var = gsub("Finfishes nei", "undefined finfish", type_var),
#         type_var = gsub("Blue whiting.*", "blue whiting", type_var),
#         type_var = gsub("Saithe.*", "saithe", type_var),
#         type_var = ifelse(grepl("Norway", type_var), type_var, tolower(type_var))
#       )
#   } else if (type == "Country") {
#     df <- df %>% dplyr::mutate(type_var = Country)
#   } else if (type == "Fisheries guild") {
#     df <- df %>% dplyr::mutate(type_var = `Fisheries guild`)
#   }

#   total_df <- df %>%
#     dplyr::group_by(Year) %>%
#     dplyr::summarise(total = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

#   ranked <- df %>%
#     dplyr::group_by(type_var) %>%
#     dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::arrange(dplyr::desc(typeTotal)) %>%
#     dplyr::filter(typeTotal >= 1) %>%
#     dplyr::mutate(RANK = dplyr::row_number())

#   plot_df <- df %>%
#     dplyr::inner_join(ranked, by = "type_var") %>%
#     dplyr::mutate(type_var = ifelse(RANK > line_count, "other", type_var)) %>%
#     dplyr::group_by(type_var, Year) %>%
#     dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

#   type_levels <- plot_df %>%
#     dplyr::group_by(type_var) %>%
#     dplyr::summarise(tt = sum(typeTotal, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::arrange(dplyr::desc(tt)) %>%
#     dplyr::pull(type_var)

#   plot_df$type_var <- factor(plot_df$type_var, levels = type_levels)

#   if (return_data) {
#     return(list(series = plot_df, total = total_df))
#   }

#   n_types <- length(unique(plot_df$type_var))
#   pal <- palette_vec(n_types)

#   subtitle_part <- if (type == "Common name" && !is.null(selected_guild) && nzchar(selected_guild)) {
#     paste0(" - ", selected_guild)
#   } else {
#     ""
#   }

#   file_stub <- paste0(
#     sanitize_stub(ifelse(is.null(ecoregion), "ecoregion", ecoregion)),
#     "_landings_",
#     sanitize_stub(type),
#     if (!is.null(selected_guild) && nzchar(selected_guild)) {
#       paste0("_", sanitize_stub(selected_guild))
#     } else {
#       ""
#     },
#     "_",
#     date_stamp
#   )

#   # Important: keyed data for click highlighting
#   keyed_df <- plotly::highlight_key(plot_df, ~type_var)

#   plotly::plot_ly(
#     keyed_df,
#     x = ~Year,
#     y = ~typeTotal,
#     color = ~type_var,
#     colors = pal,
#     showlegend = TRUE,
#     type = "scatter",
#     mode = "lines",
#     line = list(width = 3),
#     hovertemplate = paste0(
#       "<b>", type, ":</b> %{fullData.name}<br>",
#       "<b>Year:</b> %{x}<br>",
#       "<b>Landings:</b> %{y:.2f} thousand tonnes<extra></extra>"
#     ),
#     source = "landings_trends"
#   ) %>%
#     plotly::add_trace(
#       data = total_df,
#       x = ~Year,
#       y = ~total,
#       type = "scatter",
#       mode = "lines",
#       inherit = FALSE,
#       name = "Total",
#       line = list(color = "black", width = 3, dash = "dash"),
#       hovertemplate = paste0(
#         "<b>Total</b><br>",
#         "<b>Year:</b> %{x}<br>",
#         "<b>Landings:</b> %{y:.2f} thousand tonnes<extra></extra>"
#       )
#     ) %>%
#     plotly::layout(
#       font = list(size = base_size),
#       xaxis = list(
#         title = list(text = "Year", font = list(size = axis_title_size)),
#         tickfont = list(size = tick_size),
#         automargin = TRUE
#       ),
#       yaxis = list(
#         title = list(
#           text = "Landings (thousand tonnes)",
#           font = list(size = axis_title_size),
#           standoff = 18
#         ),
#         tickfont = list(size = tick_size),
#         automargin = TRUE
#       ),
#       margin = list(l = 80, r = 20, t = 70, b = bottom_margin),
#       annotations = list(
#         list(
#           text = paste0("Landings trends (", ecoregion, subtitle_part, ")"),
#           x = 0.01, y = 0.99,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "left", yanchor = "top",
#           font = list(size = title_annot_size, color = "black")
#         ),
#         list(
#           x = 1, y = 0,
#           xref = "paper", yref = "paper",
#           xanchor = "right", yanchor = "top",
#           yshift = -40,
#           text = cap_text,
#           showarrow = FALSE,
#           align = "right",
#           font = list(size = caption_size, color = "black")
#         )
#       ),
#       legend = list(
#         title = list(
#           text = paste0("<b>", type, ":</b>"),
#           font = list(size = legend_title_size)
#         ),
#         orientation = "h",
#         x = 0.5, y = 1.08,
#         xanchor = "center", yanchor = "bottom",
#         font = list(size = legend_text_size),
#         itemwidth = 50
#       ),
#       hoverlabel = list(font = list(size = base_size))
#     ) %>%
#     plotly::highlight(
#       on = "plotly_click",
#       off = "plotly_doubleclick",
#       persistent = FALSE,
#       dynamic = FALSE,
#       selected = plotly::attrs_selected(
#         opacity = 1,
#         line = list(width = 5)
#       )
#     ) %>%
#     plotly::config(
#       responsive = TRUE,
#       toImageButtonOptions = list(
#         filename = file_stub,
#         format = "png",
#         scale = 3
#       )
#     )
# }


# # V2 changes:
# # - Common-name categories are ranked by cumulative landings from 2006 to max(Year).
# # - All available years remain visible in the plotted time series.
# # - For Common name, hovering on "other" lists the ten largest species
# #   contributing to "other" in that specific year, with their annual landings.
# # - Country and Fisheries guild retain the original full-series ranking.

# plot_catch_trends_plotly <- function(
#   x,
#   type = c("Common name", "Country", "Fisheries guild"),
#   line_count = 10,
#   selected_guild = NULL,
#   dataUpdated = NULL,
#   return_data = FALSE,
#   session = NULL,
#   ecoregion = NULL
# ) {
#   type <- match.arg(type)

#   # --- Responsive font sizes
#   w <- tryCatch({
#     if (!is.null(session)) {
#       session$clientData[[paste0("output_", session$ns("landings_layer"), "_width")]]
#     } else {
#       NA_real_
#     }
#   }, error = function(e) NA_real_)

#   if (is.na(w) || is.null(w)) w <- 800

#   base_size         <- max(9,  min(18, round(w / 55)))
#   axis_title_size   <- max(10, min(20, round(w / 50)))
#   tick_size         <- max(9,  min(16, round(w / 55)))
#   legend_title_size <- max(10, min(18, round(w / 55)))
#   legend_text_size  <- max(9,  min(16, round(w / 65)))
#   title_annot_size  <- max(12, min(22, round(w / 40)))
#   caption_size      <- max(8,  min(14, round(w / 70)))

#   # --- Dynamic bottom margin for caption
#   caption_lines <- 3
#   bottom_margin <- max(100, 20 + caption_lines * (caption_size + 10))

#   # --- Expected columns
#   names(x) <- c(
#     "Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
#     "Species name", "Species code", "Common name", "Value"
#   )

#   cap_text <- paste0(
#     "Historical Nominal Catches 1950–2005.<br>",
#     "Official Nominal Catches 2006–2024.<br>",
#     dataUpdated, ", ICES, Copenhagen."
#   )

#   sanitize_stub <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
#   date_stamp <- format(Sys.Date(), "%d-%b-%y")
#   palette_vec <- function(n) grDevices::hcl.colors(max(n, 1), palette = "Temps")

#   # Keep species names safe for insertion into HTML-based Plotly hover text.
#   escape_html <- function(s) {
#     s <- gsub("&", "&amp;", s, fixed = TRUE)
#     s <- gsub("<", "&lt;", s, fixed = TRUE)
#     gsub(">", "&gt;", s, fixed = TRUE)
#   }

#   df <- x %>%
#     dplyr::filter(!is.na(Year))

#   if (type == "Common name") {
#     if (!is.null(selected_guild) && nzchar(selected_guild)) {
#       df <- df %>% dplyr::filter(`Fisheries guild` == selected_guild)
#     }

#     df <- df %>%
#       dplyr::mutate(
#         type_var = `Common name`,
#         type_var = gsub("European ", "", type_var),
#         type_var = gsub("Sandeels.*", "sandeel", type_var),
#         type_var = gsub("Finfishes nei", "undefined finfish", type_var),
#         type_var = gsub("Blue whiting.*", "blue whiting", type_var),
#         type_var = gsub("Saithe.*", "saithe", type_var),
#         type_var = ifelse(grepl("Norway", type_var), type_var, tolower(type_var))
#       )
#   } else if (type == "Country") {
#     df <- df %>% dplyr::mutate(type_var = Country)
#   } else if (type == "Fisheries guild") {
#     df <- df %>% dplyr::mutate(type_var = `Fisheries guild`)
#   }

#   total_df <- df %>%
#     dplyr::group_by(Year) %>%
#     dplyr::summarise(total = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

#   # --- V2: rank Common name on 2006 onward, other types on all available years
#   ranking_df <- if (type == "Common name") {
#     df %>%
#       dplyr::filter(
#         Year >= 2006,
#         !is.na(type_var),
#         nzchar(trimws(type_var))
#       )
#   } else {
#     df
#   }

#   if (type == "Common name" && nrow(ranking_df) == 0) {
#     stop("Cannot rank common names: the input has no observations from 2006 onward.")
#   }

#   ranked <- ranking_df %>%
#     dplyr::group_by(type_var) %>%
#     dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::arrange(dplyr::desc(typeTotal)) %>%
#     dplyr::filter(typeTotal >= 1) %>%
#     dplyr::mutate(RANK = dplyr::row_number())

#   # --- V2: preserve historical-only common names in "other".
#   # Keep original inner_join behavior for Country and Fisheries guild.
#   if (type == "Common name") {
#     classified_df <- df %>%
#       dplyr::left_join(ranked %>% dplyr::select(type_var, RANK), by = "type_var")
#   } else {
#     classified_df <- df %>%
#       dplyr::inner_join(ranked %>% dplyr::select(type_var, RANK), by = "type_var")
#   }

#   classified_df <- classified_df %>%
#     dplyr::mutate(
#       plot_group = dplyr::if_else(
#         !is.na(RANK) & RANK <= line_count,
#         type_var,
#         "other"
#       )
#     )

#   plot_df <- classified_df %>%
#     dplyr::group_by(type_var = plot_group, Year) %>%
#     dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

#   type_levels <- plot_df %>%
#     dplyr::group_by(type_var) %>%
#     dplyr::summarise(tt = sum(typeTotal, na.rm = TRUE), .groups = "drop") %>%
#     dplyr::arrange(dplyr::desc(tt)) %>%
#     dplyr::pull(type_var)

#   plot_df$type_var <- factor(plot_df$type_var, levels = type_levels)

#   if (return_data) {
#     return(list(series = plot_df, total = total_df))
#   }

#   # --- Per-point hover text. Other traces get the regular hover text.
#   plot_df <- plot_df %>%
#     dplyr::mutate(
#       hover_text = paste0(
#         "<b>", type, ":</b> ", escape_html(as.character(type_var)),
#         "<br><b>Year:</b> ", Year,
#         "<br><b>Landings:</b> ", sprintf("%.2f", typeTotal),
#         " thousand tonnes"
#       )
#     )

#   # --- V2: list the top ten contributors INSIDE "other", separately by year.
#   # Each annual contribution is based on the same normalized common name
#   # used for ranking and plotting. The plotted total is not changed.
#   if (type == "Common name") {
#     other_top10 <- classified_df %>%
#       dplyr::filter(plot_group == "other") %>%
#       dplyr::mutate(
#         species = dplyr::if_else(
#           is.na(type_var) | trimws(type_var) == "",
#           "unidentified species",
#           type_var
#         )
#       ) %>%
#       dplyr::group_by(Year, species) %>%
#       dplyr::summarise(landings_kt = sum(Value, na.rm = TRUE) / 1000,
#                        .groups = "drop") %>%
#       dplyr::group_by(Year) %>%
#       dplyr::arrange(dplyr::desc(landings_kt), species, .by_group = TRUE) %>%
#       dplyr::slice_head(n = 10) %>%
#       dplyr::mutate(
#         hover_line = paste0(
#           dplyr::row_number(), ". ", escape_html(species), ": ",
#           sprintf("%.2f", landings_kt), " thousand tonnes"
#         )
#       ) %>%
#       dplyr::summarise(
#         other_detail = paste(hover_line, collapse = "<br>"),
#         .groups = "drop"
#       )

#     plot_df <- plot_df %>%
#       dplyr::left_join(other_top10, by = "Year") %>%
#       dplyr::mutate(
#         hover_text = dplyr::if_else(
#           as.character(type_var) == "other" & !is.na(other_detail),
#           paste0(
#             hover_text,
#             "<br><br><b>Top 10 taxa within 'other' this year:</b><br>",
#             other_detail
#           ),
#           hover_text
#         )
#       ) %>%
#       dplyr::select(-other_detail)
#   }

#   n_types <- length(unique(plot_df$type_var))
#   pal <- palette_vec(n_types)

#   subtitle_part <- if (type == "Common name" && !is.null(selected_guild) && nzchar(selected_guild)) {
#     paste0(" - ", selected_guild)
#   } else {
#     ""
#   }

#   file_stub <- paste0(
#     sanitize_stub(ifelse(is.null(ecoregion), "ecoregion", ecoregion)),
#     "_landings_",
#     sanitize_stub(type),
#     if (!is.null(selected_guild) && nzchar(selected_guild)) {
#       paste0("_", sanitize_stub(selected_guild))
#     } else {
#       ""
#     },
#     "_",
#     date_stamp
#   )

#   # Important: keyed data for click highlighting
#   keyed_df <- plotly::highlight_key(plot_df, ~type_var)

#   plotly::plot_ly(
#     keyed_df,
#     x = ~Year,
#     y = ~typeTotal,
#     text = ~hover_text,
#     color = ~type_var,
#     colors = pal,
#     showlegend = TRUE,
#     type = "scatter",
#     mode = "lines",
#     line = list(width = 3),
#     hovertemplate = "%{text}<extra></extra>",
#     source = "landings_trends"
#   ) %>%
#     plotly::add_trace(
#       data = total_df,
#       x = ~Year,
#       y = ~total,
#       type = "scatter",
#       mode = "lines",
#       inherit = FALSE,
#       name = "Total",
#       line = list(color = "black", width = 3, dash = "dash"),
#       hovertemplate = paste0(
#         "<b>Total</b><br>",
#         "<b>Year:</b> %{x}<br>",
#         "<b>Landings:</b> %{y:.2f} thousand tonnes<extra></extra>"
#       )
#     ) %>%
#     plotly::layout(
#       font = list(size = base_size),
#       xaxis = list(
#         title = list(text = "Year", font = list(size = axis_title_size)),
#         tickfont = list(size = tick_size),
#         automargin = TRUE
#       ),
#       yaxis = list(
#         title = list(
#           text = "Landings (thousand tonnes)",
#           font = list(size = axis_title_size),
#           standoff = 18
#         ),
#         tickfont = list(size = tick_size),
#         automargin = TRUE
#       ),
#       margin = list(l = 80, r = 20, t = 70, b = bottom_margin),
#       annotations = list(
#         list(
#           text = paste0("Landings trends (", ecoregion, subtitle_part, ")"),
#           x = 0.01, y = 0.99,
#           xref = "paper", yref = "paper",
#           showarrow = FALSE,
#           xanchor = "left", yanchor = "top",
#           font = list(size = title_annot_size, color = "black")
#         ),
#         list(
#           x = 1, y = 0,
#           xref = "paper", yref = "paper",
#           xanchor = "right", yanchor = "top",
#           yshift = -40,
#           text = cap_text,
#           showarrow = FALSE,
#           align = "right",
#           font = list(size = caption_size, color = "black")
#         )
#       ),
#       legend = list(
#         title = list(
#           text = paste0("<b>", type, ":</b>"),
#           font = list(size = legend_title_size)
#         ),
#         orientation = "h",
#         x = 0.5, y = 1.08,
#         xanchor = "center", yanchor = "bottom",
#         font = list(size = legend_text_size),
#         itemwidth = 50
#       ),
#       hoverlabel = list(font = list(size = base_size))
#     ) %>%
#     plotly::highlight(
#       on = "plotly_click",
#       off = "plotly_doubleclick",
#       persistent = FALSE,
#       dynamic = FALSE,
#       selected = plotly::attrs_selected(
#         opacity = 1,
#         line = list(width = 5)
#       )
#     ) %>%
#     plotly::config(
#       responsive = TRUE,
#       toImageButtonOptions = list(
#         filename = file_stub,
#         format = "png",
#         scale = 3
#       )
#     )
# }



# === Active replacement: HTML hover cards and annual "other" breakdown ===
# Common-name ranking remains 2006 onward; all available years are plotted.
# Country and Fisheries guild retain their original ranking and filtering.
#' Plot landings (catch) trends with plotly
#'
#' Creates interactive landings trend plots using \pkg{plotly}, either as:
#' \itemize{
#'   \item a set of small-multiple plots (one per fisheries guild) when
#'     \code{type = "Common name"}, or
#'   \item a single plot stratified by \code{type} (e.g. country or guild).
#' }
#' The function ranks categories by total landings, keeps the top
#' \code{line_count}, and aggregates the rest into an \code{"other"} group.
#'
#' @param x A data frame with columns in the following order:
#'   \code{Year}, \code{Country}, \code{iso3}, \code{Fisheries guild},
#'   \code{Ecoregion}, \code{Species name}, \code{Species code},
#'   \code{Common name}, \code{Value}. These are renamed internally and
#' interpreted as landings (in tonnes).
#' @param type Character scalar, one of \code{"Common name"},
#'   \code{"Country"}, or \code{"Fisheries guild"}. Determines the
#'   grouping variable for the lines.
#' @param line_count Integer; maximum number of categories (within each
#'   grouping) to show as separate lines. Remaining categories are
#'   aggregated into \code{"other"}. Default is \code{10}.
#' @param dataUpdated Optional character string appended to the caption
#'   (e.g. a “data updated” date or note).
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data used for plotting instead of the plotly object(s).
#'   When \code{type = "Common name"}, this is the combined data across
#'   fisheries guilds. Default is \code{FALSE}.
#' @param session Optional Shiny session object. If provided, the
#'   function reads \code{session$clientData[["output_landings_1-landings_layer_width"]]}
#'   to adapt font sizes to the available plot width. If \code{NULL}, a
#'   default width of 800px is assumed.
#' @param per_panel_height Numeric; height (in pixels) used for each
#'   panel when \code{type = "Common name"} and multiple guild plots are
#'   produced. Default is \code{380}.
#' @param ecoregion Optional character scalar used in plot titles and
#'   image file names (e.g. \code{"Greater North Sea"} or an acronym).
#'
#' @return
#' If \code{return_data = TRUE}, a data frame with yearly aggregated
#' landings (in thousand tonnes) by grouping variable is returned.
#'
#' If \code{return_data = FALSE}:
#' \itemize{
#'   \item for \code{type = "Common name"}, an \code{htmltools::tagList}
#'     of \pkg{plotly} objects (one per fisheries guild) is returned;
#'   \item for other \code{type} values, a single \pkg{plotly} object is
#'     returned.
#' }
#'
#' @details
#' For \code{type = "Common name"}, the function:
#' \enumerate{
#'   \item Normalises common names (e.g. collapsing variants such as
#'     “Sandeels …” to \code{"sandeel"}).
#'   \item Splits the data by \code{Fisheries guild}.
#'   \item Within each guild, ranks common names by total landings and
#'     keeps the top \code{line_count}, aggregating the rest into an
#'     \code{"other"} category.
#'   \item Aggregates landings by \code{Year} and \code{type_var} and
#'     converts them to thousand tonnes.
#'   \item Produces one plotly line chart per guild, using a discrete
#'     \code{hcl.colors(..., "Temps")} palette and a common caption that
#'     distinguishes historical (1950–2006) and official (2006–2023)
#'     catches.
#' }
#'
#' For other \code{type} values (e.g. \code{"Country"}, \code{"Fisheries guild"}),
#' the function aggregates across all guilds into a single data set and
#' produces one multi-line plot.
#'
#' In both cases, the y-axis shows landings in thousand tonnes, and the
#' plots are configured with a download-to-image button whose filename
#' includes \code{ecoregion} and the current date.
#'
#' @importFrom dplyr rename all_of filter group_by summarise arrange desc
#'   mutate inner_join pull
#' @importFrom grDevices hcl.colors
#' @importFrom plotly plot_ly add_trace layout highlight config attrs_selected
#' @importFrom htmltools tagList
#' @export
plot_catch_trends_plotly <- function(
  x,
  type = c("Common name", "Country", "Fisheries guild"),
  line_count = 10,
  selected_guild = NULL,
  dataUpdated = NULL,
  return_data = FALSE,
  session = NULL,
  ecoregion = NULL
) {
  type <- match.arg(type)

  # --- Responsive font sizes
  w <- tryCatch({
    if (!is.null(session)) {
      session$clientData[[paste0("output_", session$ns("landings_layer"), "_width")]]
    } else {
      NA_real_
    }
  }, error = function(e) NA_real_)

  if (is.null(w) || is.na(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))

  # --- Dynamic bottom margin for caption
  caption_lines <- 3
  bottom_margin <- max(100, 20 + caption_lines * (caption_size + 10))

  # --- Expected columns
  names(x) <- c(
    "Year", "Country", "iso3", "Fisheries guild", "Ecoregion",
    "Species name", "Species code", "Common name", "Value"
  )

  cap_text <- paste0(
    "Historical Nominal Catches 1950–2005.<br>",
    "Official Nominal Catches 2006–2024.<br>",
    dataUpdated, ", ICES, Copenhagen."
  )

  sanitize_stub <- function(s) gsub("[^A-Za-z0-9]+", "_", s)
  date_stamp <- format(Sys.Date(), "%d-%b-%y")
  palette_vec <- function(n) grDevices::hcl.colors(max(n, 1), palette = "Temps")

  df <- x %>%
    dplyr::filter(!is.na(Year))

  if (type == "Common name") {
    if (!is.null(selected_guild) && nzchar(selected_guild)) {
      df <- df %>% dplyr::filter(`Fisheries guild` == selected_guild)
    }

    df <- df %>%
      dplyr::mutate(
        type_var = `Common name`,
        type_var = gsub("European ", "", type_var),
        type_var = gsub("Sandeels.*", "sandeel", type_var),
        type_var = gsub("Finfishes nei", "undefined finfish", type_var),
        type_var = gsub("Blue whiting.*", "blue whiting", type_var),
        type_var = gsub("Saithe.*", "saithe", type_var),
        type_var = ifelse(grepl("Norway", type_var), type_var, tolower(type_var))
      )
  } else if (type == "Country") {
    df <- df %>% dplyr::mutate(type_var = Country)
  } else if (type == "Fisheries guild") {
    df <- df %>% dplyr::mutate(type_var = `Fisheries guild`)
  }

  total_df <- df %>%
    dplyr::group_by(Year) %>%
    dplyr::summarise(total = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

  # --- V2: rank Common name on 2006 onward, other types on all available years
  ranking_df <- if (type == "Common name") {
    df %>%
      dplyr::filter(
        Year >= 2006,
        !is.na(type_var),
        nzchar(trimws(type_var))
      )
  } else {
    df
  }

  if (type == "Common name" && nrow(ranking_df) == 0) {
    stop("Cannot rank common names: the input has no observations from 2006 onward.")
  }

  ranked <- ranking_df %>%
    dplyr::group_by(type_var) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(typeTotal)) %>%
    dplyr::filter(typeTotal >= 1) %>%
    dplyr::mutate(RANK = dplyr::row_number())

  # --- V2: preserve historical-only common names in "other".
  # Keep original inner_join behavior for Country and Fisheries guild.
  if (type == "Common name") {
    classified_df <- df %>%
      dplyr::left_join(ranked %>% dplyr::select(type_var, RANK), by = "type_var")
  } else {
    classified_df <- df %>%
      dplyr::inner_join(ranked %>% dplyr::select(type_var, RANK), by = "type_var")
  }

  classified_df <- classified_df %>%
    dplyr::mutate(
      plot_group = dplyr::if_else(
        !is.na(RANK) & RANK <= line_count,
        type_var,
        "other"
      )
    )

  plot_df <- classified_df %>%
    dplyr::group_by(type_var = plot_group, Year) %>%
    dplyr::summarise(typeTotal = sum(Value, na.rm = TRUE) / 1000, .groups = "drop")

  type_levels <- plot_df %>%
    dplyr::group_by(type_var) %>%
    dplyr::summarise(tt = sum(typeTotal, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(tt)) %>%
    dplyr::pull(type_var)

  plot_df$type_var <- factor(plot_df$type_var, levels = type_levels)

  if (return_data) {
    return(list(series = plot_df, total = total_df))
  }

  # --- HTML hover payloads (kept out of return_data above).
  # Preserve the V2 breakdown: up to ten normalized common-name categories
  # within "other", ranked independently for each year. Do not filter out
  # zero/negative observations or change any of the plotted quantities.
  other_by_year <- list()
  if (type == "Common name") {
    other_annual <- classified_df %>%
      dplyr::filter(plot_group == "other") %>%
      dplyr::mutate(
        species = dplyr::if_else(
          is.na(type_var) | trimws(type_var) == "",
          "unidentified species",
          type_var
        )
      ) %>%
      dplyr::group_by(Year, species) %>%
      dplyr::summarise(
        landings_kt = sum(Value, na.rm = TRUE) / 1000,
        .groups = "drop"
      ) %>%
      dplyr::group_by(Year) %>%
      dplyr::arrange(dplyr::desc(landings_kt), species, .by_group = TRUE) %>%
      dplyr::ungroup()

    other_by_year <- lapply(split(other_annual, other_annual$Year), function(d) {
      shown <- utils::head(d, 10L)
      remaining <- if (nrow(d) > nrow(shown)) {
        sum(d$landings_kt[seq.int(nrow(shown) + 1L, nrow(d))], na.rm = TRUE)
      } else {
        0
      }
      list(
        taxa = lapply(seq_len(nrow(shown)), function(j) {
          list(
            name = as.character(shown$species[j]),
            landings_kt = as.numeric(shown$landings_kt[j])
          )
        }),
        additional_taxa = as.integer(nrow(d) - nrow(shown)),
        remaining_kt = as.numeric(remaining)
      )
    })
  }

  # Every point carries one JSON STRING. Plotly subsets this column together
  # with x/y when splitting categories and when creating highlighted traces.
  # The shared add_html_hover() helper reads and parses this string.
  make_payload <- function(group, year, landings_kt, is_total = FALSE) {
    has_breakdown <- !is_total && type == "Common name" &&
      !is.na(group) && group == "other"
    breakdown <- if (has_breakdown) other_by_year[[as.character(year)]] else NULL
    if (is.null(breakdown)) {
      breakdown <- list(taxa = list(), additional_taxa = 0L, remaining_kt = 0)
    }

    as.character(jsonlite::toJSON(
      list(
        group_type = type,
        group = as.character(group),
        year = as.integer(year),
        landings_kt = as.numeric(landings_kt),
        is_total = is_total,
        ecoregion = ecoregion,
        selected_guild = if (type == "Common name") selected_guild else NULL,
        has_breakdown = has_breakdown,
        taxa = breakdown$taxa,
        additional_taxa = breakdown$additional_taxa,
        remaining_kt = breakdown$remaining_kt
      ),
      auto_unbox = TRUE,
      na = "null",
      null = "null",
      digits = 12
    ))
  }

  plot_df$hover_payload <- vapply(seq_len(nrow(plot_df)), function(i) {
    make_payload(
      as.character(plot_df$type_var[i]),
      plot_df$Year[i],
      plot_df$typeTotal[i]
    )
  }, character(1))

  # The Total trace uses inherit = FALSE, so it needs its own payload.
  total_df$hover_payload <- vapply(seq_len(nrow(total_df)), function(i) {
    make_payload("Total", total_df$Year[i], total_df$total[i], is_total = TRUE)
  }, character(1))

  n_types <- length(unique(plot_df$type_var))
  pal <- palette_vec(n_types)

  subtitle_part <- if (type == "Common name" && !is.null(selected_guild) && nzchar(selected_guild)) {
    paste0(" - ", selected_guild)
  } else {
    ""
  }

  file_stub <- paste0(
    sanitize_stub(ifelse(is.null(ecoregion), "ecoregion", ecoregion)),
    "_landings_",
    sanitize_stub(type),
    if (!is.null(selected_guild) && nzchar(selected_guild)) {
      paste0("_", sanitize_stub(selected_guild))
    } else {
      ""
    },
    "_",
    date_stamp
  )

  # Important: keyed data for click highlighting
  keyed_df <- plotly::highlight_key(plot_df, ~type_var)

  p <- plotly::plot_ly(
    keyed_df,
    x = ~Year,
    y = ~typeTotal,
    customdata = ~hover_payload,
    color = ~type_var,
    colors = pal,
    showlegend = TRUE,
    type = "scatter",
    mode = "lines",
    line = list(width = 3),
    # Hide native labels while retaining hover and click events.
    hoverinfo = "none",
    source = "landings_trends"
  ) %>%
    plotly::add_trace(
      data = total_df,
      x = ~Year,
      y = ~total,
      type = "scatter",
      mode = "lines",
      inherit = FALSE,
      name = "Total",
      line = list(color = "black", width = 3, dash = "dash"),
      customdata = ~hover_payload,
      hoverinfo = "none"
    ) %>%
    plotly::layout(
      hovermode = "closest",
      font = list(size = base_size),
      xaxis = list(
        title = list(text = "Year", font = list(size = axis_title_size)),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      yaxis = list(
        title = list(
          text = "Landings (thousand tonnes)",
          font = list(size = axis_title_size),
          standoff = 18
        ),
        tickfont = list(size = tick_size),
        automargin = TRUE
      ),
      margin = list(l = 80, r = 20, t = 70, b = bottom_margin),
      annotations = list(
        list(
          text = paste0("Landings trends (", ecoregion, subtitle_part, ")"),
          x = 0.01, y = 0.99,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left", yanchor = "top",
          font = list(size = title_annot_size, color = "black")
        ),
        list(
          x = 1, y = 0,
          xref = "paper", yref = "paper",
          xanchor = "right", yanchor = "top",
          yshift = -40,
          text = cap_text,
          showarrow = FALSE,
          align = "right",
          font = list(size = caption_size, color = "black")
        )
      ),
      legend = list(
        title = list(
          text = paste0("<b>", type, ":</b>"),
          font = list(size = legend_title_size)
        ),
        orientation = "h",
        x = 0.5, y = 1.08,
        xanchor = "center", yanchor = "bottom",
        font = list(size = legend_text_size),
        itemwidth = 50
      ),
      hoverlabel = list(font = list(size = base_size))
    ) %>%
    plotly::highlight(
      on = "plotly_click",
      off = "plotly_doubleclick",
      persistent = FALSE,
      dynamic = FALSE,
      selected = plotly::attrs_selected(
        opacity = 1,
        line = list(width = 5)
      )
    ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = file_stub,
        format = "png",
        scale = 3
      )
    )


  # Real HTML/CSS card, matching the shared discards hover implementation.
  # Namespace the CSS so landings and discards tooltips can coexist.
  # Keep a <style> child: add_html_hover() preserves it on every hover.
  renderer <- r"---(
function(el, x) {
  const tooltip = document.createElement('div');
  tooltip.className = 'landings-trends-tooltip';
  tooltip.setAttribute('role', 'tooltip');
  tooltip.style.display = 'none';
  tooltip.innerHTML = `<style>
    .landings-trends-tooltip {
      position:fixed; z-index:2147483000; width:400px;
      max-width:calc(100vw - 20px); max-height:calc(100vh - 20px);
      overflow-y:auto; box-sizing:border-box; padding:15px 16px;
      background:#fff; color:#243746; border:1px solid #d8e2ea;
      border-radius:12px; box-shadow:0 10px 32px rgba(15,33,45,.22);
      font:13px/1.4 -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;
      pointer-events:auto;
    }
    .landings-trends-tooltip * {box-sizing:border-box;}
    .landings-trends-tooltip .lt-head {
      display:flex; justify-content:space-between; align-items:flex-start; gap:14px;
    }
    .landings-trends-tooltip .lt-label {min-width:0; flex:1;}
    .landings-trends-tooltip .lt-name {
      font-size:17px; font-weight:700; line-height:1.25; overflow-wrap:anywhere;
    }
    .landings-trends-tooltip .lt-muted {color:#697b87; font-size:11px;}
    .landings-trends-tooltip .lt-amount {text-align:right; flex-shrink:0;}
    .landings-trends-tooltip .lt-number {
      color:#17354c; font-size:22px; font-weight:750; line-height:1.2;
      font-variant-numeric:tabular-nums; white-space:nowrap;
    }
    .landings-trends-tooltip .lt-context {
      margin-top:8px; color:#697b87; font-size:11px; overflow-wrap:anywhere;
    }
    .landings-trends-tooltip .lt-rule {height:1px; background:#e4eaf0; margin:12px 0;}
    .landings-trends-tooltip .lt-section {font-size:12px; font-weight:700;}
    .landings-trends-tooltip .lt-note {margin-top:4px;}
    .landings-trends-tooltip .lt-taxon {padding:8px 0; border-bottom:1px solid #eef2f5;}
    .landings-trends-tooltip .lt-row {
      display:flex; justify-content:space-between; align-items:baseline; gap:10px;
    }
    .landings-trends-tooltip .lt-species {
      font-size:12px; font-weight:600; overflow-wrap:anywhere; min-width:0;
    }
    .landings-trends-tooltip .lt-value {
      font-size:12px; white-space:nowrap; font-variant-numeric:tabular-nums;
    }
    .landings-trends-tooltip .lt-track {
      width:100%; height:8px; margin-top:5px; background:#e9eef2;
      border-radius:4px; overflow:hidden;
    }
    .landings-trends-tooltip .lt-bar {height:100%; background:#6895af;}
    .landings-trends-tooltip .lt-share {font-size:10.5px; color:#697b87; margin-top:3px;}
    .landings-trends-tooltip .lt-remaining {
      padding:9px 10px; margin-top:10px; background:#f2f6f9; border-radius:7px;
    }
    @media (max-width:420px) {
      .landings-trends-tooltip .lt-head {flex-direction:column; gap:8px;}
      .landings-trends-tooltip .lt-amount {text-align:left;}
    }
  </style>`;

  const safe = function(value) {
    return String(value == null ? '' : value).replace(/[&<>"']/g, function(c) {
      return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c];
    });
  };
  const isNumber = function(value) {
    return typeof value === 'number' && Number.isFinite(value);
  };
  const fmt = function(value, digits) {
    return isNumber(value) ? value.toLocaleString('en-GB', {
      minimumFractionDigits:digits, maximumFractionDigits:digits
    }) : 'Not available';
  };
  // Every bar uses the FULL annual "other" total, including taxa below rank 10.
  // Do not renormalize the displayed ten taxa to 100%.
  const share = function(amount, total) {
    return isNumber(amount) && isNumber(total) && total > 0 ?
      100 * amount / total : null;
  };
  const clamp = function(value) {
    return isNumber(value) ? Math.max(0, Math.min(100, value)) : 0;
  };
  const pct = function(value) {
    return isNumber(value) ? fmt(value, 1) + '%' : 'Share unavailable';
  };

  const build = function(d) {
    const isTotal = d.is_total === true;
    let html = '<div class="lt-head"><div class="lt-label">' +
      '<div class="lt-muted">' + (isTotal ? 'Annual total' : safe(d.group_type)) + '</div>' +
      '<div class="lt-name">' + (isTotal ? 'Total landings' : safe(d.group)) + '</div>' +
      '<div class="lt-muted">Year: ' + safe(d.year) + '</div></div>' +
      '<div class="lt-amount"><div class="lt-number">' + fmt(d.landings_kt, 2) + '</div>' +
      '<div class="lt-muted">thousand tonnes</div></div></div>';

    const context = [];
    if (d.ecoregion) context.push(safe(d.ecoregion));
    if (d.selected_guild) context.push('Fisheries guild: ' + safe(d.selected_guild));
    if (context.length) html += '<div class="lt-context">' + context.join(' · ') + '</div>';

    if (d.has_breakdown === true) {
      const taxa = Array.isArray(d.taxa) ? d.taxa : [];
      html += '<div class="lt-rule"></div>';
      if (taxa.length === 0) {
        html += '<div class="lt-muted">No taxon breakdown available for this year.</div>';
      } else {
        html += '<div class="lt-section">Top ' + taxa.length + ' taxa within “other”</div>' +
          '<div class="lt-muted lt-note">Grouped by common name. Bars show each taxon’s share of all “other” landings in this year.</div>';
        taxa.forEach(function(s, i) {
          const sharePct = share(s.landings_kt, d.landings_kt);
          html += '<div class="lt-taxon"><div class="lt-row">' +
            '<span class="lt-species">' + (i + 1) + '. ' + safe(s.name) + '</span>' +
            '<span class="lt-value">' + fmt(s.landings_kt, 2) + ' kt</span></div>' +
            '<div class="lt-track" role="img" aria-label="' +
            safe(isNumber(sharePct) ? pct(sharePct) + ' of other landings' : 'Share unavailable') + '">' +
            '<div class="lt-bar" style="width:' + clamp(sharePct) + '%"></div></div>' +
            '<div class="lt-share">' +
            (isNumber(sharePct) ? pct(sharePct) + ' of “other”' : 'Share unavailable') +
            '</div></div>';
        });
        if (isNumber(d.additional_taxa) && d.additional_taxa > 0) {
          const remainingPct = share(d.remaining_kt, d.landings_kt);
          html += '<div class="lt-remaining"><div class="lt-row">' +
            '<span class="lt-species">' + safe(d.additional_taxa) + ' additional ' +
            (d.additional_taxa === 1 ? 'taxon' : 'taxa') + '</span>' +
            '<span class="lt-value">' + fmt(d.remaining_kt, 2) + ' kt</span></div>' +
            '<div class="lt-share">' +
            (isNumber(remainingPct) ? pct(remainingPct) + ' of “other”' : 'Share unavailable') +
            '</div></div>';
        }
        html += '<div class="lt-muted lt-note">kt = thousand tonnes</div>';
      }
    }
    return html;
  };

  return {tooltip, build};
}
)---"

  add_html_hover(p, renderer)
}


#' Add a custom HTML hover tooltip to a Plotly widget
#'
#' @description
#' Attach a JavaScript rendering hook that displays a custom HTML tooltip
#' when a Plotly point is hovered. Tooltip content is built from JSON data
#' stored in the point's \code{customdata}. The helper manages hover events,
#' pointer tracking, tooltip positioning, delayed hiding, and cleanup.
#'
#' @param plot A Plotly HTML widget, typically created with
#'   \code{plotly::plot_ly()}.
#' @param renderer A single character string containing a JavaScript function
#'   expression with signature \code{function(el, x)}. Here, \code{el} is the
#'   widget's HTML element and \code{x} is its serialized widget data.
#'   The function must return an object containing \code{tooltip} and
#'   \code{build}, as described below.
#'
#' @section Renderer contract:
#' The JavaScript renderer must return an object with these members:
#' \describe{
#'   \item{\code{tooltip}}{
#'     An HTML element containing a \code{<style>} element. Initialize its
#'     display to \code{"none"} and use \code{position: fixed}, because the
#'     helper positions it using viewport coordinates. The helper appends
#'     this element to \code{document.body}.
#'   }
#'   \item{\code{build}}{
#'     A JavaScript function that accepts the parsed point data and returns
#'     an HTML string containing the tooltip's content.
#'   }
#' }
#' The renderer defines the tooltip's appearance and dimensions. Set suitable
#' maximum width and height and use overflow styling when the content should
#' scroll. Allow pointer interaction with \code{pointer-events: auto} so users
#' can move into the tooltip.
#'
#' On each valid hover, the helper replaces the tooltip's content while
#' preserving its first \code{<style>} element. The HTML returned by
#' \code{build} is inserted using \code{innerHTML}; data-derived text must
#' therefore be escaped by the renderer.
#'
#' @section Point data:
#' Each relevant point must carry a JSON-encoded character string in
#' \code{customdata}. The decoded data should be an object understood by
#' the renderer's \code{build} function.
#'
#' Only the first point in a Plotly hover event is used. The payload is read
#' from \code{point.customdata}, falling back to
#' \code{point.data.customdata[point.pointNumber]} when necessary. If the
#' retrieved payload is an array, its first element is used.
#'
#' Missing or non-string payloads, invalid JSON, and decoded null or
#' non-object values schedule the tooltip to hide.
#'
#' @section Plot configuration:
#' Configure the relevant traces with \code{hoverinfo = "none"} to suppress
#' native Plotly hover labels while retaining hover and click events.
#' Remove any \code{hovertemplate} settings that would override this.
#'
#' Use \code{hovermode = "closest"} in the plot layout so the custom tooltip
#' corresponds to a single hovered point. These trace and layout settings
#' must be applied by the calling plotting function.
#'
#' @section Tooltip behaviour:
#' The tooltip appears near the pointer and follows mouse movement within
#' the plot. Its position is adjusted using the tooltip's measured dimensions
#' and the viewport boundaries.
#'
#' Leaving a hovered point or the tooltip schedules hiding after
#' 180 milliseconds. Entering the tooltip cancels a pending hide, allowing
#' users to interact with its contents.
#'
#' When the rendering hook runs again on the same widget element, the
#' previous tooltip, event listeners, timer, and observer are removed.
#' A JavaScript \code{MutationObserver} also triggers cleanup when the
#' widget element is removed from \code{document.body}.
#'
#' @return The Plotly HTML widget with an additional
#'   \code{htmlwidgets::onRender()} hook for the custom HTML tooltip.
#'
#' @seealso \code{\link[htmlwidgets]{onRender}},
#'   \code{\link[plotly]{plot_ly}}
#'
#' @keywords internal
add_html_hover <- function(plot, renderer) {
  js <- r"---(
  const {tooltip, build} = view;
  const isNumber = value => typeof value === 'number' && Number.isFinite(value);
  if (typeof el.__htmlHoverCleanup === 'function') el.__htmlHoverCleanup();
  document.body.appendChild(tooltip);
  let lastPointer = null;
  let hideTimer = null;
  let observer = null;
  function cancelHide() {
    if (hideTimer !== null) { clearTimeout(hideTimer); hideTimer = null; }
  }
  function hideSoon() {
    cancelHide();
    hideTimer = setTimeout(function() { tooltip.style.display = 'none'; }, 180);
  }
  function position(xPos, yPos) {
    if (!isNumber(xPos) || !isNumber(yPos)) return;
    const pad = 10, offset = 18;
    const tw = tooltip.offsetWidth, th = tooltip.offsetHeight;
    let left = xPos + offset;
    if (left + tw + pad > window.innerWidth) left = xPos - tw - offset;
    left = Math.max(pad, Math.min(left, window.innerWidth - tw - pad));
    let top = yPos + 12;
    if (top + th + pad > window.innerHeight) top = window.innerHeight - th - pad;
    top = Math.max(pad, top);
    tooltip.style.left = left + 'px';
    tooltip.style.top = top + 'px';
  }
  function onMove(event) {
    lastPointer = {x:event.clientX, y:event.clientY};
    if (tooltip.style.display !== 'none') position(event.clientX, event.clientY);
  }
  function onHover(event) {
    const point = event && event.points && event.points[0];
    if (!point) return;
    let payload = point.customdata;
    if (payload == null && point.data && point.data.customdata) {
      payload = point.data.customdata[point.pointNumber];
    }
    if (Array.isArray(payload)) payload = payload[0];
    if (typeof payload !== 'string') { hideSoon(); return; }
    let data;
    try { data = JSON.parse(payload); } catch(error) { hideSoon(); return; }
    if (!data || typeof data !== 'object') { hideSoon(); return; }
    cancelHide();
    tooltip.innerHTML = tooltip.querySelector('style').outerHTML + build(data);
    tooltip.style.display = 'block';
    const mouse = event.event && isNumber(event.event.clientX) ?
      {x:event.event.clientX, y:event.event.clientY} : lastPointer;
    if (mouse) position(mouse.x, mouse.y);
  }
  function onUnhover() { hideSoon(); }
  function onTipEnter() { cancelHide(); }
  function onTipLeave() { hideSoon(); }
  tooltip.addEventListener('mouseenter', onTipEnter);
  tooltip.addEventListener('mouseleave', onTipLeave);
  el.addEventListener('mousemove', onMove);
  el.on('plotly_hover', onHover);
  el.on('plotly_unhover', onUnhover);
  const cleanup = function() {
    cancelHide();
    el.removeEventListener('mousemove', onMove);
    if (typeof el.removeListener === 'function') {
      el.removeListener('plotly_hover', onHover);
      el.removeListener('plotly_unhover', onUnhover);
    }
    tooltip.removeEventListener('mouseenter', onTipEnter);
    tooltip.removeEventListener('mouseleave', onTipLeave);
    if (tooltip.parentNode) tooltip.parentNode.removeChild(tooltip);
    if (observer) observer.disconnect();
    if (el.__htmlHoverCleanup === cleanup) delete el.__htmlHoverCleanup;
  };
  el.__htmlHoverCleanup = cleanup;
  observer = new MutationObserver(function() {
    if (!document.body.contains(el)) cleanup();
  });
  observer.observe(document.body, {childList:true, subtree:true});
)---"
  htmlwidgets::onRender(plot, paste0(
    "function(el, x) { const view = (", renderer, ")(el, x);\n", js, "\n}"
  ))
}





#' Plot discard rate trends by fisheries guild (plotly)
#'
#' Computes and visualises discard rates over time, aggregated by
#' \code{FisheriesGuild}, using \pkg{plotly}. The discard rate is defined
#' as guild discards divided by the sum of guild landings and discards.
#'
#' @param x A data frame containing at least the columns:
#'   \code{Year}, \code{StockKeyLabel}, \code{FisheriesGuild},
#'   \code{Discards}, and \code{Landings}. Additional columns are
#'   ignored.
#' @param year Numeric scalar giving the last assessment year to
#'   include. Years are filtered to \code{2011:(year - 1)}.
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data used for plotting (one row per year–guild) instead
#'   of a plotly object. Default is \code{FALSE}.
#' @param ecoregion Optional character scalar used in the plot title and
#'   in the filename for downloaded images.
#'
#' @return
#' If \code{return_data = TRUE}, a data frame with columns:
#' \describe{
#'   \item{Year}{Year (numeric).}
#'   \item{FisheriesGuild}{Fisheries guild.}
#'   \item{variable}{Currently always \code{"guildRate"}.}
#'   \item{value}{Discard rate (between 0 and 1).}
#' }
#'
#' If \code{return_data = FALSE}, a \pkg{plotly} object showing discard
#' rate trends by fisheries guild is returned, configured with a download
#' button whose filename includes \code{ecoregion} and the current date.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts \code{Year} to numeric, warning and dropping rows
#'     with non-numeric year values.
#'   \item Restricts the data to years from 2011 up to \code{year - 1}.
#'   \item Expands the data so that all year–stock–guild combinations
#'     are present, filling missing entries.
#'   \item Computes total landings and discards per year and guild (in
#'     thousand tonnes).
#'   \item Derives the discard rate as
#'     \code{guildDiscards / (guildLandings + guildDiscards)}.
#'   \item Plots the discard rate over time for each guild as a line
#'     chart, with hover text showing guild, year, and formatted rate.
#' }
#'
#' Font sizes are adjusted heuristically based on the available plot
#' width (if accessible via
#' \code{session$clientData[["output_landings_1-discard_trends_width"]]}),
#' with a fallback width of 800px when not available.
#'
#' @importFrom dplyr mutate filter select distinct group_by summarize left_join
#' @importFrom tidyr expand nesting spread gather pivot_longer
#' @importFrom tibble rowid_to_column
#' @importFrom plotly plot_ly layout config
#' @importFrom scales percent
#' @export
plot_discard_trends_app_plotly <- function(x, year, return_data = FALSE, ecoregion = NULL) {
  
  # Check for non-numeric Year values and warn if any NAs are introduced
  if (all(is.na(x$Discards))) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "No discards available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(size = 20)
          ))
        )
    )
  }


  # --- Responsive font sizes (fallback to 800px)
  w <- tryCatch({
    if (!is.null(session)) session$clientData[["output_landings_1-discard_trends_width"]] else NA_real_
  }, error = function(e) NA_real_)
  if (is.na(w) || is.null(w)) w <- 800

  base_size         <- max(9,  min(18, round(w / 55)))
  axis_title_size   <- max(10, min(20, round(w / 50)))
  tick_size         <- max(9,  min(16, round(w / 55)))
  legend_title_size <- max(10, min(18, round(w / 55)))
  legend_text_size  <- max(9,  min(16, round(w / 65)))
  title_annot_size  <- max(12, min(22, round(w / 40)))
  caption_size      <- max(8,  min(14, round(w / 70)))

  
  year_numeric <- suppressWarnings(as.numeric(x$Year))
  if (any(is.na(year_numeric) & !is.na(x$Year))) {
    warning("Non-numeric values detected in 'Year' column. These rows will be removed.")
  }
  df <- x %>%
    dplyr::mutate(Year = year_numeric) %>%
    dplyr::filter(!is.na(Year)) %>%
    dplyr::filter(Year %in% seq(2011, year - 1))

  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))

  df3 <- df %>%
    dplyr::select(StockKeyLabel, Year, Discards) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Discards = as.numeric(Discards)
    )

  df4 <- df %>%
    dplyr::select(StockKeyLabel, Year, Landings) %>%
    dplyr::distinct() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Landings) %>%
    tidyr::gather(Year, Landings, 4:ncol(.)) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Landings = as.numeric(Landings)
    )
  
  df5 <- df %>%
    dplyr::select(-Discards, -Landings) %>%
    dplyr::left_join(df3, by = c("Year", "StockKeyLabel")) %>%
    dplyr::left_join(df4, by = c("Year", "StockKeyLabel")) %>%
    dplyr::group_by(Year, FisheriesGuild) %>%
    dplyr::summarize(
      guildLandings = sum(Landings, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards, na.rm = TRUE) / 1000,
      .groups = "drop"
    ) %>%
    dplyr::mutate(guildRate = guildDiscards / (guildLandings + guildDiscards)) %>%
    tidyr::pivot_longer(cols = guildRate, names_to = "variable", values_to = "value") %>%
    dplyr::filter(!is.na(value))

  if (return_data) {
    return(df5)
  }

  p <- plotly::plot_ly(
    data = df5,
    x = ~Year,
    y = ~value,
    color = ~FisheriesGuild,
    colors = "Set2",
    type = "scatter",
    mode = "lines",
    line = list(width = 3),
    hoverinfo = "text",
    text = ~ paste(
      "Guild:", FisheriesGuild,
      "<br>Year:", Year,
      "<br>Discard rate:", scales::percent(value, accuracy = 0.1)
    )
  )

  p <- plotly::layout(
    p,
    yaxis = list(
      title = "Discard rate",
      tickformat = ".0%",
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    xaxis = list(
      title = "Year",
      dtick = 1,
      font = list(size = axis_title_size),
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries guild:</b>")),
    margin = list(b = 120),
    annotations = list(
      list(
        xref = "paper",
        yref = "paper",
        xanchor = "right",
        yanchor = "bottom",
        x = 1, y = -0.4,
        showarrow = FALSE,
        text = paste0("ICES Stock Assessment Database,", format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"),
        font = list(size = caption_size)
      ),
      list(
        text = paste0("Discard trends ", " (", ecoregion, ")"),
        x = 0.01, y = 0.98,
        xref = "paper", yref = "paper",
        showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_annot_size, color = "black")
      )
    )
  ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(ecoregion, "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")),
        format   = "png",
        scale    = 3
        # width  = 1600,
        # height = 900
      )
    )

  return(p)
}



#' Plot current landings and discards by fisheries guild (plotly)
#'
#' Aggregates recent landings and discards by \code{FisheriesGuild} and
#' produces a horizontal stacked bar chart for the most recent year
#' (\code{year - 1}). This is intended as a “current snapshot” view,
#' optionally ordered to match an external guild ordering.
#'
#' @param x A data frame containing at least the columns:
#'   \code{Year}, \code{StockKeyLabel}, \code{FisheriesGuild},
#'   \code{Landings}, \code{Catches}, and \code{Discards}. Additional
#'   columns such as \code{FMSY} and \code{MSYBtrigger} are coerced to
#'   numeric but not used directly in the plotting.
#' @param year Numeric scalar giving the assessment year. Data are
#'   restricted to the last five years, \code{seq(year - 5, year - 1)},
#'   and the plot shows only \code{year - 1}.
#' @param position_letter Optional character label (e.g. \code{"A"},
#'   \code{"B"}) used as the panel title, for multi-panel layouts.
#' @param return_data Logical; if \code{TRUE}, the function returns the
#'   processed data (in tonnes) used for the plot instead of a plotly
#'   object. Default is \code{FALSE}.
#' @param order_df Optional data frame used to control the ordering of
#'   \code{FisheriesGuild} on the y-axis. It must contain a
#'   \code{FisheriesGuild} column; its unique values define the factor
#'   level order. Any additional \code{value} column is discarded after
#'   joining.
#' @param ecoregion Optional character scalar used in the filename of
#'   downloaded images (e.g. \code{"Greater North Sea"} or an acronym).
#'
#' @return
#' If \code{return_data = TRUE}, a data frame with columns:
#' \describe{
#'   \item{Year}{Year (numeric).}
#'   \item{FisheriesGuild}{Fisheries guild.}
#'   \item{variable}{Either \code{"guildLandings"} or \code{"guildDiscards"}.}
#'   \item{value}{Landings or discards in tonnes (not thousand tonnes).}
#' }
#'
#' If \code{return_data = FALSE}, a \pkg{plotly} object showing a
#' horizontal stacked bar chart of landings and discards (in thousand
#' tonnes) by fisheries guild for \code{year - 1} is returned. The plot
#' is configured with a download-to-image button whose filename includes
#' \code{ecoregion}, \code{position_letter}, and the current date.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters data to the last five years (\code{year - 5} to
#'     \code{year - 1}) and expands all year–stock–guild combinations.
#'   \item For each stock/year, keeps the record with the highest sum of
#'     \code{Landings + Catches + Discards}, treating \code{NA} as zero.
#'   \item Aggregates landings and discards by \code{Year} and
#'     \code{FisheriesGuild} and converts them to thousand tonnes.
#'   \item For the most recent year (\code{year - 1}), reshapes to a
#'     long format and labels the variables as \code{"Landings"} and
#'     \code{"Discards"} for plotting.
#'   \item Optionally reorders guilds according to \code{order_df}.
#' }
#'
#' Landings and discards are shown in green (\code{"#1d9e76"}) and
#' orange (\code{"#d86003"}), respectively. A caption referencing the
#' ICES Stock Assessment Database is added below the x-axis.
#'
#' @importFrom dplyr mutate filter group_by summarise select left_join top_n
#'   ungroup across
#' @importFrom tidyr expand nesting spread gather
#' @importFrom tibble rowid_to_column
#' @importFrom plotly plot_ly layout config
#' @export
plot_discard_current_plotly <- function(
    x,
    target_year = NULL,
    plot_type = c("withDiscards", "all"),
    return_data = FALSE,
    order_df = NULL,
    ecoregion = NULL,
    region_label = NULL
) {

  plot_type <- match.arg(plot_type)

  if (nrow(x) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "No discards available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(size = 20)
          ))
        )
    )
  }

  df <- x %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      FMSY = as.numeric(FMSY),
      MSYBtrigger = as.numeric(MSYBtrigger),
      Landings = as.numeric(Landings),
      Catches = as.numeric(Catches),
      Discards = as.numeric(Discards)
    ) %>%
    dplyr::filter(!is.na(Year))

  if (nrow(df) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(list(
            text = "No valid year data available",
            xref = "paper", yref = "paper", x = 0.5, y = 0.5,
            showarrow = FALSE, font = list(size = 20)
          ))
        )
    )
  }

  if (is.null(target_year)) {
    target_year <- max(df$Year, na.rm = TRUE)
  }

  df <- df %>%
    dplyr::filter(Year %in% seq(target_year - 4, target_year))

  df2 <- tidyr::expand(df, Year, tidyr::nesting(StockKeyLabel, FisheriesGuild))
  df <- dplyr::left_join(df, df2, by = c("Year", "StockKeyLabel", "FisheriesGuild"))

  df3 <- df %>%
    dplyr::select(StockKeyLabel, Year, Discards) %>%
    unique() %>%
    tibble::rowid_to_column() %>%
    dplyr::group_by(StockKeyLabel) %>%
    tidyr::spread(Year, Discards) %>%
    tidyr::gather(Year, Discards, -StockKeyLabel, -rowid) %>%
    dplyr::mutate(
      Year = as.numeric(Year),
      Discards = as.numeric(Discards)
    )

  df5 <- df %>%
    dplyr::select(-Discards) %>%
    dplyr::left_join(df3, by = c("Year", "StockKeyLabel"))

  df5$sum <- rowSums(df5[, c("Landings", "Catches", "Discards")], na.rm = TRUE)

  df5 <- df5 %>%
    dplyr::group_by(Year, StockKeyLabel) %>%
    dplyr::top_n(1, sum) %>%
    dplyr::ungroup()

  df5 <- df5 %>%
    dplyr::group_by(Year, FisheriesGuild) %>%
    dplyr::summarise(
      dplyr::across(where(is.numeric), ~ sum(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      guildLandings = dplyr::if_else(!is.na(Landings), Landings, Catches),
      guildDiscards = Discards
    ) %>%
    dplyr::mutate(
      guildLandings = as.numeric(guildLandings),
      guildDiscards = as.numeric(guildDiscards)
    ) %>%
    tidyr::pivot_longer(
      cols = c(guildLandings, guildDiscards),
      names_to = "variable",
      values_to = "value"
    ) %>%
    dplyr::filter(Year == target_year) %>%
    dplyr::mutate(value = value / 1000)

  if (!is.null(order_df)) {
    unique_levels <- unique(order_df$FisheriesGuild)

    df5 <- df5 %>%
      dplyr::left_join(order_df, by = "FisheriesGuild") %>%
      dplyr::mutate(FisheriesGuild = factor(FisheriesGuild, levels = unique_levels)) %>%
      dplyr::select(-value.y) %>%
      dplyr::rename(value = value.x)
  }

  if (return_data) {
    df5$value <- df5$value * 1000
    return(df5)
  }

  df5 <- df5 %>%
    dplyr::mutate(
      variable = dplyr::recode(
        variable,
        guildLandings = "Landings",
        guildDiscards = "Discards"
      )
    )

  title_prefix <- switch(
    plot_type,
    withDiscards = "Current discards & Landings\nStocks with recorded discards",
    all = "Current discards & Landings\nAll Stocks"
  )

  title_text <- paste0(
    title_prefix,
    " (", target_year,
    if (!is.null(region_label)) paste0(", ", region_label) else "",
    ")"
  )

  color_scale <- c("Landings" = "#1d9e76", "Discards" = "#d86003")

  plot <- plotly::plot_ly(
    data = df5,
    x = ~value,
    y = ~FisheriesGuild,
    color = ~variable,
    colors = color_scale,
    type = "bar",
    orientation = "h"
  ) %>%
    plotly::layout(
      barmode = "stack",
      title = list(text = title_text, font = list(size = 15)),
      xaxis = list(
        title = "Discards and Landings (thousand tonnes)",
        font = list(size = 14),
        tickfont = list(size = 13)
      ),
      yaxis = list(
        title = "Fisheries Guild",
        tickfont = list(size = 13)
      ),
      showlegend = TRUE,
      margin = list(l = 20, r = 20, t = 50, b = 140),
      annotations = list(
        list(
          xref = "paper",
          yref = "paper",
          xanchor = "right",
          yanchor = "bottom",
          x = 1, y = -0.5,
          showarrow = FALSE,
          text = paste0(
            "ICES Stock Assessment Database,\n",
            format(Sys.Date(), "%d-%b-%y"),
            ". ICES, Copenhagen"
          ),
          font = list(size = 12)
        )
      )
    ) %>%
    plotly::config(
      responsive = TRUE,
      toImageButtonOptions = list(
        filename = paste0(
          ecoregion, "_CurrentDiscards_", plot_type, "_", target_year, "_",
          format(Sys.Date(), "%d-%b-%y")
        ),
        format = "png",
        scale = 3
      )
    )

  return(plot)
}