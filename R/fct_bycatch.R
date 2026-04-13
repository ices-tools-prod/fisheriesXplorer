#' Retrieve bycatch advice results for a given ecoregion
#'
#' Queries the ICES bycatch API and returns the bycatch advice results for a
#' specified ecoregion.
#'
#' @param Ecoregion A character string giving the ecoregion name to be passed to
#'   the ICES bycatch API.
#'
#' @return A data frame or list, depending on the API response structure,
#'   containing bycatch advice results for the requested ecoregion.
#'
#' @details
#' This function builds the API request URL using the supplied ecoregion name,
#' URL-encodes it, and parses the JSON response with `jsonlite::fromJSON()`.
#'
#' It does not perform validation of the API response, so downstream cleaning is
#' usually needed before plotting or analysis.
#'
#' @examples
#' \dontrun{
#' bycatch_raw <- get_bycatch_ecoregion("Greater North Sea")
#' }
#'
#' @export
get_bycatch_ecoregion <- function(Ecoregion) {

        bycatch <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://bycatch.ices.dk/API/GetAdviceResults?ecoregion=%s", Ecoregion)
                )
        )
        return(bycatch)
}


#' Create an empty bycatch data frame
#'
#' Returns an empty data frame with the expected column names and column types
#' used throughout the bycatch workflow.
#'
#' @return A zero-row data frame with standardised columns for ecoregion, taxon,
#'   métier, species name, bycatch values, confidence intervals, and BPUE values.
#'
#' @details
#' This function is useful as a safe fallback when API responses are empty,
#' malformed, or unavailable. It ensures that downstream code can rely on a
#' consistent schema.
#'
#' @examples
#' empty_df <- empty_bycatch_data()
#'
#' @export
empty_bycatch_data <- function() {
  data.frame(
    ecoregion = character(),
    taxon = character(),
    metier_L4 = character(),
    common_name = character(),
    label = character(),
    bycatch_2024 = numeric(),
    bycatch_lower_CI = numeric(),
    bycatch_upper_CI = numeric(),
    bpuE_Numeric = numeric(),
    bpuE_lower_CI_Numeric = numeric(),
    bpuE_upper_CI_Numeric = numeric(),
    stringsAsFactors = FALSE
  )
}


#' Clean and standardise bycatch data
#'
#' Cleans a raw bycatch data frame and returns a standardised version with
#' consistent column names, value types, and formatted labels.
#'
#' @param df A data frame containing raw bycatch data.
#'
#' @return A cleaned data frame with the same standard structure as
#'   `empty_bycatch_data()`.
#'
#' @details
#' The function:
#' \itemize{
#'   \item returns an empty standardised data frame if `df` is `NULL`, not a
#'   data frame, or has zero rows;
#'   \item adds missing required columns and fills them with `NA`;
#'   \item standardises text formatting for ecoregion, taxon, métier, and common
#'   names;
#'   \item creates a combined `label` column from métier and common name;
#'   \item converts numeric measurement columns safely using
#'   `suppressWarnings(as.numeric(...))`;
#'   \item returns only the expected columns in a consistent order.
#' }
#'
#' A special case is included to preserve the preferred capitalisation of
#' `"Bay of Biscay and the Iberian Coast"`.
#'
#' @param df A raw bycatch data frame.
#'
#' @examples
#' \dontrun{
#' bycatch_raw <- get_bycatch_ecoregion("Celtic Seas")
#' bycatch_clean <- clean_bycatch_data(bycatch_raw)
#' }
#'
#' @export
clean_bycatch_data <- function(df) {
  required_cols <- c(
    "ecoregion", "taxon", "metier_L4", "common_name",
    "bycatch_2024", "bycatch_lower_CI", "bycatch_upper_CI",
    "bpuE_Numeric", "bpuE_lower_CI_Numeric", "bpuE_upper_CI_Numeric"
  )

  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(empty_bycatch_data())
  }

  missing_cols <- setdiff(required_cols, names(df))
  if (length(missing_cols) > 0) {
    for (col in missing_cols) {
      df[[col]] <- NA
    }
  }

  df %>%
    mutate(
      metier_L4 = toupper(as.character(metier_L4)),
      ecoregion = stringr::str_to_title(as.character(ecoregion)),
      ecoregion = if_else(
        ecoregion == "Bay Of Biscay And The Iberian Coast",
        "Bay of Biscay and the Iberian Coast",
        ecoregion
      ),
      taxon = stringr::str_to_title(as.character(taxon)),
      common_name = stringr::str_to_sentence(as.character(common_name)),
      label = paste(metier_L4, common_name, sep = " and "),
      bycatch_2024 = suppressWarnings(as.numeric(bycatch_2024)),
      bycatch_lower_CI = suppressWarnings(as.numeric(bycatch_lower_CI)),
      bycatch_upper_CI = suppressWarnings(as.numeric(bycatch_upper_CI)),
      bpuE_Numeric = suppressWarnings(as.numeric(bpuE_Numeric)),
      bpuE_lower_CI_Numeric = suppressWarnings(as.numeric(bpuE_lower_CI_Numeric)),
      bpuE_upper_CI_Numeric = suppressWarnings(as.numeric(bpuE_upper_CI_Numeric))
    ) %>%
    select(any_of(names(empty_bycatch_data())))
}



#' Colour palette for métier level 4 categories
#'
#' A named character vector mapping métier level 4 codes to hexadecimal colour
#' values used in bycatch plots.
#'
#' @format A named character vector.
#'
#' @details
#' The names correspond to métier level 4 codes and the values are hex colour
#' strings. This palette is used as the default fill scale in the interactive
#' bycatch and BPUE plotting functions.
#'
#' @examples
#' metier_palette["OTB"]
#'
#' @export
metier_palette <- c(
  "GTR" = "#e6ab02",
  "LLD" = "#a6761d",
  "OTB" = "#1b9e77",
  "PS" = "#666666",
  "PTM" = "#1f78b4", 
  "GNS" ="#d95f02",
  "LLS"="#66a61e", 
  "FPO"="#b2df8a", 
  "LHP"="#fb9a99", 
  "FPN"="#fdbf6f",
  "FYK"="#cab2d6",
  "OTM"="#e7298a",
  "SDN"="#ffff99",
  "OTT"="#7570b3",
  "GND"="#6a3d9a",
  "GTN"="#ff7f00",
  "LTL"="#b15928",
  "PTB"="#8dd3c7",
  "TBB"="#ffffb3",
  "SSC"="#bebada",
  "DRB"="#fb8072",
  "LHM"="#80b1d3"
)


#' Prepare bycatch data for plotting
#'
#' Filters and formats cleaned bycatch data for use in interactive ggplot/plotly
#' plots.
#'
#' @param df A cleaned bycatch data frame.
#' @param taxa_selected A character vector of one or more taxa to include.
#' @param value_col A character string giving the name of the main value column
#'   to plot.
#' @param lower_col A character string giving the name of the lower confidence
#'   interval column.
#' @param upper_col A character string giving the name of the upper confidence
#'   interval column.
#'
#' @return A data frame filtered to the selected taxa, with reordered labels and
#'   a formatted HTML tooltip column.
#'
#' @details
#' The function:
#' \itemize{
#'   \item checks that `taxa_selected` is a character vector;
#'   \item filters the data to the requested taxa and removes rows with missing
#'   plotting values;
#'   \item converts `taxon` into a factor to preserve plotting order;
#'   \item creates `label_reordered` using `tidytext::reorder_within()` for
#'   within-group ordering;
#'   \item builds an HTML tooltip including ecoregion, taxon, métier, species,
#'   value, and confidence intervals.
#' }
#'
#' This function is intended as an internal data-preparation step for
#' `plot_bycatch_metric_interactive()` and its wrappers.
#'
#' @examples
#' \dontrun{
#' plot_data <- prepare_bycatch_plot_data(
#'   df = bycatch_clean,
#'   taxa_selected = c("Fish"),
#'   value_col = "bycatch_2024",
#'   lower_col = "bycatch_lower_CI",
#'   upper_col = "bycatch_upper_CI"
#' )
#' }
#'
#' @export
prepare_bycatch_plot_data <- function(df,
                                      taxa_selected,
                                      value_col,
                                      lower_col,
                                      upper_col) {

  stopifnot(is.character(taxa_selected), length(taxa_selected) >= 1)

  taxa_selected <- unique(taxa_selected)

  df %>%
    filter(
      .data$taxon %in% taxa_selected,
      !is.na(.data[[value_col]])
    ) %>%
    mutate(
      taxon = factor(.data$taxon, levels = taxa_selected),
      label_reordered = tidytext::reorder_within(
        x = label,
        by = .data[[value_col]],
        within = taxon
      )
    ) %>%
    mutate(
      tooltip = paste0(
        "<b>Ecoregion:</b> ", ecoregion,
        "<br><b>Taxon:</b> ", taxon,
        "<br><b>Metier:</b> ", metier_L4,
        "<br><b>Species:</b> ", common_name,
        "<br><b>Value:</b> ", round(.data[[value_col]], 3),
        "<br><b>Lower CI:</b> ", round(.data[[lower_col]], 3),
        "<br><b>Upper CI:</b> ", round(.data[[upper_col]], 3)
      )
    )
}


#' Create an interactive bycatch metric plot
#'
#' Builds an interactive plotly version of a bycatch plot for a selected metric,
#' including point estimates, confidence intervals, and tooltip information.
#'
#' @param df A cleaned bycatch data frame.
#' @param taxon A character vector of one or more taxa to include.
#' @param value_col A character string giving the name of the value column to
#'   plot.
#' @param lower_col A character string giving the name of the lower confidence
#'   interval column.
#' @param upper_col A character string giving the name of the upper confidence
#'   interval column.
#' @param y_label A character string used as the y-axis label.
#' @param empty_title A character string used as the title when no data are
#'   available. Defaults to `"No data available"`.
#' @param legend_title A character string used as the legend title. Defaults to
#'   `"Metier level 4"`.
#' @param palette A named character vector of fill colours. Defaults to
#'   `metier_palette`.
#'
#' @return A plotly object.
#'
#' @details
#' The function first prepares the data using `prepare_bycatch_plot_data()`. If
#' no rows are available after filtering, it returns an empty plotly object with
#' the supplied `empty_title`.
#'
#' Otherwise, it creates a `ggplot2` plot with:
#' \itemize{
#'   \item confidence intervals shown as vertical line ranges;
#'   \item point estimates shown as filled points;
#'   \item fill colour mapped to métier level 4;
#'   \item interactive HTML tooltips;
#'   \item layout adjustments for legend position and axis margins.
#' }
#'
#' This is the general plotting engine used by `plot_bpue_interactive()` and
#' `plot_bycatch_interactive()`.
#'
#' @examples
#' \dontrun{
#' plot_bycatch_metric_interactive(
#'   df = bycatch_clean,
#'   taxon = "Fish",
#'   value_col = "bycatch_2024",
#'   lower_col = "bycatch_lower_CI",
#'   upper_col = "bycatch_upper_CI",
#'   y_label = "Total bycatch"
#' )
#' }
#'
#' @export
plot_bycatch_metric_interactive <- function(df,
                                            taxon,
                                            value_col,
                                            lower_col,
                                            upper_col,
                                            y_label,
                                            empty_title = "No data available",
                                            legend_title = "Metier level 4",
                                            palette = metier_palette,
                                            ecoregion) {

  data_subset <- prepare_bycatch_plot_data(
    df = df,
    taxa_selected = taxon,
    value_col = value_col,
    lower_col = lower_col,
    upper_col = upper_col
  )

  if (nrow(data_subset) == 0) {
    return(
      plotly::plot_ly() %>%
        plotly::layout(
          title = list(text = empty_title),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }

  cap_text <- paste0(
    "ICES Bycatch database.<br>",
    base::format(base::Sys.Date(), "%d-%b-%y"), ",<br>ICES, Copenhagen."
  )

  p <- ggplot2::ggplot(
    data_subset,
    aes(
      x = label_reordered,
      y = .data[[value_col]],
      fill = metier_L4,
      text = tooltip
    )
  ) +
    ggplot2::geom_linerange(
      aes(
        ymin = .data[[lower_col]],
        ymax = .data[[upper_col]]
      ),
      linewidth = 0.8,
      colour = "black"
    ) +
    ggplot2::geom_point(
      shape = 21,
      size = 4,
      stroke = 0.2,
      colour = "black"
    ) +
    tidytext::scale_x_reordered() +
    ggplot2::scale_fill_manual(
      values = palette,
      na.value = "grey70",
      name = legend_title
    ) +
    ggplot2::labs(
      x = "Metier level 4 and species",
      y = y_label
    ) +
    ggplot2::theme_classic(base_size = 13) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, vjust = 1),
      panel.grid.major = ggplot2::element_line(
        colour = "grey85",
        linewidth = 0.4
      ),
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      legend.position = "top"
    )

  plotly::ggplotly(p, tooltip = "text") %>%
    plotly::layout(
      autosize = TRUE,
      margin = list(l = 110, r = 40, t = 90, b = 140),
      legend = list(
        orientation = "h",
        y = 1.08,
        x = 0.5,
        xanchor = "center",
        yanchor = "bottom"
      ),
      yaxis = list(
        automargin = TRUE,
        title = list(
          text = y_label,
          standoff = 20
        )
      ),
      xaxis = list(
        automargin = TRUE
      ),
      annotations = list(
        list(
          x = 1, y = -0.8, xref = "paper", yref = "paper",
          text = cap_text,
          showarrow = FALSE,
          xanchor = "right",
          yanchor = "bottom",
          font = list(size = 10, color = "black")
        ),
        list(
          text = paste0("Bycatch: ", taxon, " (", ecoregion, ")"),
          x = 0.01, y = 0.99, xref = "paper", yref = "paper",
          showarrow = FALSE,
          xanchor = "left",
          yanchor = "top",
          font = list(size = 18, color = "black")
        )
      )
    )
}

#' Plot interactive BPUE values
#'
#' Convenience wrapper around `plot_bycatch_metric_interactive()` for plotting
#' bycatch per unit effort (BPUE).
#'
#' @param df A cleaned bycatch data frame.
#' @param taxon A character vector of one or more taxa to include.
#' @param palette A named character vector of fill colours. Defaults to
#'   `metier_palette`.
#'
#' @return A plotly object showing BPUE values and their confidence intervals.
#'
#' @details
#' This function uses:
#' \itemize{
#'   \item `bpuE_Numeric` as the plotted value,
#'   \item `bpuE_lower_CI_Numeric` as the lower confidence interval,
#'   \item `bpuE_upper_CI_Numeric` as the upper confidence interval.
#' }
#'
#' @examples
#' \dontrun{
#' plot_bpue_interactive(bycatch_clean, taxon = "Fish")
#' }
#'
#' @export
plot_bpue_interactive <- function(df,
                                  taxon,
                                  palette = metier_palette,
                                  ecoregion) {
  plot_bycatch_metric_interactive(
    df = df,
    taxon = taxon,
    value_col = "bpuE_Numeric",
    lower_col = "bpuE_lower_CI_Numeric",
    upper_col = "bpuE_upper_CI_Numeric",
    y_label = "Bycatch per unit effort \n BPUE (individuals/DaS)",
    empty_title = "No BPUE data available",
    palette = palette,
    ecoregion = get_ecoregion_acronym(ecoregion)
  )
}


#' Plot interactive total bycatch values
#'
#' Convenience wrapper around `plot_bycatch_metric_interactive()` for plotting
#' total bycatch values.
#'
#' @param df A cleaned bycatch data frame.
#' @param taxon A character vector of one or more taxa to include.
#' @param palette A named character vector of fill colours. Defaults to
#'   `metier_palette`.
#'
#' @return A plotly object showing total bycatch values and their confidence
#'   intervals.
#'
#' @details
#' This function uses:
#' \itemize{
#'   \item `bycatch_2024` as the plotted value,
#'   \item `bycatch_lower_CI` as the lower confidence interval,
#'   \item `bycatch_upper_CI` as the upper confidence interval.
#' }
#'
#' @examples
#' \dontrun{
#' plot_bycatch_interactive(bycatch_clean, taxon = c("Fish", "Elasmobranchs"))
#' }
#'
#' @export
plot_bycatch_interactive <- function(df,
                                     taxon,
                                     palette = metier_palette,
                                     ecoregion) {
  plot_bycatch_metric_interactive(
    df = df,
    taxon = taxon,
    value_col = "bycatch_2024",
    lower_col = "bycatch_lower_CI",
    upper_col = "bycatch_upper_CI",
    y_label = "Total Bycatch in 2024 \n (individuals)",
    empty_title = "No bycatch data available",
    palette = palette,
    ecoregion = get_ecoregion_acronym(ecoregion)
  )
}














