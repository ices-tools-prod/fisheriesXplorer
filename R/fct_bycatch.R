
library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(ggplot2)
library(plotly)

get_bycatch_ecoregion <- function(Ecoregion) {
       
        # EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        bycatch <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://bycatch.ices.dk/API/GetAdviceResults?ecoregion=%s", Ecoregion)
                )
        )
        return(bycatch)
}

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
      ecoregion = str_to_title(as.character(ecoregion)),
      ecoregion = if_else(
        ecoregion == "Bay Of Biscay And The Iberian Coast",
        "Bay of Biscay and the Iberian Coast",
        ecoregion
      ),
      taxon = str_to_title(as.character(taxon)),
      common_name = str_to_sentence(as.character(common_name)),
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



#define color palette
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


library(tidytext)

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

plot_bycatch_metric_interactive <- function(df,
                                            taxon,
                                            value_col,
                                            lower_col,
                                            upper_col,
                                            y_label,
                                            empty_title = "No data available",
                                            legend_title = "Metier level 4",
                                            palette = metier_palette) {

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

  p <- ggplot(
    data_subset,
    aes(
      x = label_reordered,
      y = .data[[value_col]],
      fill = metier_L4,
      text = tooltip
    )
  ) +
    geom_linerange(
      aes(
        ymin = .data[[lower_col]],
        ymax = .data[[upper_col]]
      ),
      linewidth = 0.8,
      colour = "black"
    ) +
    geom_point(
      shape = 21,
      size = 3.5,
      stroke = 0.5,
      colour = "black"
    ) +
    facet_wrap(
      ~ taxon,
      ncol = 1,
      scales = "free_x",
      strip.position = "top"
    ) +
    tidytext::scale_x_reordered() +
    scale_fill_manual(
      values = palette,
      na.value = "grey70",
      name = legend_title
    ) +
    labs(
      x = "Metier level 4 and species",
      y = y_label
    ) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
      strip.background = element_blank(),
      strip.placement = "outside",
      strip.text = element_text(hjust = 0),
      legend.position = "bottom"
    )

  ggplotly(p, tooltip = "text") %>%
    layout(
      legend = list(
        orientation = "v",
        x = 1.02,
        y = 0.5
      )
    )
}



plot_bpue_interactive <- function(df,
                                  taxon,
                                  palette = metier_palette) {
  plot_bycatch_metric_interactive(
    df = df,
    taxon = taxon,
    value_col = "bpuE_Numeric",
    lower_col = "bpuE_lower_CI_Numeric",
    upper_col = "bpuE_upper_CI_Numeric",
    y_label = "Bycatch per unit effort - BPUE (individuals/DaS)",
    empty_title = "No BPUE data available",
    palette = palette
  )
}

plot_bycatch_interactive <- function(df,
                                     taxon,
                                     palette = metier_palette) {
  plot_bycatch_metric_interactive(
    df = df,
    taxon = taxon,
    value_col = "bycatch_2024",
    lower_col = "bycatch_lower_CI",
    upper_col = "bycatch_upper_CI",
    y_label = "Total Bycatch in 2024 (individuals)",
    empty_title = "No bycatch data available",
    palette = palette
  )
}














