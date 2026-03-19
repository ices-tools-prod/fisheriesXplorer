
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


clean_bycatch_data <- function(df) {
  df %>%
    mutate(
      metier_L4 = toupper(metier_L4),
      ecoregion = str_to_title(ecoregion),
      ecoregion = if_else(
        ecoregion == "Bay Of Biscay And The Iberian Coast",
        "Bay of Biscay and the Iberian Coast",
        ecoregion
      ),
      taxon = str_to_title(taxon),
      common_name = str_to_sentence(common_name),
      label = paste(metier_L4, common_name, sep = " and "),
      bycatch_2024 = as.numeric(bycatch_2024),
      bycatch_lower_CI = as.numeric(bycatch_lower_CI),
      bycatch_upper_CI = as.numeric(bycatch_upper_CI),
      bpuE_Numeric = as.numeric(bpuE_Numeric),
      bpuE_lower_CI_Numeric = as.numeric(bpuE_lower_CI_Numeric),
      bpuE_upper_CI_Numeric = as.numeric(bpuE_upper_CI_Numeric)
    )
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

# prepare_bpue_not_fish <- function(df, ecoregion_name) {
#   df %>%
#     filter(
#       ecoregion == ecoregion_name,
#       taxon %in% c("Seabirds", "Turtles", "Mammals"),
#       !is.na(bpuE_Numeric)
#     ) %>%
#     arrange(taxon, bpuE_Numeric) %>%
#     mutate(
#       label = factor(label, levels = unique(label))
#     )
# }

# plot_bpue_not_fish_plotly <- function(df, ecoregion_name, palette = metier_palette) {
  
#   data_subset <- prepare_bpue_not_fish(df, ecoregion_name)
  
#   if (nrow(data_subset) == 0) {
#     return(
#       plot_ly() %>%
#         layout(
#           title = list(text = paste("No BPUE data available for", ecoregion_name)),
#           xaxis = list(visible = FALSE),
#           yaxis = list(visible = FALSE)
#         )
#     )
#   }
  
#   taxa_order <- c("Seabirds", "Turtles", "Mammals")
#   taxa_present <- taxa_order[taxa_order %in% unique(data_subset$taxon)]
  
#   fig <- subplot(
#     lapply(taxa_present, function(current_taxon) {
      
#       dat <- data_subset %>%
#         filter(taxon == current_taxon) %>%
#         arrange(bpuE_Numeric)
      
#       dat$label <- factor(dat$label, levels = dat$label)
      
#       p <- plot_ly()
      
#       for (i in seq_len(nrow(dat))) {
#         this_colour <- palette[dat$metier_L4[i]]
#         if (is.na(this_colour)) this_colour <- "#999999"
        
#         p <- p %>%
#           add_segments(
#             x = dat$bpuE_lower_CI_Numeric[i],
#             xend = dat$bpuE_upper_CI_Numeric[i],
#             y = dat$label[i],
#             yend = dat$label[i],
#             line = list(color = this_colour, width = 2),
#             hoverinfo = "skip",
#             showlegend = FALSE
#           )
#       }
      
#       p <- p %>%
#         add_markers(
#           data = dat,
#           x = ~bpuE_Numeric,
#           y = ~label,
#           color = ~metier_L4,
#           colors = palette,
#           marker = list(size = 9),
#           text = ~paste0(
#             "<b>Ecoregion:</b> ", ecoregion,
#             "<br><b>Taxon:</b> ", taxon,
#             "<br><b>Metier:</b> ", metier_L4,
#             "<br><b>Species:</b> ", common_name,
#             "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
#             "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
#             "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
#           ),
#           hoverinfo = "text",
#           showlegend = TRUE
#         ) %>%
#         layout(
#           title = list(text = current_taxon, x = 0),
#           xaxis = list(title = "BPUE (individuals/DaS)", zeroline = FALSE),
#           yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = dat$label),
#           margin = list(l = 180, r = 20, t = 40, b = 40)
#         )
      
#       p
#     }),
#     nrows = length(taxa_present),
#     shareX = FALSE,
#     titleY = TRUE
#   )
  
#   fig %>%
#     layout(
#       title = list(text = paste("Bycatch per unit effort:", ecoregion_name)),
#       legend = list(
#         title = list(text = "Metier level 4"),
#         orientation = "h",
#         x = 0,
#         y = -0.08
#       )
#     )
# }

# plot_bpue_not_fish_gg <- function(df, ecoregion_name, palette = metier_palette) {
  
#   data_subset <- prepare_bpue_not_fish(df, ecoregion_name)
  
#   if (nrow(data_subset) == 0) {
#     return(NULL)
#   }
  
#   ggplot(
#     data_subset,
#     aes(
#       x = label,
#       y = bpuE_Numeric,
#       ymin = bpuE_lower_CI_Numeric,
#       ymax = bpuE_upper_CI_Numeric,
#       fill = metier_L4,
#       text = paste0(
#         "<b>Ecoregion:</b> ", ecoregion,
#         "<br><b>Taxon:</b> ", taxon,
#         "<br><b>Metier:</b> ", metier_L4,
#         "<br><b>Species:</b> ", common_name,
#         "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
#         "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
#         "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
#       )
#     )
#   ) +
#     geom_crossbar(
#       width = 0.5,
#       fatten = 0.6,
#       colour = "black"
#     ) +
#     facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
#     coord_flip() +
#     scale_fill_manual(
#       values = palette,
#       na.value = "grey70",
#       name = "Metier level 4"
#     ) +
#     labs(
#       x = "Metier level 4 and species",
#       y = "Bycatch per unit effort - BPUE (individuals/DaS)"
#     ) +
#     theme_classic()
# }


# plot_bpue_not_fish_interactive <- function(df, ecoregion_name, palette = metier_palette) {
  
#   data_subset <- prepare_bpue_not_fish(df, ecoregion_name)
  
#   if (nrow(data_subset) == 0) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = list(text = paste("No BPUE data available for", ecoregion_name)),
#           xaxis = list(visible = FALSE),
#           yaxis = list(visible = FALSE)
#         )
#     )
#   }
  
#   data_subset <- data_subset %>%
#     dplyr::mutate(
#       tooltip = paste0(
#         "<b>Ecoregion:</b> ", ecoregion,
#         "<br><b>Taxon:</b> ", taxon,
#         "<br><b>Metier:</b> ", metier_L4,
#         "<br><b>Species:</b> ", common_name,
#         "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
#         "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
#         "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
#       )
#     )
  
#   p <- ggplot2::ggplot(
#     data_subset,
#     ggplot2::aes(
#       x = label,
#       y = bpuE_Numeric,
#       fill = metier_L4,
#       text = tooltip
#     )
#   ) +
#     ggplot2::geom_linerange(
#       ggplot2::aes(
#         ymin = bpuE_lower_CI_Numeric,
#         ymax = bpuE_upper_CI_Numeric
#       ),
#       linewidth = 0.8,
#       colour = "black"
#     ) +
#     ggplot2::geom_point(
#       shape = 21,
#       size = 3.5,
#       stroke = 0.5,
#       colour = "black"
#     ) +
#     ggplot2::facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
#     ggplot2::coord_flip() +
#     ggplot2::scale_fill_manual(
#       values = palette,
#       na.value = "grey70",
#       name = "Metier level 4"
#     ) +
#     ggplot2::labs(
#       x = "Metier level 4 and species",
#       y = "Bycatch per unit effort - BPUE (individuals/DaS)"
#     ) +
#     ggplot2::theme_classic() +
#     theme(
#   strip.text.y = ggplot2::element_text(
#     angle = 270,
#     hjust = 0.5,
#     margin = ggplot2::margin(r = 10)
#   ),
#   strip.placement = "outside",
#   legend.position = "bottom"
# )
  
#   plotly::ggplotly(
#     p,
#     tooltip = "text"
#   ) %>%
#     plotly::layout(
#       legend = list(
#         orientation = "h",
#         x = 0,
#         y = -0.12
#       )
#     )
# }

# plot_bpue_fish_interactive <- function(df , ecoregion_name, palette = metier_palette) {
  
#   data_subset <- subset(
#     df,
#     !is.na(bpuE_Numeric) &
#       ecoregion_name == ecoregion &
#       (taxon %in% c("Elasmobranchs", "Fish"))
#   )
  
#   if (nrow(data_subset) == 0) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = list(text = paste("No BPUE data available for", ecoregion_name)),
#           xaxis = list(visible = FALSE),
#           yaxis = list(visible = FALSE)
#         )
#     )
#   }
  
#   data_subset <- data_subset %>%
#     dplyr::mutate(
#       tooltip = paste0(
#         "<b>Ecoregion:</b> ", ecoregion,
#         "<br><b>Taxon:</b> ", taxon,
#         "<br><b>Metier:</b> ", metier_L4,
#         "<br><b>Species:</b> ", common_name,
#         "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
#         "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
#         "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
#       )
#     )
  
#   p <- ggplot2::ggplot(
#     data_subset,
#     ggplot2::aes(
#       x = label,
#       y = bpuE_Numeric,
#       fill = metier_L4,
#       text = tooltip
#     )
#   ) +
#     ggplot2::geom_linerange(
#       ggplot2::aes(
#         ymin = bpuE_lower_CI_Numeric,
#         ymax = bpuE_upper_CI_Numeric
#       ),
#       linewidth = 0.8,
#       colour = "black"
#     ) +
#     ggplot2::geom_point(
#       shape = 21,
#       size = 3.5,
#       stroke = 0.5,
#       colour = "black"
#     ) +
#     ggplot2::scale_fill_manual(
#       values = palette,
#       na.value = "grey70",
#       name = "Metier level 4"
#     ) +
#     ggplot2::facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
#     ggplot2::coord_flip() +
#     ggplot2::theme_classic() +
#     ggplot2::labs(
#       x = "Metier level 4, Species",
#       y = "Bycatch per unit effort - BPUE (individuals/DaS)"
#     ) +
#     ggplot2::theme(
#       strip.text.y = ggplot2::element_text(
#         angle = 270,
#         vjust = 0.5,
#         hjust = 0.5,
#         margin = ggplot2::margin(r = 10)
#       ),
#       strip.placement = "outside",
#       legend.position = "bottom"
#     )
  
#   plotly::ggplotly(p, tooltip = "text") %>%
#     plotly::layout(
#       margin = list(l = 120),
#       legend = list(
#         orientation = "h",
#         x = 0,
#         y = -0.12
#       )
#     )
# }
# plot_bycatch_notFish_interactive <- function(df, ecoregion_name, palette = metier_palette) {
  
#   data_subset <- subset(
#     df,
#     !is.na(bycatch_2024) &
#       ecoregion_name == ecoregion &
#       (taxon %in% c("Seabirds", "Turtles", "Mammals"))
#   )
  
#   if (nrow(data_subset) == 0) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = list(text = paste("No bycatch data available for", ecoregion_name)),
#           xaxis = list(visible = FALSE),
#           yaxis = list(visible = FALSE)
#         )
#     )
#   }
  
#   data_subset <- data_subset %>%
#     dplyr::mutate(
#       tooltip = paste0(
#         "<b>Ecoregion:</b> ", ecoregion,
#         "<br><b>Taxon:</b> ", taxon,
#         "<br><b>Metier:</b> ", metier_L4,
#         "<br><b>Species:</b> ", common_name,
#         "<br><b>Total bycatch 2024:</b> ", round(bycatch_2024, 3),
#         "<br><b>Lower CI:</b> ", round(bycatch_lower_CI, 3),
#         "<br><b>Upper CI:</b> ", round(bycatch_upper_CI, 3)
#       )
#     )
  
#   p <- ggplot2::ggplot(
#     data_subset,
#     ggplot2::aes(
#       x = label,
#       y = bycatch_2024,
#       fill = metier_L4,
#       text = tooltip
#     )
#   ) +
#     ggplot2::geom_linerange(
#       ggplot2::aes(
#         ymin = bycatch_lower_CI,
#         ymax = bycatch_upper_CI
#       ),
#       linewidth = 0.8,
#       colour = "black"
#     ) +
#     ggplot2::geom_point(
#       shape = 21,
#       size = 3.5,
#       stroke = 0.5,
#       colour = "black"
#     ) +
#     ggplot2::scale_fill_manual(
#       values = palette,
#       na.value = "grey70",
#       name = "Metier level 4"
#     ) +
#     ggplot2::facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
#     ggplot2::coord_flip() +
#     ggplot2::theme_classic() +
#     ggplot2::labs(
#       x = "Metier level 4, Species",
#       y = "Total Bycatch in 2024 (individuals)"
#     ) +
#     ggplot2::theme(
#       strip.text.y = ggplot2::element_text(
#         angle = 270,
#         vjust = 0.5,
#         hjust = 0.5,
#         margin = ggplot2::margin(r = 10)
#       ),
#       strip.placement = "outside",
#       legend.position = "bottom"
#     )
  
#   plotly::ggplotly(p, tooltip = "text") %>%
#     plotly::layout(
#       margin = list(l = 120),
#       legend = list(
#         orientation = "h",
#         x = 0,
#         y = -0.12
#       )
#     )
# }

# plot_bycatch_fish_interactive <- function(df, ecoregion_name, palette = metier_palette) {
  
#   data_subset <- subset(
#     df,
#     !is.na(bycatch_2024) &
#       ecoregion_name == ecoregion &
#       (taxon %in% c("Elasmobranchs", "Fish"))
#   )
  
#   if (nrow(data_subset) == 0) {
#     return(
#       plotly::plot_ly() %>%
#         plotly::layout(
#           title = list(text = paste("No bycatch data available for", ecoregion_name)),
#           xaxis = list(visible = FALSE),
#           yaxis = list(visible = FALSE)
#         )
#     )
#   }
  
#   data_subset <- data_subset %>%
#     dplyr::mutate(
#       tooltip = paste0(
#         "<b>Ecoregion:</b> ", ecoregion,
#         "<br><b>Taxon:</b> ", taxon,
#         "<br><b>Metier:</b> ", metier_L4,
#         "<br><b>Species:</b> ", common_name,
#         "<br><b>Total bycatch 2024:</b> ", round(bycatch_2024, 3),
#         "<br><b>Lower CI:</b> ", round(bycatch_lower_CI, 3),
#         "<br><b>Upper CI:</b> ", round(bycatch_upper_CI, 3)
#       )
#     )
  
#   p <- ggplot2::ggplot(
#     data_subset,
#     ggplot2::aes(
#       x = label,
#       y = bycatch_2024,
#       fill = metier_L4,
#       text = tooltip
#     )
#   ) +
#     ggplot2::geom_linerange(
#       ggplot2::aes(
#         ymin = bycatch_lower_CI,
#         ymax = bycatch_upper_CI
#       ),
#       linewidth = 0.8,
#       colour = "black"
#     ) +
#     ggplot2::geom_point(
#       shape = 21,
#       size = 3.5,
#       stroke = 0.5,
#       colour = "black"
#     ) +
#     ggplot2::scale_fill_manual(
#       values = palette,
#       na.value = "grey70",
#       name = "Metier level 4"
#     ) +
#     ggplot2::facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
#     ggplot2::coord_flip() +
#     ggplot2::theme_classic() +
#     ggplot2::labs(
#       x = "Metier level 4, Species",
#       y = "Total Bycatch in 2024 (individuals)"
#     ) +
#     ggplot2::theme(
#       strip.text.y = ggplot2::element_text(
#         angle = 270,
#         vjust = 0.5,
#         hjust = 0.5,
#         margin = ggplot2::margin(r = 10)
#       ),
#       strip.placement = "outside",
#       legend.position = "bottom"
#     )
  
#   plotly::ggplotly(p, tooltip = "text") %>%
#     plotly::layout(
#       margin = list(l = 120),
#       legend = list(
#         orientation = "h",
#         x = 0,
#         y = -0.12
#       )
#     )
# }

# bycatch <- get_bycatch_ecoregion("Greater North Sea") %>%
#   clean_bycatch_data()

# plot_bpue_not_fish_plotly(bycatch, "Celtic Seas")
# ggplotly(plot_bpue_not_fish_gg(bycatch, "Celtic Seas", metier_palette))
# plot_bpue_not_fish_interactive(bycatch, "Celtic Seas")
# plot_bpue_fish_interactive(bycatch, "Celtic Seas")
# plot_bycatch_notFish_interactive(bycatch, "Celtic Seas")
# plot_bycatch_fish_interactive(bycatch, "Celtic Seas")

# plot_bpue_not_fish_interactive(bycatch, "Greater North Sea")
# plot_bpue_fish_interactive(bycatch, "Greater North Sea")
# plot_bycatch_notFish_interactive(bycatch, "Greater North Sea")
# plot_bycatch_fish_interactive(bycatch, "Greater North Sea")
# plot_bpue_interactive(
#   df = bycatch,
#   taxon = c("Mammals", "Seabirds", "Elasmobranchs")
# )
# plot_bycatch_interactive(
#   df = bycatch,
#   taxon = c("Mammals", "Seabirds", "Elasmobranchs")
# )

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














