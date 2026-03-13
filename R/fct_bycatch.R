getBycatch_ecoregion <- function(Ecoregion) {
       
        # EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        bycatch <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://bycatch.ices.dk/API/GetAdviceResults?ecoregion=%s", Ecoregion)
                )
        )
        return(bycatch)
}

bycatch <- getBycatch_ecoregion("Celtic Seas")


# #add unique metier-species labels
# bycatch <- bycatch %>% dplyr::mutate(label = paste(toupper(bycatch$metier_L4),str_to_sentence(bycatch$common_name),sep=" and "))
# #change gear to upper case
# bycatch <- bycatch %>% dplyr::mutate(metier_L4 = toupper(metier_L4))
# #change ecoregion to upper case
# bycatch <- bycatch %>% dplyr::mutate(ecoregion = str_to_title(ecoregion)) 
# bycatch <- bycatch %>% dplyr::mutate(ecoregion = replace(ecoregion, ecoregion == "Bay Of Biscay And The Iberian Coast", "Bay of Biscay and the Iberian Coast"))
# bycatch <- bycatch %>% dplyr::mutate(taxon = str_to_title(taxon))
# #eliminate uncertain bycatch estimates, turn to numeric
# bycatch <- bycatch %>% dplyr::mutate(bycatch_2024 = as.numeric(bycatch_2024), bycatch_lower_CI = as.numeric(bycatch_lower_CI), bycatch_upper_CI = as.numeric(bycatch_upper_CI))
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

prepare_bpue_not_fish <- function(df, ecoregion_name) {
  df %>%
    filter(
      ecoregion == ecoregion_name,
      taxon %in% c("Seabirds", "Turtles", "Mammals"),
      !is.na(bpuE_Numeric)
    ) %>%
    arrange(taxon, bpuE_Numeric) %>%
    mutate(
      label = factor(label, levels = unique(label))
    )
}

plot_bpue_not_fish_plotly <- function(df, ecoregion_name, palette = metier_palette) {
  
  data_subset <- prepare_bpue_not_fish(df, ecoregion_name)
  
  if (nrow(data_subset) == 0) {
    return(
      plot_ly() %>%
        layout(
          title = list(text = paste("No BPUE data available for", ecoregion_name)),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  taxa_order <- c("Seabirds", "Turtles", "Mammals")
  taxa_present <- taxa_order[taxa_order %in% unique(data_subset$taxon)]
  
  fig <- subplot(
    lapply(taxa_present, function(current_taxon) {
      
      dat <- data_subset %>%
        filter(taxon == current_taxon) %>%
        arrange(bpuE_Numeric)
      
      dat$label <- factor(dat$label, levels = dat$label)
      
      p <- plot_ly()
      
      for (i in seq_len(nrow(dat))) {
        this_colour <- palette[dat$metier_L4[i]]
        if (is.na(this_colour)) this_colour <- "#999999"
        
        p <- p %>%
          add_segments(
            x = dat$bpuE_lower_CI_Numeric[i],
            xend = dat$bpuE_upper_CI_Numeric[i],
            y = dat$label[i],
            yend = dat$label[i],
            line = list(color = this_colour, width = 2),
            hoverinfo = "skip",
            showlegend = FALSE
          )
      }
      
      p <- p %>%
        add_markers(
          data = dat,
          x = ~bpuE_Numeric,
          y = ~label,
          color = ~metier_L4,
          colors = palette,
          marker = list(size = 9),
          text = ~paste0(
            "<b>Ecoregion:</b> ", ecoregion,
            "<br><b>Taxon:</b> ", taxon,
            "<br><b>Metier:</b> ", metier_L4,
            "<br><b>Species:</b> ", common_name,
            "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
            "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
            "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
          ),
          hoverinfo = "text",
          showlegend = TRUE
        ) %>%
        layout(
          title = list(text = current_taxon, x = 0),
          xaxis = list(title = "BPUE (individuals/DaS)", zeroline = FALSE),
          yaxis = list(title = "", automargin = TRUE, categoryorder = "array", categoryarray = dat$label),
          margin = list(l = 180, r = 20, t = 40, b = 40)
        )
      
      p
    }),
    nrows = length(taxa_present),
    shareX = FALSE,
    titleY = TRUE
  )
  
  fig %>%
    layout(
      title = list(text = paste("Bycatch per unit effort:", ecoregion_name)),
      legend = list(
        title = list(text = "Metier level 4"),
        orientation = "h",
        x = 0,
        y = -0.08
      )
    )
}

plot_bpue_not_fish_gg <- function(df, ecoregion_name, palette = metier_palette) {
  
  data_subset <- prepare_bpue_not_fish(df, ecoregion_name)
  
  if (nrow(data_subset) == 0) {
    return(NULL)
  }
  
  ggplot(
    data_subset,
    aes(
      x = label,
      y = bpuE_Numeric,
      ymin = bpuE_lower_CI_Numeric,
      ymax = bpuE_upper_CI_Numeric,
      fill = metier_L4,
      text = paste0(
        "<b>Ecoregion:</b> ", ecoregion,
        "<br><b>Taxon:</b> ", taxon,
        "<br><b>Metier:</b> ", metier_L4,
        "<br><b>Species:</b> ", common_name,
        "<br><b>BPUE:</b> ", round(bpuE_Numeric, 3),
        "<br><b>Lower CI:</b> ", round(bpuE_lower_CI_Numeric, 3),
        "<br><b>Upper CI:</b> ", round(bpuE_upper_CI_Numeric, 3)
      )
    )
  ) +
    geom_crossbar(
      width = 0.5,
      fatten = 0.6,
      colour = "black"
    ) +
    facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
    coord_flip() +
    scale_fill_manual(
      values = palette,
      na.value = "grey70",
      name = "Metier level 4"
    ) +
    labs(
      x = "Metier level 4 and species",
      y = "Bycatch per unit effort - BPUE (individuals/DaS)"
    ) +
    theme_classic()
}

plot_bpue_not_fish_plotly <- function(df, ecoregion_name, palette = metier_palette) {
  
  p <- plot_bpue_not_fish_gg(df, ecoregion_name, palette)
  
  if (is.null(p)) {
    return(
      plot_ly() %>%
        layout(
          title = list(text = paste("No BPUE data available for", ecoregion_name)),
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE)
        )
    )
  }
  
  ggplotly(p, tooltip = "text") %>%
    layout(
      legend = list(
        title = list(text = "Metier level 4"),
        orientation = "h"
      ),
      margin = list(l = 100, r = 20, t = 40, b = 40)
    )
}

bycatch <- get_bycatch_ecoregion("Celtic Seas") %>%
  clean_bycatch_data()

plot_bpue_not_fish_plotly(bycatch, "Celtic Seas")


















plot_bpue_notFish <- function(EcoR){
  data_subset <- subset(
    bycatch,
    !is.na(bpuE_Numeric) &
      EcoR == ecoregion &
      (taxon %in% c("Seabirds", "Turtles", "Mammals"))
  )
  
  if (nrow(data_subset) == 0) {
    message("Skipping ", ecoregion, " because no data after filtering.")
    next
  }
  ggplot(data_subset, aes(x = label, y = bpuE_Numeric)) +
    geom_crossbar(aes(ymin = bpuE_lower_CI_Numeric, ymax = bpuE_upper_CI_Numeric, fill = metier_L4),
                  width = 0.5, fatten = 0.5) +
    scale_fill_manual(values = metier_palette, name = "Metier level 4") +
    facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
    coord_flip() + theme_classic() +
    xlab("Metier level 4, Species") +
    ylab("Bycatch per unit effort - BPUE (individuals/DaS)")
  
}
