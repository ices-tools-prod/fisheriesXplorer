

library(dplyr)
library(jsonlite)
library(tidyr)
library(stringr)
library(ggplot2)

#Sarah code drafing
bycatch <- jsonlite::fromJSON(
  URLencode(
    sprintf("https://bycatch.ices.dk/API/GetAdviceResults")
  ))

#Prep -- need a function

#add unique metier-species labels
bycatch <- bycatch %>% dplyr::mutate(label = paste(toupper(bycatch$metier_L4),str_to_sentence(bycatch$common_name),sep=" and "))
#change gear to upper case
bycatch <- bycatch %>% dplyr::mutate(metier_L4 = toupper(metier_L4))
#change ecoregion to upper case
bycatch <- bycatch %>% dplyr::mutate(ecoregion = str_to_title(ecoregion)) 
bycatch <- bycatch %>% dplyr::mutate(ecoregion = replace(ecoregion, ecoregion == "Bay Of Biscay And The Iberian Coast", "Bay of Biscay and the Iberian Coast"))
bycatch <- bycatch %>% dplyr::mutate(taxon = str_to_title(taxon))
#eliminate uncertain bycatch estimates, turn to numeric
bycatch <- bycatch %>% dplyr::mutate(bycatch_2024 = as.numeric(bycatch_2024), bycatch_lower_CI = as.numeric(bycatch_lower_CI), bycatch_upper_CI = as.numeric(bycatch_upper_CI))


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

plot_bpue_fish <- function(EcoR){
  data_subset <- subset(
    bycatch,
    !is.na(bpuE_Numeric) &
      EcoR == ecoregion &
      (taxon %in% c("Elasmobranchs", "Fish"))
  )
  
  if (nrow(data_subset) == 0) {
    message("Skipping ", EcoR, " because no data after filtering.")
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


plot_bycatch_notFish <- function(EcoR){
  data_subset <- subset(
    bycatch,
    !is.na(bycatch_2024) &
      EcoR == ecoregion &
      (taxon %in% c("Seabirds", "Turtles", "Mammals"))
  )
  
  if (nrow(data_subset) == 0) {
    message("Skipping ", ecoregion, " because no data after filtering.")
    next
  }
  ggplot(data_subset, aes(x = label, y = bycatch_2024)) +
    geom_crossbar(aes(ymin = bycatch_lower_CI, ymax = bycatch_upper_CI, fill = metier_L4),
                  width = 0.5, fatten = 0.5) +
    scale_fill_manual(values = metier_palette, name = "Metier level 4") +
    facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
    coord_flip() + theme_classic() +
    xlab("Metier level 4, Species") +
    ylab("Total Bycatch in 2024 (individuals)")
  
}

plot_bycatch_fish <- function(EcoR){
  data_subset <- subset(
    bycatch,
    !is.na(bycatch_2024) &
      EcoR == ecoregion &
      (taxon %in% c("Elasmobranchs", "Fish"))
  )
  
  if (nrow(data_subset) == 0) {
    message("Skipping ", EcoR, " because no data after filtering.")
    next
  }
  ggplot(data_subset, aes(x = label, y = bycatch_2024)) +
    geom_crossbar(aes(ymin = bycatch_lower_CI, ymax = bycatch_upper_CI, fill = metier_L4),
                  width = 0.5, fatten = 0.5) +
    scale_fill_manual(values = metier_palette, name = "Metier level 4") +
    facet_grid(taxon ~ ., scales = "free_y", space = "free_y") +
    coord_flip() + theme_classic() +
    xlab("Metier level 4, Species") +
    ylab("Total Bycatch in 2024 (individuals)")
}





#do we want the Ecoregion to be printed?
#ecoregions <- c("Oceanic Northeast Atlantic","Azores","Greenland Sea","Icelandic Waters","Norwegian Sea","Barents Sea",
#  "Celtic Seas", "Faroes","Greater North Sea","Baltic Sea","Bay of Biscay and the Iberian Coast")
#note that we don't currently have bycatch estimates for Faroes or Greenland Sea