# Run devtools::load_all() before running this script

library(icesVMS)
library(icesFO)
library(icesVocab)
library(purrr)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthhires)
library(stringr)
library(ggplot2)

source("R/fct_vms.R")
source("R/fct_helpers.R")
# run source("data-raw/ecoregion.R")
source("data-raw/ecoregion_shapefile.R")

#' Plot effort layer with ecoregion outline and land. plot has data update date stamp
#'
#' @param effort sf spatial object giving fishing effort
#' @param land_shape sf spatial object of land
#' @param fishing_category character vector given by user input. Should be a gear type or "all"
#' @param crs a valid crs string
#' @param ecoregion_name character name of ecoregion
#' @param ecoregion_shape sf spatial object of ecoregion polygon
#' @param yr numeric giving Year of vms data update
#' @param data_update_date character -month and year or more specific to include in plot
#'
#' @import ggplot2
#' @importFrom sf st_transform st_bbox
#' @importFrom dplyr filter
plot_effort_map_app <- function(effort, ecoregion_name, ecoregion_shape, land_shape,
                                fishing_category, crs, data_update_date, yr) {
  
  ecoregion_shape <- st_transform(ecoregion_shape, crs = crs)
  box <- st_bbox(ecoregion_shape)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  
  if (fishing_category != "all") {
    effort <- effort %>% filter(fishing_category_FO == fishing_category)
  }
  
  effort <- effort %>%
    filter(!is.na(mw_fishinghours), is.finite(mw_fishinghours), mw_fishinghours > 0)
  
  if (nrow(effort) == 0) {
    message("No effort data for ", ecoregion_name, " / ", fishing_category)
    return(NULL)
  }
  
  effort <- effort %>%
    mutate(effort_breaks = icesFO:::get_map_breaks(mw_fishinghours))
  
  p <- ggplot() +
    geom_sf(data = ecoregion_shape, color = "grey30", fill = "transparent") +
    geom_sf(data = land_shape, fill = "grey85", color = "grey60") +
    geom_sf(data = effort, aes(fill = effort_breaks, colour = effort_breaks), linewidth = 0.05) +
    scale_fill_viridis_d(
      name = "MW Fishing Hours",
      direction = -1,
      option = "A",
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_colour_viridis_d(
      direction = -1,
      option = "A",
      guide = "none"
    ) +
    theme_bw(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "#d6e8f7", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    labs(caption = paste0("Natural Earth and ICES VMS Data \nVMS data updated ", data_update_date)) +
    ggtitle(
      paste0("Average MW Fishing hours ", paste(yr - 3, yr, sep = "-")),
      subtitle = paste0(ecoregion_name, ": ", fishing_category)
    )
  
  if (fishing_category == "all") {
    p <- p +
      facet_wrap(~fishing_category_FO) +
      theme(strip.text = element_text(size = 11)) +
      ggtitle(
        paste0("Average MW Fishing hours ", paste(yr - 3, yr, sep = "-")),
        subtitle = paste0(ecoregion_name, ": All gears")
      )
  }
  
  p
}


#' Plot sar layer with ecoregion outline and land. plot has data update date stamp
#'
#' @param sar_data sf spatial object giving benthic impact of fishing
#' @param land_shape sf spatial object of land
#' @param sar_layer character vector given by user input. Should be a "surface", "subsurface" or "all"
#' @param crs a valid crs string
#' @param ecoregion_name character name of ecoregion
#' @param ecoregion_shape sf spatial object of ecoregion polygon
#' @param yr numeric giving Year of vms data update
#' @param data_update_date character -month and year or more specific to include in plot
#'
#' @import ggplot2
#' @importFrom sf st_transform st_bbox
#' @importFrom dplyr filter
#' @importFrom stringr str_to_title
plot_sar_map_app <- function(sar_data, ecoregion_name, ecoregion_shape, land_shape,
                             sar_layer, crs, data_update_date, yr) {
  
  sar_data <- sar_data %>%
    dplyr::mutate(
      sar = as.numeric(sar),
      layer = tolower(layer)
    )
  
  if (sar_layer != "all") {
    legend_name <- paste0(stringr::str_to_title(sar_layer), " Swept\nArea Ratio")
    sar_data <- sar_data %>%
      dplyr::filter(layer == sar_layer)
  } else {
    legend_name <- "Swept\nArea Ratio"
  }
  
  sar_data <- sar_data %>%
    dplyr::filter(!is.na(sar), is.finite(sar), sar > 0)
  
  if (nrow(sar_data) == 0) {
    message("No SAR data for ", ecoregion_name, " / ", sar_layer)
    return(NULL)
  }
  
  sar_data <- sar_data %>%
  dplyr::mutate(
    layer = factor(layer, levels = c("surface", "subsurface"))
  )

  sar_data <- sar_data %>%
    dplyr::mutate(sar_breaks = icesFO:::get_map_breaks(sar))
  
  ecoregion_shape <- st_transform(ecoregion_shape, crs = crs)
  box <- st_bbox(ecoregion_shape)
  xlims <- c(box[1], box[3])
  ylims <- c(box[2], box[4])
  
  p <- ggplot() +
    geom_sf(data = ecoregion_shape, color = "grey30", fill = "transparent") +
    geom_sf(data = land_shape, fill = "grey85", color = "grey60") +
    geom_sf(data = sar_data, aes(fill = sar_breaks, colour = sar_breaks), linewidth = 0.05) +
    scale_fill_viridis_d(
      name = legend_name,
      direction = -1,
      option = "A",
      guide = guide_legend(reverse = TRUE)
    ) +
    scale_colour_viridis_d(
      direction = -1,
      option = "A",
      guide = "none"
    ) +
    theme_bw(base_size = 15) +
    theme(
      panel.background = element_rect(fill = "#d6e8f7", colour = NA),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.grid.major = element_line(color = "grey70", linewidth = 0.3),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    coord_sf(crs = crs, xlim = xlims, ylim = ylims) +
    labs(caption = paste0("Natural Earth and ICES VMS Data \nVMS data updated ", data_update_date)) +
    ggtitle(
      paste0("Swept Area Ratio ", paste(yr - 3, yr, sep = "-")),
      subtitle = paste0(ecoregion_name, ": ", stringr::str_to_title(sar_layer), " layer")
    )
  
  if (sar_layer == "all") {
    p <- p +
      facet_wrap(~layer,
      labeller = labeller(layer = c(surface = "Surface", subsurface = "Subsurface"))) +
      theme(strip.text = element_text(size = 11)) +
      ggtitle(
        paste0("Swept Area Ratio ", paste(yr - 3, yr, sep = "-")),
        subtitle = paste0(ecoregion_name, ": Surface and Subsurface layers")
      )
  }
  
  p
}

zip_sf <- function(dat, directory, zip_name, fname) {
  sf::write_sf(dat, paste0(directory, "/", fname, ".shp"))
  files <- dir(directory, pattern = fname, full = TRUE)
  files <- files[tools::file_ext(files) != "png"]
  zip::zip(paste0(directory, "/", zip_name, ".zip"), files)
  file.remove(files)
}

ecoregions <- sort(get_ecoregion_list())
ecoregions <- ecoregions[c(4:7,9:13, 15,16)]

#set year for effort and sar maps
yr <- 2024
# Set resolution for plotting
dpi <- 144 

#plot dimensions
plot_width <- 1300
plot_height <- 1000

# SAR and Effort updated November 2025
vms_data_update = "November 2025"
vms_data_update_short = "Nov-25"

CRS_LAEA_EUROPE <- "+proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"

###### get shapefile for vms plot functions ######
atlantic_land_shp <- ne_countries(scale = 10, type = "map_units", 
                                  continent = c("Europe",
                                                "North America"), returnclass = "sf")

atlantic_land_shp <- atlantic_land_shp[, c("iso_a3", "iso_n3", "admin",
                                           "geometry")]



####### Get effort maps ############## Get effort maps ############## Get effort maps #######

effort_maps <- map(ecoregions, get_effort_map, year = yr)
names(effort_maps) <- ecoregions

gears <- c("Static", "Midwater", "Otter", "Demersal seine","Dredge", "Beam")

wrangle_effort <- function(effort_map) {
  effort_map <-
    effort_map %>%
      dplyr::filter(fishing_category_FO %in% gears) %>%
      dplyr::mutate(
        fishing_category_FO =
          dplyr::recode(fishing_category_FO,
                        Static = "Static gears",
                        Midwater = "Pelagic trawls and seines",
                        Otter = "Bottom otter trawls",
                        `Demersal seine` = "Bottom seines",
                        Dredge = "Dredges",
                        Beam = "Beam trawls"),
        mw_fishinghours = as.numeric(mw_fishinghours)
      ) %>%
      filter(!is.na(mw_fishinghours)) %>% 
      filter(mw_fishinghours != 0)
  
}

effort_maps <- map(effort_maps, wrangle_effort)
effort_maps <- map(effort_maps, ~ mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>%
                     select(-wkt) %>% 
                     st_sf)


usethis::use_data(effort_maps, overwrite = TRUE)

####### Get SAR maps ############## Get SAR maps ############## Get SAR maps #######

sar_maps <- map(ecoregions, get_sar_map, year = yr)
names(sar_maps) <- ecoregions
sar_maps[["Azores"]] <- NULL


sar_maps <- map(sar_maps, ~ {if(!is.null(.)) mutate(., geometry = st_as_sfc(wkt, crs = 4326)) %>% 
                  select(-wkt) %>%
                  st_sf}) 

sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) tidyr::pivot_longer(.x, cols = c(surface_sar, subsurface_sar), values_to = "sar", names_to = "layer"))
sar_maps <- purrr::map(.x = sar_maps, .f = function(.x) dplyr::mutate(.x, layer = factor(dplyr::recode(layer, 
                                                                                                surface_sar = "Surface",
                                                                                                subsurface_sar = "Subsurface"), levels = c("Surface", "Subsurface"))))

usethis::use_data(sar_maps, overwrite = TRUE)



# Make vms effort plots
gear <- c("all", "Static gears", "Pelagic trawls and seines", "Bottom otter trawls", "Bottom seines", "Dredges", "Beam trawls")
for(i in 1:length(ecoregions)){
    
  
  
  ecoregion_name <- get_ecoregion_acronym(ecoregions[i])
  zip_sf(dat = effort_maps[[ecoregions[i]]],
         directory = "data",
         zip_name = paste0("vms_effort_", ecoregion_name),
         fname = paste("vms_effort", ecoregion_name, vms_data_update_short, sep = "_")
          )
  
  for(j in 1:length(gear)) {
  
  gear_name  <- tolower(gear[j])
  gear_name  <- str_replace_all(gear_name, " ", "_")
    
  name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_effort_", gear_name, ".png")
  
  result <- plot_effort_map_app(effort_maps[[ecoregions[i]]], 
                              ecoregion_name = ecoregions[i],
                              ecoregion_shape = ecoregion[[ecoregions[i]]],
                              land_shape = atlantic_land_shp,
                              fishing_category = gear[j],
                              crs = CRS_LAEA_EUROPE,
                              data_update_date = vms_data_update,
                              yr = yr)
  
  if (!is.null(result) && inherits(result, "ggplot")) {
      ragg::agg_png(filename = name_of_file, units = "px",width = plot_width, height = plot_height, res = dpi)
      print(result)
      grDevices::dev.off()
        
  } else {
    message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
  }
  }
}


# Make vms SAR plots

layers <- c("all", "surface", "subsurface")
for(i in 1:length(sar_maps)){
  
  if(!is.null(sar_maps[[i]][1][[1]])){
    
    ecoregion_name <- get_ecoregion_acronym(names(sar_maps[i]))
    zip_sf(dat = sar_maps[[names(sar_maps[i])]],
           directory = "data",
           zip_name = paste0("vms_sar_", ecoregion_name),
           fname = paste("vms_sar", ecoregion_name, vms_data_update_short, sep = "_")
    )
    
    
    
    for(j in 1:length(layers)) {
      
      name_of_file <- paste0("inst/app/www/vms/",ecoregion_name, "_sar_", layers[j], ".png")
      
      result <- plot_sar_map_app(sar_maps[[names(sar_maps[i])]], 
                              ecoregion_name = names(sar_maps[i]), 
                              ecoregion_shape = ecoregion[[names(sar_maps[i])]],
                              land_shape = atlantic_land_shp, 
                              sar_layer = layers[j],
                              crs = CRS_LAEA_EUROPE,
                              data_update_date = vms_data_update,
                              yr = yr)
      
      if (!is.null(result) && inherits(result, "ggplot")) {
        ragg::agg_png(filename = name_of_file, units = "px",width = plot_width, height = plot_height, res = dpi)
        print(result)
        grDevices::dev.off()
      } else {
        message("Not saving ", name_of_file, " — plot creation returned NULL or not a ggplot")
      }
  }
  }
}


