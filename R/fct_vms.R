#' Generate filename for vms download bundle
#'
#' @param selected_ecoregion A reactive or function returning the selected ecoregion.
#' @param vms_layer Character string such as "effort" or "sar".
#'
#' @return A function that generates the zip filename.
#' @export
#'
#' @examples
#' selected_ecoregion <- function() "Greater North Sea"
#' vms_bundle_filename(selected_ecoregion, "effort")()
vms_bundle_filename <- function(selected_ecoregion, vms_layer) {
  function() {
    ecoregion <- selected_ecoregion()
    acronym  <- get_ecoregion_acronym(ecoregion)
    date_tag <- format(Sys.Date(), "%d-%b-%y")
    paste0("vms_", vms_layer, "_data_bundle_", acronym, "_", date_tag, ".zip")
  }
}

#' Bundle vms content for download, either effort or sar data
#'
#' @param selected_ecoregion reactive value
#' @param vms_layer character, either "sar" or "effort"
#'
#' @importFrom zip zip zipr
vms_bundle_content <- function(selected_ecoregion, vms_layer) {

    function(file) {
    
    # --- Naming tokens
    ecoregion <- selected_ecoregion()
    acronym <- get_ecoregion_acronym(ecoregion)
    # date_tag <- format(Sys.Date(), "%d-%b-%y")
    
    # --- 1) zipped shapefiles (with acronym + date)
    # shp_zip_path <- file.path("data/", paste0("vms_", vms_layer ,"_", acronym, ".zip"))
    shp_zip_path <- system.file(
      "extdata",
      paste0("vms_", vms_layer, "_", acronym, ".zip"),
      package = "fisheriesXplorer"
    )

    if (shp_zip_path == "") {
      stop("Could not find shapefile zip for ", acronym, " and layer ", vms_layer)
    }
    
    # --- 2) Disclaimer.txt (fixed name; no acronym/date)
    # --- Temp workspace
    td <- tempfile("status_bundle_")
    dir.create(td, showWarnings = FALSE)
    on.exit(unlink(td, recursive = TRUE, force = TRUE), add = TRUE)
    
    
    disc_path_fx <- file.path(td, "Disclaimer_fisheriesXplorer.txt")
    disc_url_fx <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/Disclaimer_fisheriesXplorer.txt"
    if (!safe_download(disc_url_fx, disc_path_fx)) {
      writeLines(c(
        "Disclaimer for fisheriesXplorer.",
        "The official disclaimer could not be fetched automatically.",
        paste("Please see:", disc_url_fx)
      ), con = disc_path_fx)
    }
    
    disc_path_vms <- file.path(td, "Disclaimer_VMS.txt")
    disc_url_vms <- "https://raw.githubusercontent.com/ices-tools-prod/disclaimers/master/disclaimer_vms_data_ouput.txt"
    if (!safe_download(disc_url_vms, disc_path_vms)) {
      writeLines(c(
        "Disclaimer for VMS data output.",
        "The official disclaimer could not be fetched automatically.",
        paste("Please see:", disc_url_vms)
      ), con = disc_path_vms)
    }
    
    # --- 3) Plot image (PNG) of the static pies
    match_pattern <- paste0(acronym, "_", vms_layer, "_")
    image_path <- app_sys(paste0("app/www/vms"))
    vms_files <- list.files(image_path)
    required_files <- vms_files[str_starts(vms_files, pattern = match_pattern)]
    image_path <- paste(image_path, required_files, sep = "/")
    
    
    # --- Zip everything
    files_to_zip <- c(shp_zip_path, disc_path_fx, disc_path_vms, image_path)
    if (requireNamespace("zip", quietly = TRUE) && "zipr" %in% getNamespaceExports("zip")) {
      zipr(zipfile = file, files = files_to_zip)
    } else {
      owd <- setwd(td)
      on.exit(setwd(owd), add = TRUE)
      zip(zipfile = file, files = basename(files_to_zip))
    }
  }
}


#' Function to find and display vms images from www/vms folder
#'
#' @param ecoregion character
#' @param gear fishing gear, character, given by input$fishing_cat_selector
#' @param vms_layer character "sar" or "effort"
#' @param ns namespace from server definition
render_vms <- function(ecoregion, gear, vms_layer, ns){

  
  eco_acronym <- get_ecoregion_acronym(ecoregion)
  gear_name <- str_replace_all(tolower(gear), " ", "_")
  file_name <- paste0(eco_acronym, "_", vms_layer, "_", gear_name, ".png")
  
  # Web path used by img tag
  webpath <- file.path("www/vms", file_name)
  
  # Filesystem path used for existence check
  file_systempath <- app_sys("app/www/vms", file_name)
  
  validate(
    need(file.exists(file_systempath),
         paste("No data available for",
               gear,
               "in the", ecoregion, "ecoregion"))
  )
  
  tags$img(
    id = ns(paste0("vms_", vms_layer, "_layer")),
    src = webpath,
    style = "width: 100%; cursor: pointer;",
    onclick = "toggleFullScreen(this)"
  )
}

