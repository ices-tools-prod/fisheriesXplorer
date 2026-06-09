# ## code to prepare `catchScenarioStock` dataset goes here
# library(dplyr)
# library(icesTAF)

# # catchScenarioStk <- read.table("./data-raw/GNS/catchScenStk.csv")

# # usethis::use_data(catchScenarioStk, overwrite = TRUE)


# # function to download data from github
# download_github_data <- function(repo_owner, repo_name, file_path) {
#     # Fetch file metadata from GitHub API
#     response <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}", 
#                    owner = repo_owner, 
#                    repo = repo_name, 
#                    path = file_path)
    
#     # Extract raw file URL
#     download_url <- response$download_url
    
#     # Download and read the file
#     df <- read.csv(download_url, sep = c(",", ";"), header = TRUE)
    
#     return(df)
# }




# # Define the regions to download data for
# regions <- c("NrS","CS","IrS","IW", "BoB")


# #### catchScenarioStk
# ## download data from github for each region
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/Figure1_HeadlinePlot_data.csv")
  
#   # Download the data from GitHub
#   assign(paste0("catchScenarioStk_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
# }

# # Combine the data into a single data frame
# catchScenarioStk <- do.call(rbind, lapply(regions, function(region) {
#   df <- get(paste0("catchScenarioStk_", region))
#   df <- df %>% select(stock, scenario, catch)
#   # if region is CS, rename df$ecoregion to CSx, else df$ecoregion = region
#   if (region == "CS") {
#     df$ecoregion <- paste0(region, "x")
#   } else {
#     df$ecoregion <- region
#   }
#   # df$ecoregion <- region  # Add the ecoregion column

#   return(df)
# }))


# # Save the combined data frame as a rda file
# save(catchScenarioStk, file = "data/catchScenarioStk.rda")



# #### catchRange
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/Figure1_HeadlinePlot_advice.csv")
  
#   # Download the data from GitHub
#   assign(paste0("catchRange_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  
# }


# # Combine the data into a single data frame
# catchRange <- do.call(rbind, lapply(regions, function(region) {
#   df <- get(paste0("catchRange_", region))
#   df <- df %>% select(stock, advice, lower,  upper)
#   if (region == "CS") {
#     df$ecoregion <- paste0(region, "x")
#   } else {
#     df$ecoregion <- region
#   }
#   return(df)
# }))

# save(catchRange, file = "data/catchRange.rda")

# #### Figure2_EffortByFleet_data
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/Figure2_EffortByFleet_data.csv")
  
#   # Download the data from GitHub
#   assign(paste0("EffortByFleetStock_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  
# }


# # Combine the data into a single data frame
# EffortByFleetStock <- bind_rows(lapply(regions, function(region) {
#   df <- get(paste0("EffortByFleetStock_", region))
  
#   # Coerce known columns to consistent types
#   if ("X" %in% colnames(df)) df$X <- as.character(df$X)

#   if (region == "CS") {
#     df$ecoregion <- paste0(region, "x")
#   } else {
#     df$ecoregion <- region
#   }
#   return(df)
# }))

# save(EffortByFleetStock, file = "data/EffortByFleetStock.rda")

# #### Figure3_landByMetStock_data
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/Figure3_MetierLandings.csv")
  
#   # Download the data from GitHub
#   assign(paste0("MetierStockLandings_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  
# }


# # Combine the data into a single data frame
# MetierStockLandings <- bind_rows(lapply(regions, function(region) {
#   df <- get(paste0("MetierStockLandings_", region))
  
#   # Coerce known columns to consistent types
#   if ("X" %in% colnames(df)) df$X <- as.character(df$X)

#   if (region == "CS") {
#     df$ecoregion <- paste0(region, "x")
#   } else {
#     df$ecoregion <- region
#   }
#   return(df)
# }))

# save(MetierStockLandings, file = "data/MetierStockLandings.rda")

# #### Figure4_landByStock_data
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/Figure4_StockLandings.csv")
  
#   # Download the data from GitHub
#   assign(paste0("StockLandings_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
  
# }


# # Combine the data into a single data frame
# StockLandings <- bind_rows(lapply(regions, function(region) {
#   df <- get(paste0("StockLandings_", region))
  
#   # Coerce known columns to consistent types
#   if ("X" %in% colnames(df)) df$X <- as.character(df$X)

#   if (region == "CS") {
#     df$ecoregion <- paste0(region, "x")
#   } else {
#     df$ecoregion <- region
#   }
#   return(df)
# }))

# save(StockLandings, file = "data/StockLandings.rda")

# #### reTable
# for (region in regions) {
#   # Construct the file path for the region
#   file_path <- paste0("shiny/refTable.csv")
  
#   # Download the data from GitHub
#   assign(paste0("refTable_", region), download_github_data("ices-taf", paste0("2024_", region,"_MixedFisheriesAdvice"), file_path))
#   }


# # # Combine the data into a single data frame
# refTable <- bind_rows(lapply(regions, function(region) {
#   df <- get(paste0("refTable_", region))
  
#   # Coerce known columns to consistent types
#   if ("ref" %in% colnames(df)) df$ref <- as.character(df$ref)
  
#   # Add ecoregion
#   df$ecoregion <- ifelse(region == "CS", paste0(region, "x"), region)  
  
#   return(df)
# }))
# save(refTable, file = "data/refTable.rda")
# # # Download the data from GitHub
# # NrS_catchScenarioStk <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
# # download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")


# # flref



library(dplyr)
library(readr)
library(purrr)
library(gh)


# download_github_data <- function(repo_owner, repo_name, file_path) {
#   response <- gh::gh(
#     "GET /repos/{owner}/{repo}/contents/{path}",
#     owner = repo_owner,
#     repo = repo_name,
#     path = file_path
#   )

#   csv_text <- response$content |>
#     gsub("\\n", "", x = _) |>
#     base64enc::base64decode() |>
#     rawToChar()

#   tmp <- tempfile(fileext = ".csv")
#   writeLines(csv_text, tmp, useBytes = TRUE)

#   readr::read_csv(tmp, show_col_types = FALSE)
# }
download_github_data <- function(repo_owner, repo_name, file_path) {
  response <- gh::gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = repo_owner,
    repo = repo_name,
    path = file_path
  )

  csv_text <- response$content |>
    gsub("\\n", "", x = _) |>
    base64enc::base64decode() |>
    rawToChar()

  tmp <- tempfile(fileext = ".csv")
  on.exit(unlink(tmp), add = TRUE)

  writeLines(csv_text, tmp, useBytes = TRUE)

  df <- utils::read.csv(
    tmp,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  bad_names <- is.na(names(df)) | names(df) == ""

  if (any(bad_names)) {
    names(df)[bad_names] <- paste0("X", which(bad_names))
  }

  names(df) <- make.unique(names(df))

  df
}

advice_year <- 2025
regions <- c("NrS", "CS", "IrS", "IW", "BoB")

region_to_ecoregion <- function(region) {
  ifelse(region == "CS", "CSx", region)
}

repo_name_for_region <- function(region, year = advice_year) {
  paste0(year, "_", region, "_MixedFisheriesAdvice")
}

download_region_dataset <- function(region, file_path, select_cols = NULL, type_fixes = NULL) {
  df <- download_github_data(
    repo_owner = "ices-taf",
    repo_name = repo_name_for_region(region),
    file_path = file_path
  )

  message("Downloaded: ", repo_name_for_region(region), " / ", file_path)
  print(names(df))

  if (!is.null(select_cols)) {
    df <- df %>%
      dplyr::select(dplyr::all_of(select_cols))
  }

  if (!is.null(type_fixes)) {
    for (col in names(type_fixes)) {
      if (col %in% names(df)) {
        df[[col]] <- type_fixes[[col]](df[[col]])
      }
    }
  }

  df %>%
    dplyr::mutate(ecoregion = region_to_ecoregion(region))
}

download_and_combine_dataset <- function(file_path, select_cols = NULL, type_fixes = NULL) {
  purrr::map_dfr(
    regions,
    download_region_dataset,
    file_path = file_path,
    select_cols = select_cols,
    type_fixes = type_fixes
  )
}



dataset_config <- list(
  catchScenarioStk = list(
    file_path = "shiny/Figure1_HeadlinePlot_data.csv",
    select_cols = c("stock", "scenario", "catch"),
    type_fixes = NULL
  ),

  catchRange = list(
    file_path = "shiny/Figure1_HeadlinePlot_advice.csv",
    select_cols = c("stock", "advice", "lower", "upper"),
    type_fixes = NULL
  ),

  EffortByFleetStock = list(
    file_path = "shiny/Figure2_EffortByFleet_data.csv",
    select_cols = NULL,
    type_fixes = list(X = as.character)
  ),

  MetierStockLandings = list(
    file_path = "shiny/Figure3_MetierLandings.csv",
    select_cols = NULL,
    type_fixes = list(X = as.character)
  ),

  StockLandings = list(
    file_path = "shiny/Figure4_StockLandings.csv",
    select_cols = NULL,
    type_fixes = list(X = as.character)
  ),

  refTable = list(
    file_path = "shiny/refTable.csv",
    select_cols = NULL,
    type_fixes = list(ref = as.character)
  )
)


if (!dir.exists("data")) {
  dir.create("data", recursive = TRUE)
}

for (dataset_name in names(dataset_config)) {
  cfg <- dataset_config[[dataset_name]]

  message("\nPreparing ", dataset_name)
  message("  File: ", cfg$file_path)

  obj <- download_and_combine_dataset(
    file_path = cfg$file_path,
    select_cols = cfg$select_cols,
    type_fixes = cfg$type_fixes
  )

  message("  Dimensions: ", paste(dim(obj), collapse = " x "))
  message("  Columns: ", paste(names(obj), collapse = ", "))

  assign(dataset_name, obj, envir = .GlobalEnv)

  save(
    list = dataset_name,
    file = file.path("data", paste0(dataset_name, ".rda")),
    envir = .GlobalEnv
  )

  message("  Saved: data/", dataset_name, ".rda")
}