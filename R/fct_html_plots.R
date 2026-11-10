html_plot_catalog <- local({
  catalog <- NULL

  function() {
    if (!is.null(catalog)) return(catalog)

    directory <- app_sys("app/plots")
    read_manifest <- function(filename, columns) {
      manifest <- utils::read.csv(
        file.path(directory, filename),
        colClasses = "character", na.strings = character()
      )
      if (!all(columns %in% names(manifest)) || anyNA(manifest)) {
        stop("Invalid HTML plot manifest: ", filename)
      }
      manifest
    }
    columns <- c("plot_type", "ecoregion_id", "guild_id", "file")
    index <- read_manifest("plot_index.csv", columns)
    report <- read_manifest("plot_export_report.csv", c(columns, "status", "reason"))
    keys <- function(x) paste(x$plot_type, x$ecoregion_id, x$guild_id, sep = "|")
    regions <- c("BtS", "BI", "CS", "NrS", "NwS", "IS", "BrS", "GS", "FO", "ONA", "AZ")
    expected <- rbind(
      expand.grid(
        plot_type = c("discards", "stock_summary", "ges"),
        ecoregion_id = regions, guild_id = "", stringsAsFactors = FALSE
      ),
      expand.grid(
        plot_type = "kobe", ecoregion_id = regions,
        guild_id = c("benthic", "demersal", "elasmobranch", "pelagic", "shellfish", "all_stocks"),
        stringsAsFactors = FALSE
      )
    )
    if (anyDuplicated(keys(index)) || anyDuplicated(keys(report)) ||
        anyDuplicated(index$file) || anyDuplicated(report$file) ||
        !setequal(keys(report), keys(expected)) ||
        any(!grepl("^[A-Za-z0-9_-]+\\.html$", report$file)) ||
        any(!report$status %in% c("exported", "no_data")) ||
        any(report$status == "no_data" & !nzchar(trimws(report$reason)))) {
      stop("The HTML plot manifests contain invalid selections or export outcomes.")
    }
    exported <- report[report$status == "exported", , drop = FALSE]
    if (!setequal(keys(index), keys(exported)) ||
        !identical(index$file[match(keys(exported), keys(index))], exported$file)) {
      stop("The HTML plot index does not match the successful exports.")
    }
    paths <- file.path(directory, index$file)
    if (any(!file.exists(paths)) || any(is.na(file.info(paths)$size) | file.info(paths)$size <= 0)) {
      stop("An indexed HTML plot is missing or empty.")
    }

    catalog <<- list(directory = directory, index = index, report = report)
    catalog
  }
})

html_plot_frame <- function(catalog, plot_type, ecoregion_id, title, guild_id = "") {
  report <- catalog$report
  row <- report[
    report$plot_type == plot_type & report$ecoregion_id == ecoregion_id &
      report$guild_id == guild_id,
    , drop = FALSE
  ]
  if (nrow(row) != 1L) {
    stop("Missing or ambiguous HTML plot selection: ", plot_type, "/", ecoregion_id, "/", guild_id)
  }
  if (row$status == "no_data") {
    shiny::validate(shiny::need(FALSE, paste("No data available for this selection.", row$reason)))
  }
  path <- file.path(catalog$directory, row$file)
  if (!isTRUE(file.exists(path) && file.info(path)$size > 0)) {
    stop("The exported HTML plot is missing or empty: ", row$file)
  }
  shiny::tags$iframe(
    src = paste0("plots/", row$file),
    title = title, loading = "lazy",
    style = "width: 100%; height: 100%; border: 0; display: block;"
  )
}
