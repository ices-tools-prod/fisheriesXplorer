library(jsonlite)
library(dplyr)
library(tidyr)

#' Fetch SID stock list for a given year and ecoregion
#'
#' Queries the ICES Stock List OData service for the specified
#' \code{year}, filters to the requested ecoregion, and returns a
#' processed stock list data frame. Some assessment keys are
#' hard-coded/expanded for specific stocks, and fisheries guild labels
#' are standardised.
#'
#' @param year Integer or numeric; assessment year to query in the ICES
#'   Stock List service (used in the \code{ActiveYear} filter).
#' @param EcoR Character scalar; ecoregion name to filter on (must match
#'   the \code{EcoRegion} values returned by the service, e.g.
#'   \code{"Greater North Sea"}).
#'
#' @return
#' A data frame (tibble) containing at least the columns:
#' \describe{
#'   \item{StockKeyLabel}{ICES stock identifier.}
#'   \item{EcoRegion}{Ecoregion name (one per row after splitting).}
#'   \item{YearOfLastAssessment}{Year of last assessment.}
#'   \item{AssessmentKey}{Numeric assessment key (non-\code{NA} only).}
#'   \item{StockKeyDescription}{Text description of the stock.}
#'   \item{SpeciesScientificName}{Scientific name of the species.}
#'   \item{SpeciesCommonName}{Common name of the species.}
#'   \item{AdviceCategory}{ICES advice category.}
#'   \item{DataCategory}{ICES data category.}
#'   \item{FisheriesGuild}{Fisheries guild, with \code{"crustacean"}
#'     recoded to \code{"shellfish"}.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Calls the ICES OData endpoint
#'     \code{StockListDWs4} with a filter on \code{ActiveYear == year}
#'     and a restricted set of columns (e.g. \code{StockKeyLabel},
#'     \code{EcoRegion}, \code{AssessmentKey}, \code{FisheriesGuild}, etc.).
#'   \item Parses the JSON response via \code{jsonlite::fromJSON()} and
#'     extracts the \code{$value} array.
#'   \item Ensures \code{EcoRegion} is character and splits multi-valued
#'     ecoregions into separate rows using
#'     \code{tidyr::separate_rows(EcoRegion, sep = ", ")}.
#'   \item Filters rows to the requested \code{EcoR}.
#'   \item For a few specific stocks, calls [add_keys()] to add extra
#'     rows with additional \code{AssessmentKey} values:
#'     \code{"cod.27.46a7d20"} and \code{"cod.21.1.isc"}.
#'   \item Drops any rows with \code{NA} in \code{AssessmentKey}.
#'   \item Recodes \code{FisheriesGuild == "crustacean"} to
#'     \code{"shellfish"}.
#' }
#'
#' This helper is used upstream by the application to fetch the SID
#' (Stock Information Database) for a single ecoregion and year.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @importFrom dplyr mutate filter
#' @importFrom tidyr separate_rows
#' @noRd
getSID <- function(year, EcoR) {
        
        stock_list_long <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel,
                        EcoRegion,
                        YearOfLastAssessment,
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName,
                        AdviceCategory,
                        DataCategory,
                        YearOfLastAssessment,
                        FisheriesGuild", year)
                )
        )$value

        stock_list_long <- stock_list_long %>%
                mutate(EcoRegion = as.character(EcoRegion)) %>%
                tidyr::separate_rows(EcoRegion, sep = ", ")

        stock_list_long <- stock_list_long %>%
                filter(EcoRegion == EcoR)

        ############ Hard coded for some stocks with assessmentComponents
        stock_list_long <- add_keys(stock_list_long, "cod.27.46a7d20", c(19661,19662))
        stock_list_long <- add_keys(stock_list_long, "cod.21.1.isc", c(19605))
        
        stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]
        
        stock_list_long$FisheriesGuild[stock_list_long$FisheriesGuild == "crustacean"] <- "shellfish"
        return(stock_list_long)
} 

#' Add rows by replicating a stock template with new keys
#'
#' Helper to append additional rows to a data frame by copying the first
#' row matching a given stock label and replacing a key column with a
#' vector of new keys.
#'
#' @param df A data frame containing at least the columns
#'   \code{StockKeyLabel} and \code{key_col}.
#' @param stock_label Character scalar used to filter \code{df} by
#'   \code{StockKeyLabel}. The first matching row is used as the template.
#' @param keys Vector of new key values to insert into \code{key_col}
#'   for the replicated rows.
#' @param key_col Character scalar giving the name of the key column to
#'   modify. Defaults to \code{"AssessmentKey"}.
#'
#' @return
#' A data frame consisting of the original \code{df} plus one additional
#' row for each element of \code{keys}, where all other columns are
#' copied from the template row.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{df} to \code{StockKeyLabel == stock_label} and
#'     takes the first row as a template.
#' \item Replicates this template once per element of \code{keys}.
#'   \item Replaces \code{key_col} in these replicated rows with
#'     \code{keys}.
#'   \item Appends the new rows to \code{df} via \code{dplyr::bind_rows()}.
#' }
#'
#' If no rows match \code{stock_label}, the behaviour will depend on
#' the result of the initial filter (typically an error or empty
#' additions), so it is advisable to ensure that at least one template
#' row exists.
#'
#' @examples
#' \dontrun{
#' df2 <- add_keys(
#'   df          = stock_list_long,
#'   stock_label = "cod.27.46a7d20",
#'   keys        = c(12345, 67890)
#' )
#' }
#'
#' @export
add_keys <- function(df, stock_label, keys, key_col = "AssessmentKey") {
          template <- df %>%
            dplyr::filter(StockKeyLabel == stock_label) %>%
            dplyr::slice(1)
          additions <- template[rep(1, length(keys)), ]
          additions[[key_col]] <- keys
          dplyr::bind_rows(df, additions)
        }

#' Fetch latest SAG data for an ecoregion
#'
#' Queries the ICES SAG (Stock Assessment Graphs) API for the latest
#' stock data corresponding to a given ecoregion.
#'
#' @param Ecoregion Character scalar giving the full ecoregion name
#'   (e.g. \code{"Greater North Sea"}, \code{"Baltic Sea"}). This is
#'   converted to the corresponding ICES ecoregion code via
#'   [get_ecoregion_acronym()].
#'
#' @return
#' A list or data frame (as returned by \code{jsonlite::fromJSON()})
#' containing the latest SAG data for the requested ecoregion.
#' The exact structure is determined by the SAG API response.
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Converts \code{Ecoregion} to its ICES acronym using
#'     [get_ecoregion_acronym()].
#'   \item Calls the SAG API endpoint
#'     \code{https://sag.ices.dk/SAG_API/LatestStocks/Download}
#'     with the \code{ecoregion} query parameter set to that acronym.
#'   \item Parses the JSON response via \code{jsonlite::fromJSON()} and
#'     returns the parsed object directly.
#' }
#'
#' This helper is typically used upstream by the application to obtain
#' the latest assessment data for all stocks in a given ecoregion.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom utils URLencode
#' @noRd
getSAG_ecoregion_new <- function(Ecoregion) {
       
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}

getSAG_SettingsEcoregion <- function(Ecoregion) {
        
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag_settings <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag_settings)
}

#' Get acronym for an ICES ecoregion
#'
#' Translates a full ICES ecoregion name into the corresponding
#' three-letter acronym used in the app (e.g. `"Baltic Sea"` → `"BtS"`).
#'
#' @param ecoregion A single character string giving the full
#'   ecoregion name. Must be one of:
#'   `"Baltic Sea"`, `"Bay of Biscay and the Iberian Coast"`,
#'   `"Celtic Seas"`, `"Greater North Sea"`, `"Norwegian Sea"`,
#'   `"Icelandic Waters"`, `"Barents Sea"`, `"Greenland Sea"`,
#'   `"Faroes"`, `"Oceanic Northeast Atlantic"`, or `"Azores"`.
#'
#' @return A character string with the corresponding acronym:
#'   `"BtS"`, `"BI"`, `"CS"`, `"NrS"`, `"NwS"`, `"IS"`, `"BrS"`,
#'   `"GS"`, `"FO"`, `"ONA"`, or `"AZ"`.
#'
#' @details
#' If `ecoregion` does not match any of the supported names,
#' the function raises an error via [base::stop()].
#'
#' @examples
#' get_ecoregion_acronym("Baltic Sea")
#' get_ecoregion_acronym("Greater North Sea")
#'
#' @export
# get_ecoregion_acronym <- function(ecoregion) {
#   switch(ecoregion,
#          "Baltic Sea" = "BtS",
#          "Bay of Biscay and the Iberian Coast" = "BI",
#          "Bay of Biscay" = "BoB",
#          "Iberian Waters" = "IW",
#          "Celtic Seas" = "CS",
#          "Celtic Sea" = "CSx",
#          "Irish Sea" = "IrS",
#          "Greater North Sea" = "NrS",
#          "Norwegian Sea" = "NwS",
#          "Icelandic Waters" = "IS",
#          "Barents Sea" = "BrS",
#          "Greenland Sea" = "GS",
#          "Faroes" = "FO",
#          "Oceanic Northeast Atlantic" = "ONA",
#          "Azores" = "AZ",
#          stop("Unknown ecoregion")
#   )
# }
get_ecoregion_acronym <- function(ecoregion) {
  acr <- switch(ecoregion,
    "Baltic Sea" = "BtS",
    "Bay of Biscay and the Iberian Coast" = "BI",
    "Bay of Biscay" = "BoB",
    "Iberian Waters" = "IW",
    "Celtic Seas" = "CS",
    "Celtic Sea" = "CSx",
    "Irish Sea" = "IrS",
    "Greater North Sea" = "NrS",
    "Norwegian Sea" = "NwS",
    "Icelandic Waters" = "IS",
    "Barents Sea" = "BrS",
    "Greenland Sea" = "GS",
    "Faroes" = "FO",
    "Oceanic Northeast Atlantic" = "ONA",
    "Azores" = "AZ",
    NULL
  )

  # normalize empty results to NA
  if (is.null(acr) || identical(acr, "")) NA_character_ else acr
}

SAGSettings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")

#' Extract proxy reference-point choices from SAG settings
#'
#' Parses the output of the SAG settings web service and extracts user-defined
#' proxy reference-point selections for fishing mortality (F) and spawning
#' stock biomass (SSB) charts. In ICES SAG settings, proxy reference points are
#' defined using `settingKey == 51`, where the `settingValue` indicates which of
#' the available custom reference points (1–4) should replace the default
#' reference points shown in standard SAG graphs.
#'
#' The function filters settings corresponding to fishing mortality and SSB
#' charts (`SAGChartKey == 3` and `SAGChartKey == 4` respectively), expands any
#' comma-separated `settingValue` entries into individual rows, and keeps only
#' valid proxy identifiers (`1`, `2`, `3`, `4`). For each `AssessmentKey` and
#' chart type, the first valid proxy option is retained.
#'
#' The output is reshaped to a wide format with one row per `AssessmentKey`
#' containing proxy choices for the F and SSB charts.
#'
#' @param sag_settings A data frame returned by the SAG settings web service
#'   (e.g. `icesSAG::getSAGSettingsForAStock()` or equivalent bulk download).
#'   The table must contain at least the columns:
#'   `AssessmentKey`, `SAGChartKey`, `settingKey`, and `settingValue`.
#'
#' @return A data frame with one row per `AssessmentKey` and the following columns:
#'   \describe{
#'     \item{AssessmentKey}{Integer assessment identifier.}
#'     \item{choice_3}{Selected proxy reference-point index (1–4) for the fishing
#'     mortality chart (`SAGChartKey == 3`). `NA` if no proxy is defined.}
#'     \item{choice_4}{Selected proxy reference-point index (1–4) for the SSB
#'     chart (`SAGChartKey == 4`). `NA` if no proxy is defined.}
#'   }
#'
#' @details
#' In the SAG system, the setting with `settingKey == 51` determines whether
#' custom reference points should be used in place of the standard reference
#' points (e.g. `FMSY` or `MSYBtrigger`). The value stored in `settingValue`
#' indicates which custom reference point (1–4) should be used.
#'
#' Some settings may contain multiple comma-separated values; in such cases the
#' function selects the first valid option.
#'
#' @examples
#' \dontrun{
#' settings <- icesSAG::getSAGSettingsForAStock(c(18808, 20919))
#' proxy_choices <- extract_custom_refpoint_choices(settings)
#' }
#'
#' @export
extract_custom_refpoint_choices <- function(sag_settings) {
  out <- sag_settings %>%
    dplyr::filter(settingKey == 51, SAGChartKey %in% c(3, 4)) %>%
    dplyr::transmute(
      AssessmentKey = as.integer(AssessmentKey),
      SAGChartKey = as.integer(SAGChartKey),
      settingValue = as.character(settingValue)
    ) %>%
    tidyr::separate_rows(settingValue, sep = ",") %>%
    dplyr::mutate(settingValue = trimws(settingValue)) %>%
    dplyr::filter(settingValue %in% c("1", "2", "3", "4")) %>%
    dplyr::group_by(AssessmentKey, SAGChartKey) %>%
    dplyr::summarise(settingValue = dplyr::first(settingValue), .groups = "drop") %>%
    tidyr::pivot_wider(
      names_from = SAGChartKey,
      values_from = settingValue,
      names_prefix = "choice_"
    )

  if (!"choice_3" %in% names(out)) out$choice_3 <- NA_character_
  if (!"choice_4" %in% names(out)) out$choice_4 <- NA_character_

  out %>%
    dplyr::mutate(
      choice_3 = as.integer(choice_3),
      choice_4 = as.integer(choice_4)
    ) %>%
    dplyr::select(AssessmentKey, choice_3, choice_4)
}

#' Format SAG data and attach fisheries guilds
#'
#' Cleans and enriches raw SAG data by attaching fisheries guild
#' information from SID, filtering out ambiguous multi-purpose stocks,
#' and harmonising stock labels and guild names.
#'
#' @param sag A data frame (or tibble) with SAG data, typically the
#'   output of \code{getSAG_ecoregion_new()}, containing at least
#'   \code{StockKeyLabel}, \code{AssessmentKey}, \code{Purpose}, and
#'   \code{AssessmentComponent}.
#' @param sid A SID data frame, usually from \code{getSID()}, containing
#'   at least \code{AssessmentKey}, \code{FisheriesGuild}, and
#'   \code{YearOfLastAssessment}.
#'
#' @return
#' A data frame with:
#' \describe{
#'   \item{All SAG columns}{from \code{sag}, merged with \code{sid} on
#'     \code{AssessmentKey}.}
#'   \item{FisheriesGuild}{Lower-case fisheries guild, with
#'     \code{"crustacean"} recoded to \code{"shellfish"}.}
#'   \item{StockKeyLabel}{Potentially modified stock label including
#'     \code{AssessmentComponent} (when present) and with any literal
#'     “Substock” removed.}
#' }
#'
#' @details
#' The function:
#' \enumerate{
#'   \item Filters \code{sid} to rows with a non-missing
#'     \code{YearOfLastAssessment} and keeps only
#'     \code{AssessmentKey} and \code{FisheriesGuild}.
#'   \item Merges \code{sag} and \code{sid} via
#'     \code{merge(sag, sid, all.x = TRUE, all.y = FALSE)}.
#'   \item Normalises \code{FisheriesGuild} to lower case and recodes
#'     \code{"crustacean"} to \code{"shellfish"}.
#'   \item Identifies stocks with multiple \code{Purpose} entries (same
#'     \code{StockKeyLabel} but multiple purposes) and removes those
#'     from the output via \code{dplyr::anti_join()} to avoid
#'     ambiguous records.
#'   \item Appends \code{AssessmentComponent} to \code{StockKeyLabel}
#'     where non-empty and strips any trailing “Substock” (case
#'     insensitive).
#' }
#'
#' This helper is intended for internal use when preparing SAG data
#' for plotting and summaries within the application.
#'
#' @importFrom dplyr filter select anti_join
#' @noRd
format_sag <- function(sag, sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sid <- dplyr::select(sid,AssessmentKey, FisheriesGuild)
        
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        
        df1 <-as.data.frame(df1)
        
        # df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # replace the fisheries guild == crustacean with shellfish
        df1$FisheriesGuild[df1$FisheriesGuild == "crustacean"] <- "shellfish"
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        
        out <- dplyr::anti_join(df1, check)

        out$StockKeyLabel <- ifelse(is.na(out$AssessmentComponent) | out$AssessmentComponent == "", out$StockKeyLabel, paste0(out$StockKeyLabel, "_", out$AssessmentComponent))
        out$StockKeyLabel <- gsub("\\s*Substock\\b", "", out$StockKeyLabel, ignore.case = TRUE)
        
        return(out)
}


#' Apply proxy reference points to formatted SAG data
#'
#' Integrates proxy reference-point selections into a formatted SAG reference
#' point dataset. When SAG settings specify that a custom reference point
#' should replace the default reference point, the function overwrites the
#' corresponding values in the dataset.
#'
#' Specifically:
#' - `FMSY` is replaced using the selected custom reference point when a proxy
#'   is defined for the fishing mortality chart (`SAGChartKey == 3`).
#' - `MSYBtrigger` is replaced when a proxy is defined for the spawning stock
#'   biomass chart (`SAGChartKey == 4`).
#'
#' The function also records whether the reference point is a proxy and stores
#' the corresponding proxy reference-point name.
#'
#' @param sag_formatted A formatted SAG reference-point dataset containing
#'   standard reference points and custom reference-point fields. The table must
#'   include the columns:
#'   `AssessmentKey`, `FMSY`, `MSYBtrigger`,
#'   `CustomRefPointName1`–`CustomRefPointName4`, and
#'   `CustomRefPointValue1`–`CustomRefPointValue4`.
#'
#' @param sag_settings A data frame containing SAG settings retrieved from the
#'   SAG settings web service. This is passed internally to
#'   `extract_custom_refpoint_choices()` to determine which proxy reference
#'   points should be applied.
#'
#' @return A modified version of `sag_formatted` with:
#'   \describe{
#'     \item{FMSY}{Possibly replaced by a selected custom reference-point value.}
#'     \item{MSYBtrigger}{Possibly replaced by a selected custom reference-point
#'     value.}
#'     \item{FMSY_is_proxy}{Logical flag indicating whether `FMSY` was replaced
#'     by a proxy reference point.}
#'     \item{MSYB_is_proxy}{Logical flag indicating whether `MSYBtrigger` was
#'     replaced by a proxy reference point.}
#'     \item{FMSY_proxy_name}{Name of the custom reference point used as proxy,
#'     if applicable.}
#'     \item{MSYB_proxy_name}{Name of the custom reference point used as proxy,
#'     if applicable.}
#'   }
#'
#' @details
#' Proxy reference points are defined in SAG settings using `settingKey == 51`.
#' The numeric value (1–4) indicates which of the custom reference points stored
#' in the SAG reference-point dataset should be used.
#'
#' The function:
#' \enumerate{
#'   \item Extracts proxy selections from the SAG settings table.
#'   \item Joins these selections to the formatted SAG dataset using
#'   `AssessmentKey`.
#'   \item Replaces `FMSY` and/or `MSYBtrigger` with the corresponding custom
#'   reference-point values where proxies are defined.
#' }
#'
#' If no proxy is specified for an assessment, the original reference points
#' remain unchanged.
#'
#' @examples
#' \dontrun{
#' sag_settings <- icesSAG::getSAGSettingsForAStock(assessment_keys)
#'
#' sag_final <- add_proxyRefPoints(
#'   sag_formatted = sag_refpts,
#'   sag_settings = sag_settings
#' )
#' }
#'
#' @export
add_proxyRefPoints <- function(sag_formatted, sag_settings) {
  cust_choice <- extract_custom_refpoint_choices(sag_settings)
  
  sag_formatted %>%
    dplyr::left_join(cust_choice, by = "AssessmentKey") %>%
    dplyr::mutate(
      dplyr::across(
        c(FMSY, MSYBtrigger, dplyr::starts_with("CustomRefPointValue")),
        ~ suppressWarnings(as.numeric(.x))
      )
    ) %>%
    dplyr::mutate(
      FMSY_proxy_name = dplyr::case_when(
        choice_3 == 1 ~ CustomRefPointName1,
        choice_3 == 2 ~ CustomRefPointName2,
        choice_3 == 3 ~ CustomRefPointName3,
        choice_3 == 4 ~ CustomRefPointName4,
        TRUE ~ NA_character_
      ),
      MSYB_proxy_name = dplyr::case_when(
        choice_4 == 1 ~ CustomRefPointName1,
        choice_4 == 2 ~ CustomRefPointName2,
        choice_4 == 3 ~ CustomRefPointName3,
        choice_4 == 4 ~ CustomRefPointName4,
        TRUE ~ NA_character_
      )
    ) %>%
    dplyr::mutate(
      FMSY_is_valid_proxy = !is.na(FMSY_proxy_name) &
        !grepl("custom|loss|mgt|mp|pa|lim|lowerbound|F/F",
               FMSY_proxy_name, ignore.case = TRUE),
      MSYB_is_valid_proxy = !is.na(MSYB_proxy_name) &
        !grepl("custom|loss|mgt|mp|pa|lim|lowerbound|F/F",
               MSYB_proxy_name, ignore.case = TRUE)
    ) %>%
    dplyr::mutate(
      FMSY_is_proxy = !is.na(choice_3) & FMSY_is_valid_proxy,
      MSYB_is_proxy = !is.na(choice_4) & MSYB_is_valid_proxy,
      FMSY = dplyr::coalesce(
        dplyr::case_when(
          FMSY_is_proxy & choice_3 == 1 ~ CustomRefPointValue1,
          FMSY_is_proxy & choice_3 == 2 ~ CustomRefPointValue2,
          FMSY_is_proxy & choice_3 == 3 ~ CustomRefPointValue3,
          FMSY_is_proxy & choice_3 == 4 ~ CustomRefPointValue4,
          TRUE ~ NA_real_
        ),
        FMSY
      ),
      MSYBtrigger = dplyr::coalesce(
        dplyr::case_when(
          MSYB_is_proxy & choice_4 == 1 ~ CustomRefPointValue1,
          MSYB_is_proxy & choice_4 == 2 ~ CustomRefPointValue2,
          MSYB_is_proxy & choice_4 == 3 ~ CustomRefPointValue3,
          MSYB_is_proxy & choice_4 == 4 ~ CustomRefPointValue4,
          TRUE ~ NA_real_
        ),
        MSYBtrigger
      ),
      FMSY_proxy_name = dplyr::if_else(FMSY_is_proxy, FMSY_proxy_name, NA_character_),
      MSYB_proxy_name = dplyr::if_else(MSYB_is_proxy, MSYB_proxy_name, NA_character_)
    ) %>%
    dplyr::select(
      -dplyr::starts_with("choice_"),
      -FMSY_is_valid_proxy,
      -MSYB_is_valid_proxy
    )
}


sid <- getSID(2025, "Bay of Biscay and the Iberian Coast")
sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")
SAGSettings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")

sag_formatted <- format_sag(sag, sid)
sag_new_refpts <- add_proxyRefPoints(sag_formatted, SAGSettings)


stocks_with_proxies <- sag_new_refpts %>%
  dplyr::filter(FMSY_is_proxy | MSYB_is_proxy) %>%
  dplyr::select(StockKeyLabel, AssessmentKey, FMSY_is_proxy, MSYB_is_proxy, FMSY_proxy_name, MSYB_proxy_name)
