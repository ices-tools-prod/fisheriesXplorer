

library(dplyr)
library(future)
library(jsonlite)
library(icesSAG)
library(icesASD)
library(promises)
library(data.table)
library(memoise)
library(future.apply)
library(tidyr)
library(jsonlite)

load_sag_status_new <- function(sag) {
        stocks <- unique(sag[c("AssessmentKey","FishStock")])
        status <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
        status <- do.call(rbind.data.frame, status)
        # stocks$AssessmentKey <- as.character(stocks$AssessmentKey)
        status <- dplyr::left_join(status, stocks)
        status <- dplyr::mutate(status, StockKeyLabel= FishStock)
        status <- subset(status, select = -c(FishStock))
        status <- dplyr::relocate(status, StockKeyLabel, .before = lineNumber)
        status
}

stocks <- icesSAG::getListStocks(year = 2024)
stocks <- unique(stocks[c("AssessmentKey","StockKeyLabel")])
test1 <- icesSAG::getStockStatusValues(stocks$AssessmentKey)
test2 <- do.call(rbind.data.frame, test1)


getSID <- function(year, EcoR) {
    message("Downloading SID data for year: ", year)
    stock_list_long <- jsonlite::fromJSON(
        URLencode(
            sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, EcoRegion, YearOfLastAssessment, AssessmentKey, AdviceCategory, FisheriesGuild", year)
        )
    )$value

    stock_list_long %>%
        mutate(EcoRegion = as.character(EcoRegion)) %>%
        separate_rows(EcoRegion, sep = ", ")
    stock_list_long <- stock_list_long %>%
        filter(EcoRegion == EcoR)
    # setDT(stock_list_long)

    # # Get unique valid years (excluding NA and 0)
    # valid_years <- unique(stock_list_long$YearOfLastAssessment)
    # valid_years <- valid_years[!is.na(valid_years) & valid_years != 0]


    # # Parallelized API calls for ASD records
    # ASDList <- rbindlist(future_lapply(valid_years, function(y) {
    #     message("Fetching ASD advice records for year: ", y)
    #     as.data.table(icesASD::getAdviceViewRecord(year = y))
    # }), fill = TRUE)

    # ASDList <- ASDList %>% group_by(stockCode) %>% filter(assessmentYear == max(assessmentYear, na.rm = TRUE, finite = TRUE)) %>% ungroup()
    # ASDList <- ASDList %>% select(stockCode, assessmentKey, adviceComponent, adviceStatus)

    # # Ensure ASDList is a valid data frame
    # if (is.null(ASDList) || identical(ASDList, list()) || nrow(ASDList) == 0) {
    #     ASDList <- data.frame(
    #         StockKeyLabel = character(),
    #         AssessmentKey = character(),
    #         AssessmentComponent = character(),
    #         stringsAsFactors = FALSE
    #     )
    # } else {
    #     ASDList <- ASDList %>%
    #         mutate(adviceComponent = na_if(adviceComponent, "N.A.")) %>%
    #         rename(
    #             StockKeyLabel = stockCode,
    #             AssessmentKey = assessmentKey,
    #             AssessmentComponent = adviceComponent
    #         ) %>%
    #         filter(adviceStatus == "Advice")
    # }
    # setDT(ASDList)

    # message("Merging SID and ASD records...")
    # # Efficient merge using data.table
    # stock_list_long <- ASDList[stock_list_long, on = "StockKeyLabel"]

    # Find missing AssessmentKeys using YearOfLastAssessment
    # browser()
    missing_keys <- which(is.na(stock_list_long$AssessmentKey) &
        !is.na(stock_list_long$YearOfLastAssessment) &
        stock_list_long$YearOfLastAssessment != 0)

    if (length(missing_keys) > 0) {
        message("Finding missing assessment keys...")

        # Retrieve assessment keys (returns list)
        assessment_keys <- lapply(missing_keys, function(i) {
            keys <- icesSAG::findAssessmentKey(stock_list_long$StockKeyLabel[i],
                year = stock_list_long$YearOfLastAssessment[i]
            )
            if (length(keys) > 0) keys[1] else NA # Take only the first key or return NA
        })

        # Convert list to vector and assign
        stock_list_long$AssessmentKey[missing_keys] <- unlist(assessment_keys)
    }

    # Drop rows where AssessmentKey is still NA
    # stock_list_long <- stock_list_long[!is.na(AssessmentKey)]
    stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]


    message("Data processing complete.")
    return(stock_list_long)
}

df <- getSID(year = 2024, EcoR = "Greater North Sea")
# status <- icesSAG::getStockStatusValues(df$AssessmentKey)
# Set up parallel execution (adjust workers based on system capability)
plan(multisession, workers = parallel::detectCores() - 1)

# Ensure AssessmentKey is unique to avoid redundant API calls
unique_keys <- unique(df$AssessmentKey)

# Fetch stock status values in parallel
status_list <- future_lapply(unique_keys, function(key) {
    tryCatch(
        icesSAG::getStockStatusValues(key),
        error = function(e) {
            message(sprintf("Error fetching data for AssessmentKey: %s", key))
            return(NULL)
        }
    )
})

status <- do.call(rbind, status_list)
# Combine results into a single dataframe
status <- do.call(rbind.data.frame, status)
# Merge stock status values with SID data
df_merged <- merge(df, status, by = "AssessmentKey", all.x = TRUE)
df_merged$FisheriesGuild <- tolower(df_merged$FisheriesGuild)

# read rda file
load("data/clean_status.rda")
str(clean_status)
names(clean_status)
names(df_merged)
format_sag_status_new <- function(x) {
        df <- x
        # df <- dplyr::filter(df,(grepl(pattern = ecoregion, Ecoregion)))
        df <- dplyr::mutate(df,status = case_when(status == 0 ~ "UNDEFINED",
                                                  status == 1 ~ "GREEN",
                                                  status == 2 ~ "qual_GREEN", #qualitative green
                                                  status == 3 ~ "ORANGE",
                                                  status == 4 ~ "RED",
                                                  status == 5 ~ "qual_RED", #qualitative red
                                                  status == 6 ~ "GREY",
                                                  status == 7 ~ "qual_UP",
                                                  status == 8 ~ "qual_STEADY",
                                                  status == 9 ~ "qual_DOWN",
                                                  TRUE ~ "OTHER"),
                            fishingPressure = case_when(fishingPressure == "-" &
                                                                type == "Fishing pressure" ~ "FQual",
                                                        TRUE ~ fishingPressure),
                            stockSize = case_when(stockSize == "-" &
                                                          type == "Stock Size" ~ "SSBQual",
                                                  TRUE ~ stockSize),
                            stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
                            variable = case_when(type == "Fishing pressure" ~ fishingPressure,
                                                 type == "Stock Size" ~ stockSize,
                                                 TRUE ~ type),
                            variable = case_when(lineDescription == "Management plan" &
                                                         type == "Fishing pressure" ~ "FMGT",
                                                 lineDescription == "Management plan" &
                                                         type == "Stock Size" ~ "SSBMGT",
                                                 TRUE ~ variable),
                            variable = case_when(
                                    grepl("Fpa", variable) ~ "FPA",
                                    grepl("Bpa", variable) ~ "BPA",
                                    grepl("^Qual*", variable) ~ "SSBQual",
                                    grepl("-", variable) ~ "FQual",
                                    grepl("^BMGT", variable) ~ "SSBMGT",
                                    grepl("MSYBtrigger", variable) ~ "BMSY",
                                    grepl("FMSY", variable) ~ "FMSY",
                                    TRUE ~ variable
                            )) 
        df <- dplyr::filter(df,variable != "-")
        
        df <- dplyr::filter(df, lineDescription != "Management plan")
        df <- dplyr::filter(df, lineDescription != "Qualitative evaluation")
        df <- dplyr::mutate(df,key = paste(StockKeyLabel, lineDescription, type))
        df<- df[order(-df$year),]
        df <- df[!duplicated(df$key), ]
        df<- subset(df, select = -key)
        df<- subset(df, select = c(StockKeyLabel, AssessmentKey,lineDescription, type, status, FisheriesGuild))
        df<- tidyr::spread(df,type, status)
        
        df2<- dplyr::filter(df,lineDescription != "Maximum Sustainable Yield")
        df2<- dplyr::filter(df2,lineDescription != "Maximum sustainable yield")
        
        colnames(df2) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" )
        df2 <-dplyr::mutate(df2, SBL = case_when(FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                                                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                                                 FishingPressure == "ORANGE"  |  StockSize == "ORANGE" ~ "RED",
                                                 TRUE ~ "GREY"))
        df2<- subset(df2, select = c(StockKeyLabel, SBL))
        df <- dplyr::left_join(df, df2)
        df$lineDescription <- gsub("Maximum Sustainable Yield", "Maximum sustainable yield", df$lineDescription)
        df$lineDescription <- gsub("Precautionary Approach", "Precautionary approach", df$lineDescription)
        # colnames(df) <- c("StockKeyLabel","AssessmentYear","AdviceCategory","lineDescription","FishingPressure","StockSize", "SBL" )
        # sid <- dplyr::select(y,StockKeyLabel,
        #                      FisheriesGuild)
        # sid$FisheriesGuild <- tolower(sid$FisheriesGuild)
        # colnames(sid) <- c("StockKeyLabel", "AssessmentYear", "Ecoregion", "FisheriesGuild")
        # df <- merge(df, sid, all = FALSE)
        return(df)
}

# library(dplyr)
# library(tidyr)

# format_sag_status_new <- function(x) {
#     # Ensure column names are valid
#     names(x) <- make.names(names(x), unique = TRUE)
    
#     df <- x %>%
#         mutate(
#             status = case_when(
#                 status == 0 ~ "UNDEFINED",
#                 status == 1 ~ "GREEN",
#                 status == 2 ~ "qual_GREEN",  # qualitative green
#                 status == 3 ~ "ORANGE",
#                 status == 4 ~ "RED",
#                 status == 5 ~ "qual_RED",    # qualitative red
#                 status == 6 ~ "GREY",
#                 status == 7 ~ "qual_UP",
#                 status == 8 ~ "qual_STEADY",
#                 status == 9 ~ "qual_DOWN",
#                 TRUE ~ "OTHER"
#             ),
#             fishingPressure = ifelse(fishingPressure == "-" & type == "Fishing pressure", "FQual", fishingPressure),
#             stockSize = ifelse(stockSize == "-" & type == "Stock Size", "SSBQual", stockSize),
#             stockSize = gsub("MSY BT*|MSY Bt*|MSYBT|MSYBt", "MSYBt", stockSize),
#             variable = case_when(
#                 type == "Fishing pressure" ~ fishingPressure,
#                 type == "Stock Size" ~ stockSize,
#                 TRUE ~ type
#             ),
#             variable = case_when(
#                 lineDescription == "Management plan" & type == "Fishing pressure" ~ "FMGT",
#                 lineDescription == "Management plan" & type == "Stock Size" ~ "SSBMGT",
#                 TRUE ~ variable
#             ),
#             variable = case_when(
#                 grepl("Fpa", variable) ~ "FPA",
#                 grepl("Bpa", variable) ~ "BPA",
#                 grepl("^Qual*", variable) ~ "SSBQual",
#                 grepl("-", variable) ~ "FQual",
#                 grepl("^BMGT", variable) ~ "SSBMGT",
#                 grepl("MSYBtrigger", variable) ~ "BMSY",
#                 grepl("FMSY", variable) ~ "FMSY",
#                 TRUE ~ variable
#             )
#         ) %>%
#         filter(variable != "-", 
#                !lineDescription %in% c("Management plan", "Qualitative evaluation")) %>%
#         mutate(
#             key = paste(StockKeyLabel, lineDescription, type)
#         ) %>%
#         arrange(desc(year)) %>%
#         distinct(key, .keep_all = TRUE) %>%
#         select(-key) %>%
#         pivot_wider(names_from = type, values_from = status)

#     # Create and merge SBL status
#     df2 <- df %>%
#         filter(!lineDescription %in% c("Maximum Sustainable Yield", "Maximum sustainable yield")) %>%
#         rename(FishingPressure = `Fishing pressure`, StockSize = `Stock Size`) %>%
#         mutate(
#             SBL = case_when(
#                 FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
#                 FishingPressure == "RED" | StockSize == "RED" ~ "RED",
#                 FishingPressure == "ORANGE" | StockSize == "ORANGE" ~ "RED",
#                 TRUE ~ "GREY"
#             )
#         ) %>%
#         select(StockKeyLabel, SBL)

#     df <- left_join(df, df2, by = "StockKeyLabel") %>%
#         mutate(
#             lineDescription = gsub("Maximum Sustainable Yield", "Maximum sustainable yield", lineDescription),
#             lineDescription = gsub("Precautionary Approach", "Precautionary approach", lineDescription)
#         )

#     return(df)
# }

cleanStatus <- format_sag_status_new(df_merged)
str(cleanStatus)
str(clean_status)
names(cleanStatus)
colnames(cleanStatus) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" , "SBL")

plot_status_prop_pies <- function(x, cap_month = "November",
                         cap_year = "2018",
                         return_data = FALSE) {
        df <- x
        colnames(df) <- c("StockKeyLabel","AssessmentKey","lineDescription","FisheriesGuild","FishingPressure","StockSize" , "SBL")
        cap_lab <- ggplot2::labs(title = "", x = "", y = "",
                        caption = sprintf("ICES Stock Assessment Database, %s %s. ICES, Copenhagen",
                                          cap_month,
                                          cap_year))
        colList <- c("GREEN" = "#00B26D",
                     "GREY" = "#d3d3d3",
                     "ORANGE" = "#ff7f00",
                     "RED" = "#d93b1c",
                     "qual_RED" = "#d93b1c",
                     "qual_GREEN" = "#00B26D")


        df_stock <- dplyr::select(df,StockKeyLabel,
                       FisheriesGuild,
                       lineDescription,
                       FishingPressure,
                       StockSize,
                       SBL)
        df_stock <- tidyr::gather(df_stock,Variable, Colour, FishingPressure:StockSize, factor_key = TRUE)
        df2 <- dplyr::group_by(df_stock, FisheriesGuild, lineDescription, Variable, Colour)
        df2 <- dplyr::summarize(df2, COUNT = dplyr::n())
        df2 <- tidyr::spread(df2, Colour, COUNT)
        df2[is.na(df2)] <- 0
        df3 <- subset(df2,select =-c(FisheriesGuild))
        df3 <- dplyr::group_by(df3,lineDescription, Variable)
        df3 <- dplyr::summarise_each(df3,dplyr::funs(sum))
        df3$FisheriesGuild <- "total"
        df2 <- rbind(df2,df3)

        df4 <- dplyr::filter(df2,Variable == "SBL")
        df4$lineDescription <- ""
        df4 <- unique(df4)
        df2 <- dplyr::filter(df2,Variable != "SBL")
        df2 <- rbind(df2,df4)
        df2$lineDescription <- gsub("Maximum sustainable yield","MSY", df2$lineDescription)
        df2$lineDescription <- gsub("Precautionary approach", "PA", df2$lineDescription)
        df2$header <- paste0(df2$Variable, "\n" , df2$lineDescription)

        df2 <- tidyr::gather(df2,colour, value,GREEN:RED, factor_key = TRUE)
        df2 <- dplyr::filter(df2,value > 0)


        tot <- dplyr::filter(df2,FisheriesGuild == "total")
        tot <- dplyr::group_by(tot,header)
        tot <- dplyr::mutate(tot, tot = sum(value))
        max <- unique(tot$tot)
        df2 <- dplyr::group_by(df2, FisheriesGuild, header)
        df2 <- dplyr::mutate(df2,sum = sum(value))
        df2$fraction <- df2$value*max/df2$sum
        df2$header <- factor(df2$header, levels = c("FishingPressure\nMSY", "StockSize\nMSY",
                                                    "FishingPressure\nPA" ,"StockSize\nPA",
                                                    "SBL\n" ))
        df2$FisheriesGuild <- tolower(df2$FisheriesGuild)
        df2$FisheriesGuild <- factor(df2$FisheriesGuild, levels= c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        
        
        p1 <- ggplot2::ggplot(data = df2, ggplot2::aes(x = "", y = fraction, fill = colour)) +
                ggplot2::geom_bar(stat = "identity", width = 1) +
                ggplot2::geom_text(ggplot2::aes(label = value),
                          position = ggplot2::position_stack(vjust = 0.5),
                          size = 3) +
                ggplot2::scale_fill_manual(values = colList) +
                ggplot2::theme_bw(base_size = 9) +
                ggplot2::theme(panel.grid = ggplot2::element_blank(),
                      panel.border = ggplot2::element_blank(),
                      panel.background = ggplot2::element_blank(),
                      legend.position="none") +
                ggplot2::theme(axis.text=ggplot2::element_blank(),
                      axis.ticks=ggplot2::element_blank(),
                      strip.background = ggplot2::element_blank(),
                      plot.caption = ggplot2::element_text(size = 6)) +
                # cap_lab +
                ggplot2::coord_polar(theta = "y", direction = 1) +
                ggplot2::facet_grid(FisheriesGuild ~ header)
                
        # ggplotly(p1)     
        # browser()
        # Create a list of separate plots for each header when interactive mode is enabled
        
        # browser()
        # plot_list <- df2 %>%
        # split(list(.$FisheriesGuild, .$header)) %>%
        # lapply(function(data) {
        #     if (nrow(data) > 0) {
        #         plot_ly(data, labels = ~colour, values = ~fraction, type = 'pie',
        #                 textinfo = 'label+percent', marker = list(colors = colList)) %>%
        #             layout(title = paste(data$FisheriesGuild[1], "-", data$header[1]))
        #     } else {
        #         NULL
        #     }
        # }) %>%
        # purrr::compact() 

        # # Ensure no NA values are passed to layout
        # plot_list <- lapply(plot_list, function(p) {
        #     if (!is.null(p)) {
        #         p$x$layout <- Filter(Negate(is.na), p$x$layout)
        #     }
        #     p
        # })

        # # Determine the number of rows and columns for the subplot
        # n_plots <- length(plot_list)
        # n_cols <- 5
        # n_rows <- 6#ceiling(n_plots / n_cols)
        
        # return(subplot(plot_list, nrows = n_rows, ncols = n_cols, shareX = TRUE, shareY = TRUE))
        if(return_data == T){
                df2
        }else{
                p1
        }
}

plot_status_prop_pies(cleanStatus, cap_month = "November", cap_year = "2018", return_data = FALSE) 


plot_status_prop_pies <- function(x, cap_month = "November", cap_year = "2018", return_data = FALSE) {
    df <- x %>%
        setNames(c("StockKeyLabel", "AssessmentKey", "lineDescription", "FisheriesGuild", "FishingPressure", "StockSize", "SBL"))
    
    colList <- c("GREEN" = "#00B26D", "GREY" = "#d3d3d3", "ORANGE" = "#ff7f00", "RED" = "#d93b1c",
                 "qual_RED" = "#d93b1c", "qual_GREEN" = "#00B26D")
    
    df_stock <- df %>%
        select(StockKeyLabel, FisheriesGuild, lineDescription, FishingPressure, StockSize, SBL) %>%
        pivot_longer(cols = FishingPressure:StockSize, names_to = "Variable", values_to = "Colour")
    
    df2 <- df_stock %>%
        count(FisheriesGuild, lineDescription, Variable, Colour) %>%
        pivot_wider(names_from = Colour, values_from = n, values_fill = list(n = 0))
    
    df3 <- df2 %>%
        select(-FisheriesGuild) %>%
        group_by(lineDescription, Variable) %>%
        summarise(across(everything(), sum), .groups = "drop") %>%
        mutate(FisheriesGuild = "total")
    
    df2 <- bind_rows(df2, df3)
    
    df4 <- df2 %>%
        filter(Variable == "SBL") %>%
        mutate(lineDescription = "") %>%
        distinct()
    
    df2 <- df2 %>% filter(Variable != "SBL") %>% bind_rows(df4)
    
    df2 <- df2 %>%
        mutate(lineDescription = recode(lineDescription, "Maximum sustainable yield" = "MSY", "Precautionary approach" = "PA"),
               header = paste(Variable, "\n", lineDescription)) %>%
        pivot_longer(cols = GREEN:RED, names_to = "colour", values_to = "value") %>%
        filter(value > 0)
    
    tot <- df2 %>%
        filter(FisheriesGuild == "total") %>%
        group_by(header) %>%
        summarise(tot = sum(value), .groups = "drop")
    
    df2 <- df2 %>%
        left_join(tot, by = "header") %>%
        group_by(FisheriesGuild, header) %>%
        mutate(sum = sum(value), fraction = ifelse(sum > 0, value * tot / sum, 0)) %>%
        ungroup() %>%
        mutate(
            header = factor(header, levels = c("FishingPressure\nMSY", "StockSize\nMSY", "FishingPressure\nPA", "StockSize\nPA", "SBL\n")),
            FisheriesGuild = factor(tolower(FisheriesGuild), levels = c("total", "benthic", "demersal", "pelagic", "crustacean", "elasmobranch"))
        )
    
    if (return_data) return(df2)
    
    # Generate interactive pie charts for each combination of FisheriesGuild and header
    plot_list <- df2 %>%
        split(list(.$FisheriesGuild, .$header)) %>%
        lapply(function(data) {
            if (nrow(data) > 0) {
                plot_ly(data, labels = ~colour, values = ~fraction, type = 'pie',
                        textinfo = 'label+percent', marker = list(colors = colList)) %>%
                    layout(title = paste(data$FisheriesGuild[1], "-", data$header[1]))
            } else {
                NULL
            }
        }) %>%
        purrr::compact() # Remove NULL elements
    
    if (length(plot_list) == 0) {
        stop("No valid data to plot.")
    }
    
    return(subplot(plot_list, nrows = 6, shareX = TRUE, shareY = TRUE))
}


clean_status <- format_sag_status_new(getStatus(year = 2024, EcoR = "Greater North Sea"))

format_annex_table <- function(status, year) {
  # sid <- getSD(year)
#   sid <- dplyr::filter(sid, !is.na(YearOfLastAssessment))
#   sid <- dplyr::select(sid,
#                        StockKeyLabel,
#                        StockKeyDescription,
#                        SpeciesScientificName,
#                        SpeciesCommonName,
#                        # FisheriesGuild,
#                        DataCategory)
#   sid <- sid %>% filter(StockKeyLabel %in% df$StockKeyLabel)
#   df <- dplyr::left_join(df, sid, by = "StockKeyLabel")
  # df <- df[c(1,10,11,12,13,14,2,3,4,5,8,6,7)]
  year <- 2024
  status <- clean_status
  sid <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel, 
                        EcoRegion, 
                        YearOfLastAssessment, 
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName, 
                        AdviceCategory, 
                        FisheriesGuild", year)
                )
        )$value
    sid <- sid %>% filter(StockKeyLabel %in% status$StockKeyLabel)
  df <- dplyr::left_join(status, sid, by = "StockKeyLabel")
    # status <- test
  df <- dplyr::mutate(df,
                      D3C1 = FishingPressure,
                      D3C2 = StockSize,
                      GES = dplyr::case_when(
                          FishingPressure == "GREEN" & StockSize == "GREEN" ~ "GREEN",
                          FishingPressure == "RED" | StockSize == "RED" ~ "RED",
                          FishingPressure == "GREY" | StockSize == "GREY" ~ "GREY",
                          TRUE ~ "GREY"))


  df$StockKeyDescription <- gsub("\\s*\\([^\\)]+\\)","",df$StockKeyDescription, perl = TRUE)

  df
}


getSAG_ecoregion <- function(year, ecoregion, sid){
        years <- ((year-4):year)
        ecoreg <- gsub(" ", "%20", ecoregion, fixed = TRUE)
        # sid <- icesSD::getSD(NULL,year)
        out <- data.frame()
        res <- data.frame()
        for(n in 1:5){
                x <- years[n]
                url <- paste0("https://sag.ices.dk/SAG_API/api/SAGDownload?year=", x, "&EcoRegion=", ecoreg)
                tmpSAG <- tempfile(fileext = ".zip")
                download.file(url, destfile = tmpSAG, mode = "wb", quiet = FALSE)
                names <-unzip(tmpSAG, list = TRUE)
                res <- read.csv(unz(tmpSAG, names$Name[1]),
                                stringsAsFactors = FALSE,
                                header = TRUE,
                                fill = TRUE)
                res<- unique(res)
                out <- rbind(out, res)
        }
        out <- dplyr::filter(out, Purpose == "Advice")
        out <- data.table::as.data.table(out) 
        # out <- out[out[, .I[AssessmentKey == max(AssessmentKey)], by=FishStock]$V1]
        out <- out[out[, .I[AssessmentYear == max(AssessmentYear)], by=FishStock]$V1]
        out <- as.data.frame(out)
        out <- dplyr::filter(out,out$FishStock %in% sid$StockKeyLabel)
}

format_sag <- function(sag,sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sag <- dplyr::mutate(sag, StockKeyLabel=FishStock)
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        # df1 <- left_join(x, y)
        # df1 <- left_join(x, y, by = c("StockKeyLabel", "AssessmentYear"))
        df1 <-as.data.frame(df1)
        
        df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        df1 <- subset(df1, select = -c(FishStock))
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        # check <-unique(df1[c("StockKeyLabel", "FisheriesGuild")])
        out <- dplyr::anti_join(df1, check)
}
sag_complete_frmt <- format_sag(sag, sid)

stockstatus_CLD_current <- function(x) {
        df<- dplyr::select(x,Year,
                           StockKeyLabel,
                           FisheriesGuild,
                           FishingPressure,
                           AssessmentYear,
                           FMSY,
                           StockSize,
                           MSYBtrigger,
                           Catches,
                           Landings,
                           Discards)
        df$FishingPressure <- as.numeric(df$FishingPressure)
        df$StockSize <- as.numeric(df$StockSize)
        df$FMSY <- as.numeric(df$FMSY)
        df$MSYBtrigger <- as.numeric(df$MSYBtrigger)
        df2 <- dplyr::group_by(df,StockKeyLabel)
        df2 <- dplyr::filter(df2,Year == AssessmentYear - 1)
        df2 <- dplyr::mutate(df2,F_FMSY =  ifelse(!is.na(FMSY),
                                                                FishingPressure / FMSY,
                                                                NA))
        df2 <- dplyr::select(df2,StockKeyLabel,
                                               FisheriesGuild,
                                               F_FMSY,
                                               Catches,
                                               Landings,
                                               Discards,
                                               FMSY,
                                               FishingPressure)
        df3 <- dplyr::group_by(df,StockKeyLabel)
        df3 <- dplyr::filter(df3, Year %in% c(AssessmentYear, (AssessmentYear - 1)))
        df3 <- dplyr::mutate(df3, SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger),
                                                                        StockSize / MSYBtrigger,
                                                                        NA))
        df3 <- dplyr::select(df3, StockKeyLabel,Year,
                                               FisheriesGuild,
                                               SSB_MSYBtrigger,
                                               StockSize,
                                               MSYBtrigger)
        check <- unique(df3[c("StockKeyLabel", "Year", "MSYBtrigger")])
        check <- check[order(-check$Year),]
        check2 <- check[duplicated(check$StockKeyLabel),]
        df3 <- dplyr::anti_join(df3,check2)
        df4 <- dplyr::full_join(df2, df3)
        df4 <- dplyr::mutate(df4, Status = ifelse(is.na(F_FMSY) | is.na(SSB_MSYBtrigger),
                                      "GREY",
                                      if_else(F_FMSY < 1 & SSB_MSYBtrigger >= 1,
                                              "GREEN",
                                              "RED",
                                              "GREY")))
        df4
}

sag_catch_current <- stockstatus_CLD_current(sag_complete_frmt)


# function to download data from github
download_github_data <- function(repo_owner, repo_name, file_path) {
    # Fetch file metadata from GitHub API
    response <- gh::gh("GET /repos/{owner}/{repo}/contents/{path}", 
                   owner = repo_owner, 
                   repo = repo_name, 
                   path = file_path)
    
    # Extract raw file URL
    download_url <- response$download_url
    
    # Download and read the file
    df <- read.csv(download_url)
    
    return(df)
}

NrS_catchScenarioStk <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice", "shiny/Figure1_HeadlinePlot_data.csv")
NrS_catchRange <- download_github_data("ices-taf", "2024_NrS_MixedFisheriesAdvice","shiny/Figure1_HeadlinePlot_advice.csv")
save(NrS_catchScenarioStk, file = "D:/GitHub_2023/fisheriesXplorer/data/NrS_catchScenarioStk.rda")
save(NrS_catchRange, file = "D:/GitHub_2023/fisheriesXplorer/data/NrS_catchRange.rda")


devtools::load_all(); run_app()




### recover cat 3

     unique(c(refptsAll %>% distinct(CustomRefPointName1) %>% pull,
           refptsAll %>% distinct(CustomRefPointName2)%>% pull,
           refptsAll %>% distinct(CustomRefPointName3)%>% pull,
           refptsAll %>% distinct(CustomRefPointName4)%>% pull,
           refptsAll %>% distinct(CustomRefPointName5)%>% pull))

     ### when MSYBtrigger is na and Itrigger in custom then set MSYBtrigger to Itrigger
     itrgigger <- c("I (trigger)", "I_{trigger}", "Itrigger",
"I_(trigger)",
                 "I trigger", "MP B_{trigger}", "MGT B_{trigger}",
"MGTB_{trigger}",
                 "B_{MGT}", "MSY Btrigger", "I_{Btrigger}")

     refptsAll <- refptsAll %>% mutate(CustomRefPointValue1 =
as.numeric(CustomRefPointValue1),
                                     CustomRefPointValue2 =
as.numeric(CustomRefPointValue2),
                                     CustomRefPointValue3 =
as.numeric(CustomRefPointValue3),
                                     CustomRefPointValue4 =
as.numeric(CustomRefPointValue4),
                                     CustomRefPointValue5 =
as.numeric(CustomRefPointValue5))
     refptsAll2 <- refptsAll

     refptsAll2 <- refptsAll2 %>%
         mutate(MSYBtrigger = case_when(
             (MSYBtrigger == "" | is.na(MSYBtrigger)) &
CustomRefPointName1 %in% itrgigger ~ CustomRefPointValue1,
             (MSYBtrigger == "" | is.na(MSYBtrigger)) &
CustomRefPointName2 %in% itrgigger ~ CustomRefPointValue2,
             (MSYBtrigger == "" | is.na(MSYBtrigger)) &
CustomRefPointName3 %in% itrgigger ~ CustomRefPointValue3,
             (MSYBtrigger == "" | is.na(MSYBtrigger)) &
CustomRefPointName4 %in% itrgigger ~ CustomRefPointValue4,
             (MSYBtrigger == "" | is.na(MSYBtrigger)) &
CustomRefPointName5 %in% itrgigger ~ CustomRefPointValue5,
             TRUE ~ MSYBtrigger))

     ### when FMSY is na and Fmsy proxy in custom then set FMSY to Fmsy proxy
     fmsyproxy <- c("HR_{MSY proxy}", "F_{MSYproxy}", "FMSY proxy",
"F_{MSY proxy}",
         "Relative FMSY", "HR_{MSY proxy}", "Fmsy proxy", "F_(msy
proxy)",
         "F_(MSY proxy)", "F_{MSY proxy}", "FMSY proxy",
         "F_{MSY proxy}", "Fmsy proxy", "F_{MSYproxy}",
         "F_{MGT}", "Rel FMSY", "F_{MP}", "F/F_{MSY}" , "HR_{MGT}",
          "F MSY proxy" ,"F MSY proxy", "LBI"
          #"HRmsy proxy", "HRMSY proxy", "HR_{MSY}", "HRMSY proxy",
"F_(MSY proxy)", "HR MSY proxy", "HRmsy",

          )

     refptsAll2 <- refptsAll2 %>%
         mutate(FMSY = case_when(
             (FMSY == "" | is.na(FMSY)) & CustomRefPointName1 %in%
fmsyproxy ~ CustomRefPointValue1,
             (FMSY == "" | is.na(FMSY)) & CustomRefPointName2 %in%
fmsyproxy ~ CustomRefPointValue2,
             (FMSY == "" | is.na(FMSY)) & CustomRefPointName3 %in%
fmsyproxy ~ CustomRefPointValue3,
             (FMSY == "" | is.na(FMSY)) & CustomRefPointName3 %in%
fmsyproxy ~ CustomRefPointValue4,
             (FMSY == "" | is.na(FMSY)) & CustomRefPointName3 %in%
fmsyproxy ~ CustomRefPointValue5,
             TRUE ~ FMSY))


# Fix
unique(refptsAll2$FMGT_lower)
unique(refptsAll2$FMGTRange_low)

refptsAll2 <- refptsAll2 %>%
     mutate(FMGT_lower = case_when(
         (FMGT_lower == "" | is.na(FMGT_lower)) &   (FMGTRange_low != ""
| !is.na(FMGTRange_low)) ~ FMGTRange_low,
         TRUE ~ FMGT_lower))

unique(refptsAll2$FMGT_upper)
unique(refptsAll2$FMGTRange_high)

refptsAll2 <- refptsAll2 %>%
     mutate(FMGT_lower = case_when(
         (FMGT_upper == "" | is.na(FMGT_upper)) &   (FMGTRange_high != ""
| !is.na(FMGTRange_high)) ~ FMGTRange_high,
         TRUE ~ FMGT_upper))




#### code to scale plot text size for ggplotly
output$myPlot <- renderPlotly({
  w <- session$clientData$output_myPlot_width
  size <- max(8, min(16, round(w / 50)))
  
  p <- ggplot(df, aes(x, y)) +
    geom_point() +
    theme(
      axis.title = element_text(size = size),
      axis.text  = element_text(size = size - 2),
      legend.text = element_text(size = size - 2),
      legend.title = element_text(size = size)
    )
  
  ggplotly(p)
})



/* -------- Plotly responsive typography -------- */

/* Base (tablet-ish defaults) */
:root{
  --plt-tick: 12px;
  --plt-legend: 12px;
  --plt-annotation: 12px;
  --plt-axistitle: 14px;
  --plt-title: 16px;
}

/* Phones & small screens */
@media (max-width: 600px){
  :root{
    --plt-tick: 9px;
    --plt-legend: 9px;
    --plt-annotation: 9px;
    --plt-axistitle: 11px;
    --plt-title: 13px;
  }
}

/* Laptops / typical desktops */
@media (min-width: 601px) and (max-width: 1440px){
  :root{
    --plt-tick: 12px;
    --plt-legend: 12px;
    --plt-annotation: 13px;
    --plt-axistitle: 14px;
    --plt-title: 16px;
  }
}

/* Large / wide screens */
@media (min-width: 1441px){
  :root{
    --plt-tick: 14px;
    --plt-legend: 12px;
    --plt-annotation: 14px;
    --plt-axistitle: 16px;
    --plt-title: 18px;
  }
}

/* Apply variables to Plotly SVG text */
.js-plotly-plot .xtick text,
.js-plotly-plot .ytick text{
  font-size: var(--plt-tick) !important;
}

.js-plotly-plot .legend .legendtext,
.js-plotly-plot .legend text{
  font-size: var(--plt-legend) !important;
}

/* Axis titles */
.js-plotly-plot g.xtitle text,
.js-plotly-plot g.ytitle text{
  font-size: var(--plt-axistitle) !important;
}

/* Plot title */
.js-plotly-plot .gtitle{
  font-size: var(--plt-title) !important;
}

/* Annotations */
.js-plotly-plot .annotation-text,
.js-plotly-plot .annotation text{
  font-size: var(--plt-annotation) !important;
}


# ---- Packages
# install.packages(c("rsconnect", "dplyr", "ggplot2"))
library(rsconnect)
library(dplyr)
library(ggplot2)

# ---- CONFIG: fill these in
account_name <- "ices-taf"
apps <- c("fisheriesXplorer", "	advicexplorer", "seabass-catch-allocation-tool", "neafc-catch-explorer","SEAwiseToolbox")

# If you haven't authenticated on this machine yet, do it once:
# rsconnect::setAccountInfo(name = account_name, token = "TOKEN", secret = "SECRET")

# ---- Helpers
as_date_utc <- function(x) as.Date(as.POSIXct(x, origin = "1970-01-01", tz = "UTC"))

get_hours_90d <- function(app) {
  df <- rsconnect::showUsage(
    appName    = app,
    account    = account_name,
    server     = "shinyapps.io",
    usageType  = "hours",
    from       = "90d",
    interval   = "1d"
  )
  # Expected columns include: timestamp, hours
  df %>%
    mutate(app = app, date = as_date_utc(timestamp)) %>%
    select(app, date, hours)
}

get_connections_90d <- function(app) {
  df <- rsconnect::showMetrics(
    metricSeries = "container_status",
    metricNames  = "connect_count",
    appName      = app,
    account      = account_name,
    server       = "shinyapps.io",
    from         = "90d",
    interval     = "1d"
  )
  # Expected columns include: timestamp, connect_count
  df %>%
    mutate(app = app, date = as_date_utc(time)) %>%
    select(app, date, connect_count)
}

# ---- Pull data (3 apps)
hours_df <- bind_rows(lapply(apps, function(a) {
  tryCatch(get_hours_90d(a), error = function(e) {
    message("Hours failed for ", a, ": ", e$message)
    NULL
  })
}))

conn_df <- bind_rows(lapply(apps, function(a) {
  tryCatch(get_connections_90d(a), error = function(e) {
    message("Connections failed for ", a, ": ", e$message)
    NULL
  })
}))

# ---- Plot: Active hours (daily)
p_hours <- ggplot(hours_df, aes(x = date, y = hours)) +
  geom_line() +
  facet_wrap(~ app, ncol = 1, scales = "fixed") +
  labs(x = NULL, y = "Active hours (per day)", title = "shinyapps.io active hours (last 90 days)")

print(p_hours)
ggsave("Active hours (daily).png", p_hours, width = 10, height = 6, dpi = 300, bg = "white")
# ---- Plot: Connections (daily average connect_count)
p_conn <- ggplot(conn_df, aes(x = date, y = connect_count)) +
  geom_line() +
  facet_wrap(~ app, ncol = 1, scales = "fixed") +
  labs(x = NULL, y = "connect_count (daily mean)", title = "shinyapps.io connections metric (last 90 days)")

print(p_conn)
ggsave("Connections (daily average connect_count).png", p_conn, width = 10, height = 6, dpi = 300, bg = "white")


# ---- 90-day totals per app
totals <- full_join(hours_df, conn_df, by = c("app", "date")) %>%
  group_by(app) %>%
  summarise(
    total_active_hours_90d = sum(hours, na.rm = TRUE),
    sum_daily_mean_connect_count_90d = sum(connect_count, na.rm = TRUE),
    avg_daily_mean_connect_count_90d = mean(connect_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(total_active_hours_90d))

print(totals)

totals


#' Show recent shinyapps.io logs for an app
#'
#' Requires:
#'   install.packages("rsconnect")
#'   rsconnect::setAccountInfo(name=..., token=..., secret=...)
#'
#' @param appName  App name on shinyapps.io (e.g., "fisheriesXplorer")
#' @param account  Your shinyapps.io account name (optional; inferred if only one)
#' @param server   Usually "shinyapps.io"
#' @param tail     Number of log lines to show (best-effort; depends on rsconnect version)
#' @param streaming If TRUE, keeps streaming until you interrupt (Esc/Ctrl+C)
#' @return Invisible NULL
show_shinyapps_logs <- function(appName,
                               account = NULL,
                               server = "shinyapps.io",
                               tail = 200,
                               streaming = TRUE) {
  if (!requireNamespace("rsconnect", quietly = TRUE)) {
    stop("Package 'rsconnect' is required. Install it with install.packages('rsconnect').")
  }

  # rsconnect::showLogs signature varies a bit by version; try common patterns.
  if (!isTRUE(streaming)) {
    # Non-streaming attempt (some versions support 'tail' and 'streaming')
    out <- try(
      rsconnect::showLogs(
        name = appName,
        account = account,
        server = server,
        streaming = FALSE,
        tail = tail
      ),
      silent = TRUE
    )

    if (inherits(out, "try-error")) {
      message("Your rsconnect version may not support tail/streaming args; falling back to streaming logs.")
      rsconnect::showLogs(name = appName, account = account, server = server)
    }
    return(invisible(NULL))
  }

  # Streaming logs (works on most rsconnect versions)
  rsconnect::showLogs(name = appName, account = account, server = server)
  invisible(NULL)
}

show_shinyapps_logs(appName = "fisheriesXplorer", account = "ices-tools-dev", server = "shinyapps.io", tail = 200, streaming = FALSE)


rsconnect::showLogs(appName = "fisheriesXplorer", account = "ices-tools-dev", server = "shinyapps.io",entries = 200)
rsconnect::accountInfo("ices-tools-dev")


renv::deactivate()
renv::activate()
renv::restore()



load("data/catchScenarioStk.rda")
dim(catchScenarioStk)
names(catchScenarioStk)
head(catchScenarioStk)







#' Alluvial plot
#'
#' @param data data.frame Contains information on for making an alluvial-type
#'   plot (e.g. catch or landings). The data.frame should contain several
#'   variables for grouping (e.g. `fleet`, `metier`, `stock`) as well as a
#'   variable to plot values (`value`).
#' @param group_vars vector. Contains variable names (order from left to right)
#'   that define the alluvial axes
#'   (Default: `group_vars = c("fleet", "metier", "stock")`)
#' @param fill_var character. Data variable name used for coloring alluvial
#'   (Default: `fill_var = "stock"`).
#' @param refTable data.frame Contains stock look-up information for consistent
#'   plotting of colors. The data.frame should contain a variable that matches
#'   the `fill_var` definition (Default: `fill_var = "stock"`), and also found
#'   in the `data` object. Contains a variable `col` that defines the color
#'   used to fill bars in plot. `order` defines the order of levels used in
#'   plot.
#' @param group_labs vector Contains the x-axis labels used for the group_vars.
#'   (Default: group_labs = group_vars).
#' @param text_repel logical Should stratum labels be repelled to prevent
#'   overlapping (using `ggrepel::geom_text_repel`)
#'   (Default: `text_repel = FALSE`)
#' @param text_size numeric Value for text label size
#'   (Default: `text_size = 2`)
#' @param mult_x vector. Two values defining the multiplier to expand the
#'   x-axis (Default: `mult_x = c(0.1, 0.1)`). Passes values to
#'   `expansion(mult = mult_x)`.
#' @param nudge_x numeric. Value to nudge direction of stratum labels when
#'   `text_repel = TRUE`. (Default: `nudge_x = 1/3`)
#' @param stratum_width numeric. Width of the stratum bar
#'   (Default: `stratum_width = 1/3`)
#' @param stratum_col color definition. Color used to fll the non-stock strata
#'   (Default: `stratum_col = "white"`)
#' @param xlab character X-axis label (Default: `xlab = NULL` removes label)
#' @param ylab character Y-axis label (Default: `ylab = Catch [t]`)
#' @param fillLegendTitle character Fill legend title
#'   (Default: `fillLegendTitle = "Stock"`)
#' @param plotTitle character Plot title
#'   (Default: `plotTitle = NULL` removes title)
#' @param addLegend logical Should legend of stock fill colors be added
#'
#' @return plot output of class ggplot
#' @import ggplot2 ggalluvial ggrepel
#' @export
#'
#' @examples
#'
#' data("stfMtStkSum")
#' data("refTable")
#'
#' ## create catch data by fleet, metier, and stock
#' dataYr <- 2020
#' # add country for filter (if desired)
#' stfMtStkSum$country <- unlist(lapply(strsplit(stfMtStkSum$fleet, "_"),
#'   function(x){x[1]}))
#' # filter data
#' df <- subset(stfMtStkSum, year == 2020 & scenario == "min" &
#'   country == "BE")[,c("fleet", "metier", "stock", "landings")]
#' df$stock <- refTable$stock[match(df$stock, refTable$stock_short)]
#' names(df)[4] <- "value"
#'
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "metier", "stock"),
#'   fill_var = c("stock"))
#'
#' # doesn't make sense, but redefine group_var order
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "stock", "metier"),
#'   fill_var = c("stock"))
#'
#' # a 2-level version
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "stock"),
#'   fill_var = c("stock"))
#'
#' # repel labels
#' plot_alluvial(data = df, refTable = refTable,
#'   group_vars = c("fleet", "metier", "stock"),
#'   text_repel = TRUE, text_size = 2)
#'
#' # repel labels and suppress legend
#' plot_alluvial(data = df, refTable = refTable,
#'   text_repel = TRUE, text_size = 2, addLegend = FALSE)
#'
plot_alluvial <- function(data, refTable,
  group_vars = c("fleet", "metier", "stock"),
  fill_var = c("stock"),
  group_labs = NULL,
  text_repel = FALSE,
  text_size = 2,
  mult_x = c(0.1, 0.1),
  nudge_x = 1/3,
  stratum_width = 1/3,
  stratum_col = "white",
  xlab = NULL,
  ylab = "Catch [t]",
  fillLegendTitle = "Stock",
  addLegend = TRUE,
  plotTitle = NULL){

  if(!fill_var %in% names(refTable)){stop("fill_var must be variable in refTable")}
  if(is.null(group_labs)){group_labs <- group_vars}

  palFill <- data.frame(tmp = unique(data[fill_var]))
  names(palFill) <- fill_var
  palFill <- merge(x = palFill, y = refTable, all.x = TRUE)
  palFill <- palFill[order(palFill$order),]
  palColors <- palFill$col
  names(palColors) <- palFill[,fill_var]
  palColorScale <- scale_colour_manual(
    name = fillLegendTitle, values = palColors, aesthetics = c("fill"),
    na.value = stratum_col, # define other stratum box fill color (NA)
    limits = \(x) x[!is.na(x)]) # don't show NA fill level in legend

  # ensure plotting order
  data[,fill_var] <- factor(data[,fill_var], levels = palFill[,fill_var])

  # Build a named character vector for aesthetics
  axis_mapping <- setNames(group_vars, paste0("axis", seq_along(group_vars)))
  aes_args <- c(axis_mapping, list(y = "value"))


  p <- ggplot(data = data) +
    do.call(aes_string, aes_args) + # aes()
    ggalluvial::geom_alluvium(aes(fill = get(fill_var)), width = stratum_width) +
    ggalluvial::geom_stratum(aes(fill = get(fill_var)), width = stratum_width) +
    scale_x_discrete(limits = group_labs,
      expand = expansion(mult = mult_x)) +
    labs(y = ylab, x = xlab, title = plotTitle) +
    theme_bw() +
    theme(text = element_text(size = 9),
      axis.text.x = element_text(angle = 90, hjust = 0, vjust = 0.3,
        size = 9)) +
    palColorScale

  if(!text_repel){
    p <- p + geom_text(stat = "stratum", aes(label = after_stat(stratum)),
      size = text_size)
  }

  if(text_repel){
    p <- p + ggrepel::geom_text_repel(
      stat = "stratum", aes(label = after_stat(stratum)),
      size = text_size,
      direction = "y",
      nudge_x = nudge_x
    )
  }

  if(!addLegend){
    p <- p + guides(fill="none")
  }

  return(p)
}



library(mixfishtools)
data("stfMtStkSum")
data("refTable")
'
## create catch data by fleet, metier, and stock
dataYr <- 2024
# add country for filter (if desired)
stfMtStkSum$country <- unlist(lapply(strsplit(stfMtStkSum$fleet, "_"),
  function(x){x[1]}))
# filter data
df <- subset(stfMtStkSum, year == dataYr & scenario == "min" &
  country == "NL")[,c("fleet", "metier", "stock", "landings")]
df$stock <- refTable$stock[match(df$stock, refTable$stock_short)]
names(df)[4] <- "value"

plot_alluvial(data = df, refTable = refTable,
  group_vars = c("fleet", "metier", "stock"),
  fill_var = c("stock"))

# doesn't make sense, but redefine group_var order
plot_alluvial(data = df, refTable = refTable,
  group_vars = c("fleet", "stock", "metier"),
  fill_var = c("stock"))

# a 2-level version
plot_alluvial(data = df, refTable = refTable,
  group_vars = c("fleet", "stock"),
  fill_var = c("stock"))

# repel labels
plot_alluvial(data = df, refTable = refTable,
  group_vars = c("fleet", "metier", "stock"),
  text_repel = TRUE, text_size = 2)

# repel labels and suppress legend
plot_alluvial(data = df, refTable = refTable,
  text_repel = TRUE, text_size = 2, addLegend = FALSE)






#' Interactive alluvial plot
#'
#' @param data data.frame containing grouping variables and a `value` column.
#' @param refTable data.frame containing fill-variable lookup information,
#'   including `col` and `order`.
#' @param group_vars vector of variable names defining the alluvial axes.
#' @param fill_var character. Variable used to colour the flows.
#' @param group_labs labels for the alluvial axes.
#' @param text_repel retained for compatibility with plot_alluvial().
#' @param text_size text size.
#' @param mult_x retained for compatibility with plot_alluvial().
#' @param nudge_x retained for compatibility with plot_alluvial().
#' @param stratum_width approximate node width.
#' @param stratum_col colour for strata not corresponding to fill_var.
#' @param xlab x-axis label.
#' @param ylab value label used in hover text.
#' @param fillLegendTitle legend title.
#' @param addLegend logical. Add colour legend.
#' @param plotTitle plot title.
#'
#' @return plotly object
#'
#' @export
plot_alluvial_plotly <- function(
    data,
    refTable,
    group_vars = c("fleet", "metier", "stock"),
    fill_var = "stock",
    group_labs = NULL,
    text_repel = FALSE,
    text_size = 11,
    mult_x = c(0.1, 0.1),
    nudge_x = 1/3,
    stratum_width = 1/3,
    stratum_col = "white",
    xlab = NULL,
    ylab = "Catch [t]",
    fillLegendTitle = "Stock",
    addLegend = TRUE,
    plotTitle = NULL) {

  # ------------------------------------------------------------------
  # Checks
  # ------------------------------------------------------------------

  if (length(fill_var) != 1) {
    stop("fill_var must contain exactly one variable name")
  }

  if (!fill_var %in% names(refTable)) {
    stop("fill_var must be variable in refTable")
  }

  required_vars <- c(group_vars, fill_var, "value")
  missing_vars <- setdiff(required_vars, names(data))

  if (length(missing_vars) > 0) {
    stop(
      paste(
        "Missing required variables in data:",
        paste(missing_vars, collapse = ", ")
      )
    )
  }

  if (is.null(group_labs)) group_labs <- group_vars

  if (length(group_labs) != length(group_vars)) {
    stop("group_labs must have the same length as group_vars")
  }

  if (length(group_vars) < 2) {
    stop("group_vars must contain at least two variables")
  }


  # ------------------------------------------------------------------
  # Colour lookup - same basic logic as plot_alluvial()
  # ------------------------------------------------------------------

  palFill <- data.frame(
    tmp = unique(as.character(data[[fill_var]])),
    stringsAsFactors = FALSE
  )

  names(palFill) <- fill_var

  palFill <- merge(
    x = palFill,
    y = refTable,
    by = fill_var,
    all.x = TRUE
  )

  if ("order" %in% names(palFill)) {
    palFill <- palFill[order(palFill$order), , drop = FALSE]
  }

  palColors <- palFill$col
  names(palColors) <- as.character(palFill[[fill_var]])

  # Fallback colour if a fill value is missing from refTable
  palColors[is.na(palColors)] <- "#BDBDBD"


  # ------------------------------------------------------------------
  # Clean data
  # ------------------------------------------------------------------

  dat <- data

  dat$value <- as.numeric(dat$value)

  dat <- dat[
    !is.na(dat$value) &
      dat$value > 0,
    ,
    drop = FALSE
  ]

  for (v in group_vars) {
    dat[[v]] <- as.character(dat[[v]])
  }

  dat[[fill_var]] <- as.character(dat[[fill_var]])


  # ------------------------------------------------------------------
  # Create unique node IDs
  #
  # Important: "BE" under Fleet and "BE" under another axis must be
  # treated as different nodes.
  # ------------------------------------------------------------------

  node_tables <- lapply(seq_along(group_vars), function(i) {
    var <- group_vars[i]

    vals <- unique(dat[[var]])

    data.frame(
      axis_no = i,
      variable = var,
      label = vals,
      node_id = paste(i, vals, sep = "__"),
      stringsAsFactors = FALSE
    )
  })

  nodes <- dplyr::bind_rows(node_tables)

  nodes$index <- seq_len(nrow(nodes)) - 1


  # ------------------------------------------------------------------
  # Node colours
  #
  # Only the axis corresponding to fill_var receives stock colours.
  # Other strata retain stratum_col, matching the original intention.
  # ------------------------------------------------------------------

  nodes$color <- stratum_col

  fill_axis <- which(group_vars == fill_var)

  if (length(fill_axis) > 0) {
    idx <- nodes$axis_no %in% fill_axis

    matched_col <- palColors[nodes$label[idx]]

    nodes$color[idx] <- ifelse(
      is.na(matched_col),
      stratum_col,
      matched_col
    )
  }


  # ------------------------------------------------------------------
  # Node totals for hover
  # ------------------------------------------------------------------

  node_totals <- lapply(seq_along(group_vars), function(i) {
    var <- group_vars[i]

    tmp <- dat %>%
      dplyr::group_by(.data[[var]]) %>%
      dplyr::summarise(
        total = sum(value, na.rm = TRUE),
        .groups = "drop"
      )

    names(tmp)[1] <- "label"

    tmp$axis_no <- i
    tmp
  }) %>%
    dplyr::bind_rows()

  nodes <- nodes %>%
    dplyr::left_join(
      node_totals,
      by = c("axis_no", "label")
    )


  # ------------------------------------------------------------------
# Build links between every pair of adjacent axes
# ------------------------------------------------------------------

link_list <- vector("list", length(group_vars) - 1)

for (i in seq_len(length(group_vars) - 1)) {

  left_var  <- group_vars[i]
  right_var <- group_vars[i + 1]

  # Avoid duplicate grouping variables when fill_var is also
  # one of the two axes (e.g. stock -> stock).
  vars_to_group <- unique(c(left_var, right_var, fill_var))

  links <- dat %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(vars_to_group))) %>%
    dplyr::summarise(
      value = sum(value, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::transmute(
      source_label = as.character(.data[[left_var]]),
      target_label = as.character(.data[[right_var]]),
      fill_value = as.character(.data[[fill_var]]),
      value = value
    )

  source_lookup <- nodes %>%
    dplyr::filter(axis_no == i) %>%
    dplyr::select(
      source_label = label,
      source = index
    )

  target_lookup <- nodes %>%
    dplyr::filter(axis_no == i + 1) %>%
    dplyr::select(
      target_label = label,
      target = index
    )

  links <- links %>%
    dplyr::left_join(source_lookup, by = "source_label") %>%
    dplyr::left_join(target_lookup, by = "target_label")

  links$source_var <- left_var
  links$target_var <- right_var

  link_list[[i]] <- links
}

links <- dplyr::bind_rows(link_list)


  # ------------------------------------------------------------------
  # Link colours
  # ------------------------------------------------------------------

  links$color <- palColors[
    as.character(links$fill_value)
  ]

  links$color[is.na(links$color)] <- "#BDBDBD"


  # Make links slightly transparent
  hex_to_rgba <- function(cols, alpha = 0.55) {

    vapply(cols, function(col) {

      rgb <- grDevices::col2rgb(col)

      sprintf(
        "rgba(%d,%d,%d,%.2f)",
        rgb[1],
        rgb[2],
        rgb[3],
        alpha
      )

    }, character(1))
  }

  links$color_rgba <- hex_to_rgba(
    links$color,
    alpha = 0.55
  )


  # ------------------------------------------------------------------
  # Hover text
  # ------------------------------------------------------------------

  links$hover <- paste0(
    links$source_var, ": ", links$source_label,
    "<br>",
    links$target_var, ": ", links$target_label,
    "<br>",
    fillLegendTitle, ": ", links$fill_value,
    "<br>",
    ylab, ": ", format(
      round(links$value, 1),
      big.mark = ",",
      trim = TRUE
    )
  )

  nodes$hover <- paste0(
    nodes$variable, ": ", nodes$label,
    "<br>",
    ylab, ": ", format(
      round(nodes$total, 1),
      big.mark = ",",
      trim = TRUE
    )
  )


  # ------------------------------------------------------------------
  # Fixed x positions for each alluvial axis
  # ------------------------------------------------------------------

  axis_positions <- seq(
    0.02,
    0.98,
    length.out = length(group_vars)
  )

  nodes$x <- axis_positions[
    nodes$axis_no
  ]


  # ------------------------------------------------------------------
  # Create Plotly Sankey
  # ------------------------------------------------------------------

  p <- plotly::plot_ly(
    type = "sankey",
    arrangement = "snap",

    node = list(
      label = nodes$label,
      x = nodes$x,
      color = nodes$color,

      # Plotly thickness is in pixels, so this is only an
      # approximation of ggalluvial's stratum_width.
      thickness = max(
        10,
        round(30 * stratum_width / (1/3))
      ),

      pad = 10,

      line = list(
        color = "#808080",
        width = 0.5
      ),

      customdata = nodes$hover,
      hovertemplate = "%{customdata}<extra></extra>"
    ),

    link = list(
      source = links$source,
      target = links$target,
      value = links$value,
      color = links$color_rgba,
      customdata = links$hover,
      hovertemplate = "%{customdata}<extra></extra>"
    )
  )


  # ------------------------------------------------------------------
  # Axis labels
  # ------------------------------------------------------------------

  axis_annotations <- lapply(
    seq_along(group_vars),
    function(i) {

      list(
        x = axis_positions[i],
        y = 1.05,
        xref = "paper",
        yref = "paper",
        text = group_labs[i],
        showarrow = FALSE,
        xanchor = "center",
        font = list(
          size = text_size + 1
        )
      )
    }
  )


  # ------------------------------------------------------------------
  # Optional colour legend
  #
  # Sankey traces do not have a conventional categorical legend,
  # so we create a compact annotation-based one.
  # ------------------------------------------------------------------

  legend_annotations <- list()

  if (addLegend) {

    legend_data <- palFill[
      !is.na(palFill[[fill_var]]),
      ,
      drop = FALSE
    ]

    if (nrow(legend_data) > 0) {

      legend_annotations <- c(
        list(
          list(
            x = 1.03,
            y = 1,
            xref = "paper",
            yref = "paper",
            text = paste0(
              "<b>",
              fillLegendTitle,
              "</b>"
            ),
            showarrow = FALSE,
            xanchor = "left"
          )
        ),

        lapply(seq_len(nrow(legend_data)), function(i) {

          col <- legend_data$col[i]
          if (is.na(col)) col <- "#BDBDBD"

          list(
            x = 1.03,
            y = 1 - i * 0.045,
            xref = "paper",
            yref = "paper",
            text = paste0(
              "<span style='color:",
              col,
              "; font-size:18px;'>■</span> ",
              legend_data[[fill_var]][i]
            ),
            showarrow = FALSE,
            xanchor = "left",
            align = "left"
          )
        })
      )
    }
  }


  # ------------------------------------------------------------------
  # Layout
  # ------------------------------------------------------------------

  right_margin <- if (addLegend) 170 else 30

  p <- p %>%
    plotly::layout(
      title = list(
        text = plotTitle,
        x = 0.5,
        xanchor = "center"
      ),

      font = list(
        size = text_size
      ),

      annotations = c(
        axis_annotations,
        legend_annotations
      ),

      margin = list(
        l = 30,
        r = right_margin,
        t = 70,
        b = 40
      )
    )


  return(p)
}



plot_alluvial_plotly(
  data = df,
  refTable = refTable,
  group_vars = c("fleet", "stock", "metier"),
  fill_var = "stock"
)



if (FALSE) { # rlang::is_interactive()
items <- lapply(LETTERS, function(x) {
  accordion_panel(paste("Section", x), paste("Some narrative for section", x))
})

# First shown by default
accordion(!!!items)
# Nothing shown by default
accordion(!!!items, open = FALSE)
# Everything shown by default
accordion(!!!items, open = TRUE)

# Show particular sections
accordion(!!!items, open = "Section B")
accordion(!!!items, open = c("Section A", "Section B"))

# Provide an id to create a shiny input binding
library(shiny)
library(bslib)

ui <- page_fluid(
  accordion(!!!items, id = "acc")
)

server <- function(input, output) {
  observe(print(input$acc))
}

shinyApp(ui, server)
}


library(httr)

    # Define the JWT token
    jwt_token <- "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoibHVjYS5sYW1vbmlAaWNlcy5kayIsImp0aSI6IjQwMjMzNjdmLTM0YmEtNGM0NC1iYWZkLTBlMmI3YjJhNTBiMSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL2VtYWlsYWRkcmVzcyI6Imx1Y2EubGFtb25pQGljZXMuZGsiLCJVc2VyRW1haWwiOiJsdWNhLmxhbW9uaUBpY2VzLmRrIiwiRW1haWwiOiJsdWNhLmxhbW9uaUBpY2VzLmRrIiwiZXhwIjoxNzkzNDM4NzU5LCJpc3MiOiJodHRwOi8vdGFmLmljZXMuZGsiLCJhdWQiOiJodHRwOi8vdGFmLmljZXMuZGsifQ.sSefrqRnEiWpUWZ7kLiDMjmhBTi76JUrbhos8FuL-s4"

    # Define the API endpoint
    url <- "https://sag.ices.dk/API/API/UpdateSag/Run"

    # Make the request with the Authorization header
    response <- GET(
      url,
      add_headers(Authorization = paste("Bearer", jwt_token))
    )

     # Print response status and content
    print(status_code(response))
    print(content(response, as = "text", encoding = "UTF-8"))
