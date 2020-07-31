#' A list of auxiliary functions for data processing
#'
#' @author Daniel Vartanian
#' 
#' @description
#' 
#' * Version: 1.0.2 2020-05-10
#' * Style guide: The tidyverse style guide (2019) 
#'   <https://style.tidyverse.org/>
#' 
#' @note
#' 
#' Help functions:
#' 
#' * setwd(normalizePath(readClipboard(), "/", mustWork = FALSE))
#' * assign("last.warning", NULL, envir = baseenv())
#' * loadfonts("win", quiet = TRUE)
#' * windows()
#' 
#' @noRd


auxiliary_functions <- list(
    
    count_row_values = function(data) {
        
        output <- data
        names <- names(output)
        
        count <- vector(mode = "integer", length = 0)
        
        for (i in seq_len(nrow(output))) {
            
            count_i <- 0
            
            for (j in seq_along(output)) {
                
                suppressWarnings(
                    
                    if (is.na(output[[i, j]])) {
                        
                        next
                        
                    } else {
                        
                        count_i <- count_i + 1
                        
                    }
                    
                )
                
            }
            
            count <- append(count, count_i)
            
        }
        
        output$count <- count
        
        output <- output %>% select(count, names)
        
    },
    
    age_calc_ = function(birth_date,
                         date = lubridate::now(),
                         units = "years",
                         precise = FALSE) {
        
        # Load packages --------------------
        
        require(lubridate)
        require(eeptools)
        
        # Check values --------------------
        
        for (i in c("birth_date", "date")) {
            
            if (is.na(get(i))) {
                
                assign(i, as.Date(NA))
                
            }
            
            if (is.character(get(i))) {
                
                assign(i, as_date(get(i)))
                
            }
            
            if (!(is.Date(get(i)))) {
                
                stop(paste(i, "value is not Date"))
                
            }
            
        }
        
        if (!(units %in% c("days", "months", "years"))) {
            
            stop("units must be 'days', 'months', or 'years'")
            
        }
        
        for (i in c("precise")) {
            
            if (!(is.logical(get(i)))) {
                
                stop(paste(i, "value is not logical"))
                
            }
            
        }
        
        # Compute output --------------------
        
        if (!(is.na(birth_date)) &&
            !(is.na(date))) {
            
            output <- as.numeric(age_calc(birth_date, 
                                          date, 
                                          units = units, 
                                          precise = precise))
            
        } else {
            
            output <- as.numeric(NA)
        }
        
        # Return output --------------------
        
        output
        
    },
    
    left_join_ = function(x, 
                          y, 
                          by,
                          columns) {
        
        # Load packages --------------------
        
        require(dplyr)
        
        # Check values --------------------
        
        for (i in c("x", "y")) {
            
            if (!(is.data.frame(get(i)))) {
                
                stop(paste(i, "is not a data frame"))
                
            }
            
        }
        
        for (i in c(by, columns)) {
            
            if (!(i %in% names(x))) {
                
                stop(paste(i, "is not in x"))
                
            }
            
            if (!(i %in% names(y))) {
                
                stop(paste(i, "is not in y"))
                
            }
            
        }
        
        # Transform variables --------------------
        
        columns <- setdiff(columns, by)
        
        # Compute output --------------------
        
        output <- left_join(x, y, by = by)
        
        for (i in seq_len(nrow(output))) {
            
            for (j in columns) {
                
                if (is.null(output[[i, paste0(j, ".x")]]) ||
                    is.null(output[[i, paste0(j, ".y")]])) {
                    
                    next
                    
                }
                
                if (!(is.na(output[[i, paste0(j, ".x")]])) &&
                    !(is.na(output[[i, paste0(j, ".y")]])) &&
                    !(length(output[[i, paste0(j, ".x")]]) > 1) &&
                    !(length(output[[i, paste0(j, ".y")]]) > 1)) {
                    
                    if (nchar(as.character(output[[i, paste0(j, ".y")]])) >
                        nchar(as.character(output[[i, paste0(j, ".x")]]))) {
                        
                        output[[i, paste0(j, ".x")]] <- 
                            output[[i, paste0(j, ".y")]]  
                        
                    }
                    
                }
                
                if (is.na(output[[i, paste0(j, ".x")]]) &&
                    !(is.na(output[[i, paste0(j, ".y")]]))) {
                    
                    output[[i, paste0(j, ".x")]] <- 
                        output[[i, paste0(j, ".y")]]
                    
                }
                
            }
            
        }
        
        output <- output %>% select(seq_along(x))
        colnames(output) <- colnames(x)
        
        # Return output --------------------
        
        output
        
    },
    
    as_character_ = function(data) {
        
        # Load packages --------------------
        
        require(dplyr)
        
        # Check values --------------------
        
        for (i in c("data")) {
            
            if (!(is.data.frame(get(i)))) {
                
                stop(paste(i, "is not a data frame"))
                
            }
            
        }
        
        # Transform variables --------------------
        
        output <- data %>% 
            auxiliary_functions$unlist_() %>% 
            mutate_if(function(x) !(is.character(x)), as.character)
        
        # Return output --------------------
        
        output
        
    },
    
    anti_join_ = function(x, 
                          y, 
                          by) {
        
        # Load packages --------------------
        
        require(dplyr)
        
        # Check values --------------------
        
        for (i in by) {
            
            if (!(i %in% names(x))) {
                
                stop(paste(i, "is not in x"))
                
            }
            
            if (!(i %in% names(y))) {
                
                stop(paste(i, "is not in y"))
                
            }
            
        }
        
        # Compute output --------------------
        
        output <- anti_join(x, y, by = by)
        output <- output %>% select(seq_along(x))
        colnames(output) <- colnames(x)
        
        # Return output --------------------
        
        return(output)
        
    },
    
    unlist_ = function(data) {

        level_1 = function(x) {
            
            level_2 = function(y) {
                
                paste(unlist(y), collapse = ",")
                
            }
            
            na_if(sapply(x, level_2), "NA")
            
        }
        
        output <- data %>% mutate_if(is.list, level_1)
        
        output
        
    },
    
    convert_to_hms = function(x,
                              class = "data.frame") {
        
        require(lubridate)
        require(hms)
        
        level_1 = function(y) {
            
            as_hms(as_datetime(y))
            
        }
        
        if (class == "data.frame") {
        
            output <- x %>% mutate_if(is.duration, level_1)
            output <- output %>% mutate_if(is.period, level_1)
        
        }
        
        if (class == "vector") {
            
            output <- level_1(x)
            
        }
        
        output
        
    },
    
    re_validation = function(pattern, 
                             x, 
                             return_test = TRUE,
                             message = "", 
                             error = "warning") {
        
        #' Check if the pattern in x is true or false and returns a stop error 
        #' or warning message if false
        #'
        #' @description
        #'
        #' Args:
        #' 
        #' * pattern:     A string with a regular expression
        #' * x:           A value for the test
        #' * message:     A string with a message for the stop error or warning
        #' * error:       A string with a value ("stop" or "warning") to set 
        #'                the type of response
        #' * return_test: A logical value indicating if the function must
        #'                return a stop or warning response
        #'
        #' Return:
        #' 
        #' * A TRUE or FALSE value OR a stop error or warning message if the 
        #' pattern do not correspond to x
        
        # Load packages --------------------
        
        require(magrittr)
        
        # Check values --------------------
        
        stopifnot(is.character(pattern))
        stopifnot(!(is.na(x)) || !(is.null(x)))
        stopifnot(is.character(message))
        stopifnot(error == "stop" || error == "warning")
        stopifnot(is.logical(return_test))
        
        # Check pattern --------------------
        
        test <- grepl(pattern = pattern, 
                      as.character(x), ignore.case = TRUE)
        
        # Return value or error --------------------
        
        if (return_error == TRUE) {
            
            return(test)
            
        } else if (test == FALSE) {
            
            if (error == "stop") {
                
                stop(message)
                
            } else if (error == "warning") {
                
                warning(message)
                
            } else {
                
                stop("Error. See function.")
                
            }
            
        }
        
    },
    
    summarise_inline = function(data,
                                x, 
                                label_x) {
        
        #' Creates a one line summary statistics
        #'
        #' @description
        #'
        #' Args:
        #' 
        #' * data:    A data frame with the day_average dataset
        #' * x:       A string with the name of the variable for the 
        #'            x axis
        #' * label_x: A string with the x variable label
        #'
        #' Return:
        #' 
        #' * A string with the a statistical summary of x
        #' 
        #' Observations:
        #' 
        #' * This function requires the actimetry_analysis functions
        #' * This function only works with class numeric, Date,
        #'   and POSIXct objects
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(lubridate)
        require(hms)
        
        # Check values --------------------
        
        if (!(is.data.frame(data))) {
            
            stop("data is not a data frame")
            
        }
        
        for (i in c("x")) {
            
            if (!(get(i) %in% names(data))) {
                
                stop(paste(i, "variable can't be found in data"))
                
            }
            
        }
        
        for (i in c("label_x")) {
            
            if (!(is.character(get(i)))) {
                
                stop(paste(i, "is not character"))
                
            }
            
        }
        
        # Transform arguments --------------------
        
        data <- data %>% as_tibble
        
        # Compute output --------------------
        
        ## n
        
        n <- paste0("n = ", length(which(!(is.na(data[[x]])))))
        
        ## Normality test
        
        if (!(is.logical(data[[x]])) ||
            !(is.character(data[[x]]))) {
            
            test_normality <- data %>%
                actimetry_analysis$test_normality(x = x, 
                                                  print = FALSE, 
                                                  plot = FALSE, 
                                                  return_data = TRUE)
            
            if ("test_shapiro" %in% names(test_normality)) {
                
                if (test_normality$test_shapiro$p.value > 0.05) {
                    
                    test_normality <- paste0(
                        "SW p-value \u2248 ",
                        format(round(test_normality$test_shapiro$p.value,
                                     digits = 3), 
                               nsmall = 3,
                               scientific = FALSE),
                        " (Can assume normality)")
                    
                } else {
                    
                    test_normality <- paste0(
                        "SW p-value \u2248 ",
                        format(round(test_normality$test_shapiro$p.value,
                                     digits = 3), 
                               nsmall = 3,
                               scientific = FALSE),
                        " (Cannot assume normality)")
                    
                }
                
            } else if ("test_lcks" %in% names(test_normality)) {
                
                if (test_normality$test_lcks$p.value > 0.05) {
                    
                    test_normality <- paste0(
                        "LcKS p-value \u2248 ",
                        format(round(test_normality$test_lcks$p.value,
                                     digits = 3), 
                               nsmall = 3,
                               scientific = FALSE),
                        " (Can assume normality)")
                    
                } else {
                    
                    test_normality <- paste0(
                        "LcKS p-value \u2248 ",
                        format(round(test_normality$test_lcks$p.value,
                                     digits = 3), 
                               nsmall = 3,
                               scientific = FALSE),
                        " (Cannot assume normality)")
                    
                }
                
            }
            
        } else {
            
            test_normality <- NA
        }
        
        ## Mean and standard deviation
        
        if (is.numeric(data[[x]])) {
            
            mean <- paste0("Mean = ", round(mean(data[[x]]), 3))
            sd <- paste0("Std. dev. = ", round(sd(data[[x]]), 3))
            
        } else if (is.Date(data[[x]])) {
            
            mean <- mean(data[[x]])
            mean <- as.Date(mean)
            mean <- paste0("Mean = ", mean)
            
            sd <- sd(data[[x]])
            sd <- as.Date(sd)
            sd <- paste0("Std. dev. = ", sd)
            
        } else if (is.POSIXct(data[[x]])) {
            
            if (date(data[[x]][1]) > ymd("1971-01-01")) {
                
                mean <- mean(data[[x]])
                mean <- as.POSIXct(mean, origin = origin)
                mean <- paste0("Mean = ", mean)
                
                sd <- sd(data[[x]])
                sd <- as_hms(round(sd(data[[x]])))
                sd <- paste0("Std. dev. = ", sd)
                
            } else {
                
                mean <- paste0("Mean = ", as_hms(round(mean(data[[x]]))))
                sd <- paste0("Std. dev. = ", as_hms(round(sd(data[[x]]))))
                
            }
            
        } else {
            
            mean <- NA
            sd <- NA
            
        }
        
        # Return output --------------------
        
        if (!(is.na(mean)) && 
            !(is.na(sd)) && 
            !(is.na(test_normality))) {
            
            output <- paste0(label_x, ": ",
                             n, " | ", 
                             mean, " | ",
                             sd, " | ",
                             test_normality)
            
        } else {
            
            output <- paste0(label_x, ": ",
                             n)
            
        }
        
        ## Not run:
        ##
        ## "Min. = ", round(min(data[[x]]), 3)
        ## "Median = ", round(median(data[[x]])
        ## "Max. = ", round(max(data[[x]]), 3)
        ## "Range = ", round(range(data[[x]])
        
        return(output)
        
    },
    
    flat_posixct = function(data,
                            x) {
        
        output <- data
        
        date(output[[x]]) <- "1970-01-01"
        
        output
        
    },
    
    midday_change = function(data,
                             x) {
        
        output <- data
        
        for (i in seq_len(length(output[[x]]))) {
            
            if (!is.na(output[[x]][i])) {
                
                if (hour(output[[x]][i]) < 12) {
                    
                    day(output[[x]][i]) <- 2
                    
                }
                
            }
            
        }
        
        return(output)
        
    },
    
    convert_to_posixct = function(data,
                                  x) {
        
        require(lubridate)
        
        output <- data
        
        if (is.duration(data[[x]]) ||
            is.period(data[[x]])) {
            
            output <- output %>% 
                mutate(!!as.symbol(x) := as_datetime(!!as.symbol(x)))
            
        }
        
        return(output)
        
    },
    
    date_fix = function(data,
                        x) {
        
        require(dplyr)
        require(stringr)
        
        output <- data
        
        for (i in x) {
            
            output <- output %>% mutate(!!as.symbol(i) := case_when(
                ## 0/00/0000
                str_detect(as.character(!!as.symbol(i)), 
                           "^[0-9]/") ~ 
                    paste0("0", as.character(!!as.symbol(i))),
                ## 00/0/0000
                str_detect(as.character(!!as.symbol(i)), 
                           "^[0-9][0-9]/[0-9]/") ~ 
                    paste0(str_extract(as.character(!!as.symbol(i)), 
                                       "^[0-9][0-9]/"),
                           "0", 
                           str_sub(as.character(!!as.symbol(i)), 4)),
                ## 0/0/0000
                str_detect(as.character(!!as.symbol(i)), 
                           "^[0-9][0-9]/[0-9]/") ~ 
                    paste0("0",
                           str_extract(as.character(!!as.symbol(i)), 
                                       "^[0-9]/"),
                           "0", 
                           str_sub(as.character(!!as.symbol(i)), 3)),
                TRUE ~ !!as.symbol(i)))
            
        }
        
        output
        
    },
    
    hour_fix = function(data,
                        x) {
        
        # Load packages --------------------
        
        require(dplyr)
        require(stringr)
        
        # Transform variables --------------------
        
        output <- data
        
        for (i in x) {
            
            output <- output %>% 
                mutate(!!as.symbol(i) := str_trim(!!as.symbol(i)))
            
            output <- output %>% 
                mutate(!!as.symbol(i) := case_when(
                    ## PM & < 12
                    str_detect(as.character(!!as.symbol(i)), "PM") &
                        as.numeric(str_extract(as.character(!!as.symbol(i)),
                                    "^[0-9]{1,2}")) < 12 ~ 
                        paste0(as.numeric(
                            str_extract(as.character(!!as.symbol(i)),
                                        "^[0-9]{1,2}")) + 12, 
                            if_else(
                                is.na(str_extract(str_trim(
                                    str_replace(as.character(!!as.symbol(i)), 
                                                "PM", "")),
                                    ":[0-9:]*$")), 
                                "", 
                                str_extract(str_trim(
                                    str_replace(as.character(!!as.symbol(i)), 
                                                "PM", "")),
                                    ":[0-9:]*$"))),
                    ## PM & >= 12
                    str_detect(as.character(!!as.symbol(i)), "PM") &
                        as.numeric(str_extract(as.character(!!as.symbol(i)),
                                               "^[0-9]{1,2}")) >= 12 ~ 
                        str_trim(str_replace(
                            as.character(!!as.symbol(i)), "PM", "")),
                    ## AM & == 12
                    str_detect(as.character(!!as.symbol(i)), "AM") &
                        as.numeric(str_extract(as.character(!!as.symbol(i)),
                                               "^[0-9]{1,2}")) == 12 ~ 
                        paste0("00",
                               if_else(
                                   is.na(str_extract(str_trim(
                                       str_replace(as.character(!!as.symbol(i)), 
                                                   "AM", "")),
                                       ":[0-9:]*$")), 
                                   "", 
                                   str_extract(str_trim(
                                       str_replace(as.character(!!as.symbol(i)), 
                                                   "AM", "")),
                                       ":[0-9:]*$"))),
                    ## AM
                    str_detect(as.character(!!as.symbol(i)), "AM") ~ 
                        str_trim(str_replace(
                            as.character(!!as.symbol(i)), "AM", "")),
                    TRUE ~ !!as.symbol(i)))
            
            output <- output %>% 
                mutate(!!as.symbol(i) := case_when(
                    ## 0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]$") ~ 
                        paste0("0",
                               as.character(!!as.symbol(i)), 
                               ":00:00"),
                    ## 00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               ":00:00"),
                    ## 00:
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               "00:00"),
                    ## 00:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9]$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               "0:00"),
                    ## 00:00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9][0-9]$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               ":00"),
                    ## 00:00:
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9][0-9]:$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               "00"),
                    ## 00:00:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9][0-9]:[0-9]$") ~ 
                        paste0(as.character(!!as.symbol(i)), 
                               "0"),
                    ## 0:0:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9]:[0-9]$") ~ 
                        paste0("0",
                               str_extract(as.character(!!as.symbol(i)), 
                                           "^[0-9]:"),
                               "0", 
                               str_sub(as.character(!!as.symbol(i)), 3, 3),
                               ":0",
                               str_extract(as.character(!!as.symbol(i)), 
                                           "[0-9]$")),
                    ## 00:0:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9]:[0-9]$") ~ 
                        paste0(str_extract(as.character(!!as.symbol(i)), 
                                           "^[0-9][0-9]:"),
                               "0",
                               str_sub(as.character(!!as.symbol(i)), 4, 4),
                               ":0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "[0-9]$")),
                    ## 0:0:00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9]:[0-9][0-9]$") ~ 
                        paste0("0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9]:"),
                               "0",
                               str_sub(as.character(!!as.symbol(i)), 3, 3),
                               str_detect(as.character(!!as.symbol(i)), 
                                          ":[0-9][0-9]$")),
                    ## 0:00:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9][0-9]:[0-9]$") ~ 
                        paste0("0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9]:[0-9][0-9]:"),
                               "0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "[0-9]$")),
                    ## 0:00:00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9][0-9]:[0-9][0-9]$") ~ 
                        paste0("0",
                               as.character(!!as.symbol(i))),
                    ## 00:0:00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9]:[0-9][0-9]$") ~ 
                        paste0(str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9][0-9]:"),
                               "0",
                               str_sub(as.character(!!as.symbol(i)), 4, 4),
                               str_detect(as.character(!!as.symbol(i)), 
                                          ":[0-9][0-9]$")),
                    ## 00:00:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9][0-9]:[0-9]$") ~ 
                        paste0(str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9][0-9]:[0-9][0-9]:"),
                               "0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "[0-9]$")),
                    ## 0:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9]$") ~ 
                        paste0("0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9]:"),
                               "0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "[0-9]$"),
                               ":00"),
                    ## 0:00
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9]:[0-9][0-9]$") ~ 
                        paste0("0",
                               as.character(!!as.symbol(i)),
                               ":00"),
                    ## 00:0
                    str_detect(as.character(!!as.symbol(i)), 
                               "^[0-9][0-9]:[0-9]$") ~ 
                        paste0(str_detect(as.character(!!as.symbol(i)), 
                                          "^[0-9][0-9]:"),
                               "0",
                               str_detect(as.character(!!as.symbol(i)), 
                                          "[0-9]$"),
                               ":00"),
                    TRUE ~ !!as.symbol(i)))
            
        }
        
        # Return output --------------------
        
        output
        
    },
    
    test = function() {
        
        print("test")
        
    }
    
)