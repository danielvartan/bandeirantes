#' A list of functions for processing the bandeirantes project data
#'
#' @author Daniel Vartanian.
#'
#' @description
#' 
#' * Version: 1.0.0 2020-07-30
#' * Style guide: The tidyverse style guide (2019) 
#'   <https://style.tidyverse.org/>
#' 
#' @details
#' 
#' * Set the working directory to the project's root directory
#' 
#' @note
#' 
#' Help functions:
#' 
#' * setwd(normalizePath(readClipboard(), "/", mustWork = FALSE))
#' * assign("last.warning", NULL, envir = baseenv())
#' * list2env(processing_functions$read_input_data(), environment())
#' * list2env(processing_functions$read_valid_data(), environment())
#' 
#' @seealso
#' 
#' * GitHub: <https://github.com/danielvartan/bandeirantes>
#' 
#' @noRd


require(rprojroot)
setwd(find_rstudio_root_file())

processing_functions <- list(
    
    #' Process and creates the input datasets
    #'
    #' @param dataset A character vector with the names of the datasets that 
    #'   the function must write/return. 
    #'   Note: use "all" to write/return all datasets.
    #' @param return_data A logic value indicating if the function must
    #'   return the output data.
    #' @param write_data A logic value indicating if the function must 
    #'   write the output data.
    #'
    #' @return
    #' 
    #' * If return_data is TRUE and dataset is length 1, returns a tibble
    #' * If return_data is TRUE and dataset is "all" or length > 1, returns 
    #'   a list with tibbles
    #' * If write_data is TRUE, write a RData and csv file for all datasets 
    #'   in dataset
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * This function requires the auxiliary_functions.R list of 
    #'   functions
    #' * This function requires the tidy_functions.R list of functions
    #' * The output paths are preconfigured.
    #' 
    #' @noRd
    
    input_data = function(dataset = "all",
                          return_data = TRUE,
                          write_data = TRUE) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        require(progress)

        # Set parameters --------------------
        
        source_files <- list(
            auxiliary_functions = "./scripts/auxiliary/auxiliary_functions.R",
            tidy_functions = "./scripts/input/tidy_functions.R")
        
        raw_paths <- list(
            bandeirante = paste0(".",
                                 "/data/raw/bandeirante/"),
            qualocep = paste0(".", 
                              "/data/raw/qualocep/"))
        
        input_paths <- list(
            bandeirante = paste0(".",
                                 "/data/input/bandeirante/"),
            qualocep = paste0(".",
                              "/data/input/qualocep/"))
        
        # Check arguments --------------------
        
        for (i in c("dataset")) {
            
            if (!(is.character(get(i)))) {
                
                stop(paste(i, "value is not character"))
                
            }
            
        }
        
        for (i in dataset) {
            
            if (!(i %in% c("all", names(input_paths)))) {
                
                stop(paste(i, "value is not valid"))
                
            }
            
        }
        
        for (i in c("return_data", "write_data")) {
            
            if (!(is.logical(get(i)))) {
                
                stop(paste(i, "value is not logical"))
                
            }
        }
        
        if (isFALSE(return_data) &&
            isFALSE(write_data)) {
            
            stop(paste("What you want to do champ?", 
                       "return_data and write_data can't both be FALSE!",
                       "Help me out here."))
            
        }
        
        # Source functions --------------------
        
        for (i in names(source_files)) {
            
            source(source_files[[i]], local = TRUE, encoding = "UTF-8")
            
        }
        
        # Set dataset values --------------------
        
        if ("all" %in% dataset) {
            
            raw_datasets <- names(raw_paths)
            input_datasets <- names(input_paths)
            
        } else {
            
            raw_datasets <- dataset
            input_datasets <- dataset
            
        }
        
        # Compute the input data --------------------
        
        ## Load progress bar
        
        message(paste0("\n",
                      "Computing input data. This may take a while."))
        
        pb <- progress_bar$new(
            format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
            total = length(raw_datasets), 
            clear = FALSE, 
            show_after = 0)
        
        pb$tick(0)
        
        ## Compute data
        
        for (i in raw_datasets) {
        
        ### Helper: setwd(find_rstudio_root_file())
            
            wd <- getwd()
            setwd(raw_paths[[i]])
            
            file <- str_subset(dir(), ".csv$")
            assign(i, tidy_functions[[i]](file = file))
            
            setwd(wd)
            pb$tick()
            
        }
        
        # Write data --------------------
        
        if (isTRUE(write_data)) {
            
            for (i in input_datasets) {
                
                if (!(str_extract(input_paths[[i]], ".$") == "/")) {
                    
                    input_paths[[i]] <- paste0(input_paths[[i]], "/")
                    
                }
                
                ## Prepare csv output
                
                unlist_ <- auxiliary_functions$unlist_

                output_csv <- get(i) %>% unlist_()
                
                ## Write data
                
                output_csv %>% 
                    write_delim(paste0(input_paths[[i]], i, ".csv"), 
                                delim = ",",
                                col_names = TRUE)
                
                get(i) %>% saveRDS(paste0(input_paths[[i]], i, ".RData"))   
                
            }
            
        }
        
        # Return output --------------------
        
        if (length(input_datasets) == 1 & isTRUE(return_data)) {
            
            return(get(input_datasets))
            
        } else if (isTRUE(return_data)) {
            
            output <- vector(mode = "list", length = 0)
            
            for (i in input_datasets) {
                
                output[[i]] <- get(i)
                
            }
            
            return(output)
            
        }
        
    },
    
    #' Read the input datasets
    #'
    #' @param dataset A character vector with the names of the datasets that 
    #'   the function must write/return. 
    #'   Note: use "all" to write/return all datasets.
    #'
    #' @return 
    #' 
    #' * If dataset is length 1, returns a tibble
    #' * If dataset is "all" or length > 1, returns a list with tibbles
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * The .RData file must already be created.
    #' 
    #' @noRd
    
    read_input_data = function(dataset = "all") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        
        # Set parameters --------------------
        
        input_files <- list(
            bandeirante = paste0(".",
                            "/data/input/bandeirante/", 
                            "bandeirante.RData"),
            qualocep = paste0(".",
                              "/data/input/qualocep/", 
                              "qualocep.RData"))
        
        # Check arguments --------------------
        
        for (i in c("dataset")) {
            
            if (!(is.character(get(i)))) {
                
                stop(paste(i, "value is not character"))
                
            }
            
        }
        
        for (i in dataset) {
            
            if (!(i %in% c("all", names(input_files)))) {
                
                stop(paste(i, "value is not valid"))
                
            }
            
            if (!(i == "all")) {
                
                if (!(file.exists(input_files[[i]]))) {
                    
                    stop(paste(i, "file not found"))
                    
                }
                
            }
        }
        
        # Transform variables --------------------
        
        if ("all" %in% dataset) {
            
            dataset <- names(input_files)
            
        }
        
        # Load input data --------------------
        
        for (i in dataset) {
            
            assign(i, readRDS(input_files[[i]]))
            
        }
        
        # Return output --------------------
        
        if (length(dataset) == 1) {
            
            return(get(dataset))
            
        } else {
            
            output <- vector(mode = "list", length = 0)
            
            for (i in dataset) {
                
                output[[i]] <- get(i)
                
            }
            
            return(output)
            
        }
        
    },
    
    #' Process and creates the valid datasets
    #'
    #' @param return_data A logic value indicating if the function must
    #'   return the output data.
    #' @param write_data A logic value indicating if the function must 
    #'   write the output data.
    #'
    #' @return
    #' 
    #' * If return_data is TRUE, returns a list with tibbles
    #' * If write_date is TRUE, write several RData and csv files
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * This function requires the input datasets. Run the
    #'   input_data function before this one.
    #' * This function requires an internet connection
    #' * This function requires the auxiliary_functions.R list of 
    #'   functions
    #' * This function requires the adjustment_functions.R list 
    #'   of functions
    #' * This function requires the validation_functions.R list of
    #'   functions
    #' * The output paths are preconfigured.
    #' 
    #' @noRd
    
    valid_data = function(return_data = TRUE,
                          write_data = TRUE) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        require(curl)
        require(jsonlite)
        
        # Check arguments --------------------
        
        for (i in c("return_data", "write_data")) {
            
            if (!(is.logical(get(i)))) {
                
                stop(paste(i, "value is not logical"))
                
            }
        }
        
        if (isFALSE(return_data) &&
            isFALSE(write_data)) {
            
            stop(paste("What you want to do champ?", 
                       "return_data and write_data can't both be FALSE!",
                       "Help me out here."))
            
        }
        
        
        # Set parameters --------------------
        
        source_files <- list(
            auxiliary_functions = paste0(".", 
                                         "/scripts/auxiliary/",
                                         "auxiliary_functions.R"),
            adjustment_functions = paste0(".",
                                          "/scripts/valid/",
                                          "adjustment_functions.R"),
            validation_functions = paste0(".",
                                          "/scripts/valid/",
                                          "validation_functions.R"))
        
        input_files <- list(
            bandeirante = paste0(".",
                                 "/data/input/bandeirante/", 
                                 "bandeirante.RData"),
            qualocep = paste0(".",
                              "/data/input/qualocep/", 
                              "qualocep.RData"))
        
        valid_paths <- list(
            bandeirante = paste0(".",
                                 "/data/valid/bandeirante/"),
            qualocep = paste0(".",
                              "/data/valid/qualocep/"))
        
        # Source functions --------------------
        
        for (i in names(source_files)) {
            
            source(source_files[[i]], local = TRUE, encoding = "UTF-8")
            
        }
        
        # Load input data --------------------
        
        for (i in names(input_files)) {
            
            assign(i, readRDS(input_files[[i]]))
            
        }

        # Adjust data --------------------

        message(paste0("\n",
                       "Adjusting input datasets. ",
                       "This may take a while."))
        
        for (i in names(input_files)) {
        
            assign(i, adjustment_functions[[i]](data = get(i)))
            
        }
        
        # Validate data --------------------

        message(paste0("\n",
                      "Validating input datasets"))
        
        for (i in names(input_files)) {
            
            assign(i, validation_functions$main(data = get(i)))
            
        }
        
        # Write data --------------------
        
        if (isTRUE(write_data)) {
            
            for (i in names(valid_paths)) {
                
                if (!(str_extract(valid_paths[[i]], ".$") == "/")) {
                    
                    valid_paths[[i]] <- paste0(valid_paths[[i]], "/")
                    
                }
                
                ## Prepare csv output
                
                unlist_ <- auxiliary_functions$unlist_
                
                output_csv <- get(i) %>% unlist_()
                
                ## Write data
                
                output_csv %>% 
                    write_delim(paste0(valid_paths[[i]], i, ".csv"), 
                                delim = ",",
                                col_names = TRUE)
                    
                get(i) %>% saveRDS(paste0(valid_paths[[i]], i, ".RData"))   
                
            }
            
        }
        
        # Return output --------------------
        
        if (isTRUE(return_data)) {
            
            output <- vector(mode = "list", length = 0)
            
            for (i in names(valid_paths)) {
                
                output[[i]] <- get(i)
                
            }
            
            return(output)
            
        }
        
    },
    
    #' Read the valid datasets
    #'
    #' @param dataset A character vector with the names of the datasets that 
    #'   the function must write/return. 
    #'   Note: use "all" to write/return all datasets.
    #'
    #' @return 
    #' 
    #' * If dataset is length 1, returns a tibble
    #' * If dataset is "all" or length > 1, returns a list with tibbles
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * The .RData file must already be created.
    #' 
    #' @noRd
    
    read_valid_data = function(dataset = "all") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        
        # Set parameters --------------------

        valid_files <- list(
            bandeirante = paste0(".",
                             "/data/valid/bandeirante/",
                             "bandeirante.RData"),
            qualocep = paste0(".",
                              "/data/valid/qualocep/",
                              "qualocep.RData"))
        
        # Check arguments --------------------
        
        for (i in c("dataset")) {
            
            if (!(is.character(get(i)))) {
                
                stop(paste(i, "value is not character"))
                
            }
            
        }
            
        for (i in dataset) {
        
            if (!(i %in% c("all", names(valid_files)))) {
                
                stop(paste(i, "value is not valid"))
                
            }
            
            if (!(i == "all")) {
            
                if (!(file.exists(valid_files[[i]]))) {
                    
                    stop(paste(i, "file not found"))
                    
                }
                
            }
        }
        
        # Transform variables --------------------
        
        if ("all" %in% dataset) {
            
            dataset <- names(valid_files)
            
        }
        
        # Load valid data --------------------
        
        for (i in dataset) {
            
            assign(i, readRDS(valid_files[[i]]))
            
        }
        
        # Return output --------------------
        
        if (length(dataset) == 1) {
            
            return(get(dataset))
            
        } else {
            
            output <- vector(mode = "list", length = 0)
            
            for (i in dataset) {
                
                output[[i]] <- get(i)
                
            }
            
            return(output)
            
        }

    },
    
    #' Process and creates the analysis datasets
    #'
    #' @param return_data A logic value indicating if the function must
    #'   return the output data.
    #' @param write_data A logic value indicating if the function must 
    #'   write the output data.
    #'
    #' @return
    #' 
    #' * If return_data is TRUE, returns a list with tibbles
    #' * If write_date is TRUE, write several RData and csv files
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * This function requires the valid datasets. Run the
    #'   valid_data function before this one.
    #' * This function requires the auxiliary_functions.R list of 
    #'   functions
    #' * This function requires the match_functions.R list of 
    #'   functions
    #' * The output paths are preconfigured.
    #' 
    #' @noRd
    
    analisys_data = function(return_data = TRUE,
                             write_data = TRUE) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        require(stringi)
        
        # Check arguments --------------------
        
        for (i in c("return_data", "write_data")) {
            
            if (!(is.logical(get(i)))) {
                
                stop(paste(i, "value is not logical"))
                
            }
        }
        
        if (isFALSE(return_data) &&
            isFALSE(write_data)) {
            
            stop(paste("What you want to do champ?", 
                       "return_data and write_data can't both be FALSE!",
                       "Help me out here."))
            
        }
        
        
        # Set parameters --------------------
        
        source_files <- list(
            auxiliary_functions = "./scripts/auxiliary/auxiliary_functions.R",
            match_functions = paste0(".",
                                     "/scripts/analysis/",
                                     "match_functions.R"))
        
        valid_files <- list(
            bandeirante = paste0(".",
                                 "/data/valid/bandeirante/",
                                 "bandeirante.RData"),
            qualocep = paste0(".",
                              "/data/valid/qualocep/",
                              "qualocep.RData"))
        
        analysis_paths <- list(
            match = paste0(".",
                           "/data/analysis/match/"))
        
        # Source functions --------------------
        
        for (i in names(source_files)) {
            
            source(source_files[[i]], local = TRUE, encoding = "UTF-8")
            
        }
        
        # Load input data --------------------
        
        for (i in names(valid_files)) {
            
            assign(i, readRDS(valid_files[[i]]))
            
        }
        
        # Match bandeirantes names with street names --------------------
        
        message(paste0("\n",
                       "Matching bandeirantes names with qualocep street",
                       "names. ",
                       "This may take a while."))
        
        match_data <- list("bandeirante" = bandeirante, 
                           "qualocep" = qualocep)
        
        match <- match_functions$main(match_data)
        
        # Write data --------------------
        
        if (isTRUE(write_data)) {
            
            for (i in names(analysis_paths)) {
                
                if (!(str_extract(analysis_paths[[i]], ".$") == "/")) {
                    
                    analysis_paths[[i]] <- paste0(analysis_paths[[i]], "/")
                    
                }
                
                ## Prepare csv output
                
                unlist_ <- auxiliary_functions$unlist_
                
                output_csv <- get(i) %>% unlist_()
                
                ## Write data
                
                output_csv %>% 
                    write_delim(paste0(analysis_paths[[i]], i, ".csv"), 
                                delim = ",",
                                col_names = TRUE)
                
                get(i) %>% saveRDS(paste0(analysis_paths[[i]], i, ".RData"))   
                
            }
            
        }
        
        # Return output --------------------
        
        if (isTRUE(return_data)) {
            
            if (length(analysis_paths) == 1 & isTRUE(return_data)) {
                
                return(get(names(analysis_paths)))
                
            } else {
                
                output <- vector(mode = "list", length = 0)
                
                for (i in names(analysis_paths)) {
                    
                    output[[i]] <- get(i)
                    
                }
                
                return(output)
                
            }
            
        }
        
    },
    
    #' Read the analysis datasets
    #'
    #' @param dataset A character vector with the names of the datasets that 
    #'   the function must write/return. 
    #'   Note: use "all" to write/return all datasets.
    #'
    #' @return 
    #' 
    #' * If dataset is length 1, returns a tibble
    #' * If dataset is "all" or length > 1, returns a list with tibbles
    #' 
    #' @details
    #' 
    #' * Set the working directory to the project's root directory
    #' * The .RData file must already be created.
    #' 
    #' @noRd
    
    read_analysis_data = function(dataset = "all") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        
        # Set parameters --------------------
        
        analysis_files <- list(
            match = paste0(".",
                                 "/data/analysis/match/",
                                 "match.RData"))
        
        # Check arguments --------------------
        
        for (i in c("dataset")) {
            
            if (!(is.character(get(i)))) {
                
                stop(paste(i, "value is not character"))
                
            }
            
        }
        
        for (i in dataset) {
            
            if (!(i %in% c("all", names(analysis_files)))) {
                
                stop(paste(i, "value is not valid"))
                
            }
            
            if (!(i == "all")) {
                
                if (!(file.exists(analysis_files[[i]]))) {
                    
                    stop(paste(i, "file not found"))
                    
                }
                
            }
        }
        
        # Transform variables --------------------
        
        if ("all" %in% dataset) {
            
            dataset <- names(analysis_files)
            
        }
        
        # Load valid data --------------------
        
        for (i in dataset) {
            
            assign(i, readRDS(analysis_files[[i]]))
            
        }
        
        # Return output --------------------
        
        if (length(dataset) == 1) {
            
            return(get(dataset))
            
        } else {
            
            output <- vector(mode = "list", length = 0)
            
            for (i in dataset) {
                
                output[[i]] <- get(i)
                
            }
            
            return(output)
            
        }
        
    }
    
)