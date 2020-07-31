#' A list of functions to validate values of the input datasets
#'
#' @author Daniel Vartanian.
#'
#' @description
#' 
#' * Version: 1.0.0 2020-07-30
#' * Style guide: The tidyverse style guide (2019) 
#'   <https://style.tidyverse.org/>
#' 
#' @note
#' 
#' Help functions:
#' 
#' * setwd(normalizePath(readClipboard(), "/", mustWork = FALSE))
#' * assign("last.warning", NULL, envir = baseenv())
#' 
#' @seealso
#' 
#' * GitHub: <https://github.com/danielvartan/bandeirantes>
#' 
#' @noRd


validation_functions <- list (
    
    #' Validate main values of the input datasets
    #'
    #' @param data A tibble with the input dataset.
    #' 
    #' @return A tibble.
    #' 
    #' @details
    #' 
    #' * This function requires an internet connection
    #' 
    #' @noRd
    
    main = function(data) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(stringr)
        require(curl)
        require(jsonlite)

        # Check arguments --------------------
        
        if (!(is.data.frame(data))) {
            
            stop("data is not a data frame")
            
        }
        
        # Validate data --------------------
        
        output <- data
        
        ## Check if the state variable have invalid values
        ## See API documentation: 
        ## https://servicodados.ibge.gov.br/api/docs/localidades?versao=1
        
        if ("state" %in% names(output) &&
            is.character(output$state)) {
            
            ibge_states <- fromJSON(
                "https://servicodados.ibge.gov.br/api/v1/localidades/estados")
            output <- output %>% mutate(state = case_when(
                !(state %in% str_to_upper(ibge_states$nome)) ~ 
                    as.character(NA),
                TRUE ~ state))
            
        }
        
        ## Check if the city variable have invalid values
        ## See API documentation: 
        ## https://servicodados.ibge.gov.br/api/docs/localidades?versao=1
        
        if ("city" %in% names(output)) {
            
            ibge_cities <- fromJSON(
                "https://servicodados.ibge.gov.br/api/v1/localidades/municipios")
            output <- output %>% mutate(city = case_when(
                !(city %in% str_to_upper(ibge_cities$nome)) ~ 
                    as.character(NA),
                TRUE ~ city))
            
        }
        
        ## Check if the postal_code variable have invalid values
        
        if ("postal_code" %in% names(output)) {
            
            output <- output %>% mutate(
                postal_code = str_remove(postal_code, "-"))
            
            pattern <- "^([0-9]{8})$"
            output <- output %>% mutate(postal_code = case_when(
                !(str_detect(as.character(postal_code), pattern)) ~ 
                    as.character(NA),
                !(as.integer(postal_code) > 0) ~ as.character(NA),
                TRUE ~ postal_code))
            
        }
        
        # Return output --------------------
        
        output
        
    }
    
)