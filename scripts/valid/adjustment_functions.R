#' A list of functions to adjust values of the input datasets
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


adjustment_functions <- list(
    
    #' Adjust values of the bandeirante input dataset
    #'
    #' @param data A tibble with the input dataset.
    #' 
    #' @return A tibble.
    #' 
    #' @noRd
    
    bandeirante = function(data) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)

        # Check values --------------------
        
        for (i in c("data")) {
            
            if (!(is.data.frame(get(i)))) {
                
                stop(paste(i, "is not a data frame"))
                
            }
            
        }
        
        # Adjust values --------------------
        
        output <- data
        
        # Fine adjustments --------------------
        
        # Reorder variables --------------------
        
        # Return output --------------------
        
        output
        
    },
    
    #' Adjust values of the qualocep input dataset
    #'
    #' @param data A tibble with the input dataset.
    #' 
    #' @return A tibble.
    #' 
    #' @noRd
    
    qualocep = function(data) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(stringr)
        require(progress)
        
        # Check values --------------------
        
        for (i in c("data")) {
            
            if (!(is.data.frame(get(i)))) {
                
                stop(paste(i, "is not a data frame"))
                
            }
            
        }
        
        # Adjust values --------------------
        
        output <- data
        
        ## Adjust misplace street type values in street_type and 
        ## street_name variables
        
        message(paste0("\n",
                       "Adjusting misplace street type values in ",
                       "street_type and street_name variables"))
        
        pb <- progress_bar$new(
            format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
            total = length(unique(output$street_type)), 
            clear = FALSE, 
            show_after = 0)
        
        pb$tick(0)
        
        for (i in unique(output$street_type)) {
            
            pattern <- paste0("^", i, " ")
            
            output <- output %>% 
                mutate(dummy = is.na(street_type) & 
                           str_detect(street_name, pattern))
            
            output <- output %>% 
                mutate(street_type = if_else(dummy, 
                                             i, 
                                             street_type),
                       street_name = if_else(dummy,
                                             str_trim(str_remove(
                                                 street_name, pattern)),
                                             street_name))
            
            pb$tick()
            
        }
        
        ## Adjust abbreviations of the street_name variable
        
        values <- list(c("^AV ", "AVENIDA "), c("^AV[.,]", "AVENIDA"),
                       c("^R ", "RUA "), c("^R[.,]", "RUA"),
                       c("^AL ", "ALAMEDA "), c("^AL[.,]", "ALAMEDA"),
                       c("^PC ", "PRAÇA "), c("^PC[.,]", "PRAÇA"),
                       c("^DR ", "DOUTOR "), c(" DR ", " DOUTOR "), 
                       c("DR[.,]", "DOUTOR"),
                       c("^PROF ", "PROFESSOR "), 
                       c(" PROF ", " PROFESSOR "), c("PROF[.,]", "PROFESSOR"),
                       c("^ENG ", "ENGENHEIRO "), 
                       c(" ENG ", " ENGENHEIRO "), c("ENG[.,]", "ENGENHEIRO"),
                       c("^PE ", "PADRE "), 
                       c(" PE ", " PADRE "), c("PE[.,]", "PADRE"),
                       c("^PRES ", "PRESIDENTE "),
                       c(" PRES ", " PRESIDENTE "), c("PRES[.,]", "PRESIDENTE"),
                       c(" JR", " JÚNIOR"), c("JR[.,]", "JÚNIOR"))
        
        message(paste("\n",
                      "Adjusting abbreviations of the street_name variable"))
        
        pb <- progress_bar$new(
            format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
            total = length(values), 
            clear = FALSE, 
            show_after = 0)
        
        pb$tick(0)
        
        for (i in values) {
            
            output <- output %>% mutate(street_name = case_when(
                str_detect(street_name, i[1]) ~ 
                    str_replace_all(street_name, i[1], i[2]),
                TRUE ~ street_name))
            
            pb$tick()
            
        }
        
        ## Adjust misplace complement values for the street_name and 
        ## complement variables
        
        pattern <- c(" [0-9]+.*$",
                     " DE [0-9]+.*$", 
                     " ATÉ [0-9]+.*$",
                     " S/N$",
                     " S/Nº$")
        
        message(paste("\n",
                      "Adjusting misplace complement values for the ",
                      "street_name and complement variables"))
        
        pb <- progress_bar$new(
            format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
            total = length(pattern), 
            clear = FALSE, 
            show_after = 0)
        
        pb$tick(0)
        
        for (i in pattern) {
            
            output <- output %>% 
                mutate(dummy = str_detect(street_name, i))
            
            output <- output %>% 
                mutate(complement = if_else(dummy, 
                                            str_trim(str_extract(
                                                street_name, i)), 
                                            complement),
                       street_name = if_else(dummy,
                                             str_trim(str_remove(
                                                 street_name, i)),
                                             street_name))
            
            pb$tick()
            
        }
        
        pattern <- c("[.,;] $", "[.,;]$")
        
        for (i in pattern) {
            
            output <- output %>% 
                mutate(street_name = str_trim(str_remove(street_name, i)))
            
        }
        
        # Collapse variables --------------------
        
        ## Collapse street_type and street_name to a variable call 
        ## street
        
        output <- output %>%
            mutate(street = paste(street_type, street_name))
        
        # Reorder variables --------------------
        
        output <- output %>% 
            select(postal_code:street_name, street, complement:state_code)
        
        # Return output --------------------
        
        output
        
    }
    
)