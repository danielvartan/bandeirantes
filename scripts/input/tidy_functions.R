#' A list of functions for tidyng the raw datasets
#'
#' @author Daniel Vartanian.
#'
#' @description
#' 
#' * Version: 1.1.0 2020-08-16
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


tidy_functions <- list (
    
    #' Tidy the bandeirante raw dataset
    #'
    #' @param file A string with the csv file name for the
    #'  raw dataset.
    #' @param sep A string with the field separator in "file".
    #' @param dec A string with the decimal point in "file".
    #' 
    #' @return a tibble.
    #' 
    #' @noRd
    
    bandeirante = function(file,
                           sep = ",",
                           dec = ".") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        
        # Check arguments --------------------
        
        for (i in file) {
            
            if (!(file.exists(i))) {
                
                stop(paste(i, ", in file, do not exist"))
                
            }
            
        }
        
        if (!(sep %in% c(";", ",", "\t"))) {
            
            stop("sep is not a valid delimiter")
            
        }
        
        if (!(dec %in% c(".", ","))) {
            
            stop("dec is not a valid decimal marker")
            
        }
        
        # Load data --------------------
        
        data <- file %>% 
            read_delim(delim = sep, 
                       na = c("", " ", "NA"), 
                       col_types = cols(.default = "c"), 
                       locale = locale("en", 
                                       date_format = "%Y-%m-%d", 
                                       decimal_mark = dec), 
                       trim_ws = TRUE) %>% 
            as_tibble

        # Select data --------------------
        
        output <- data
        
        # Transform variables --------------------
        
        output <- output %>% 
            mutate_all(str_to_upper) %>% 
            mutate_all(str_squish)
        
        # Collapse values --------------------
        
        ## Transform all collapsed values to list
        
        output <- output %>%
            mutate_at("keyword", 
                      function(x) str_split(x, ", |; |\\. |,|;|\\."))
        
        # Tidy output --------------------
        
        output <- output %>%
            transmute(bandeirante_id = as.integer(bandeirante_id),
                      criteria = factor(criteria, 
                                        levels = c("BCE1",
                                                   "BCE2",
                                                   "BCE3",
                                                   "BCE4",
                                                   "BCI1"),
                                        ordered = FALSE),
                      document_frequency = as.integer(document_frequency),
                      instance_frequency = as.integer(instance_frequency),
                      name = as.character(name),
                      keyword = as.list(keyword))
        
        # Return output --------------------
        
        output
        
    },
    
    #' Tidy the match raw dataset
    #'
    #' @param file A string with the csv file name for the
    #'  raw dataset.
    #' @param sep A string with the field separator in "file".
    #' @param dec A string with the decimal point in "file".
    #' 
    #' @return a tibble.
    #' 
    #' @noRd
    
    match = function(file,
                     sep = ",",
                     dec = ".") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        
        # Check arguments --------------------
        
        for (i in file) {
            
            if (!(file.exists(i))) {
                
                stop(paste(i, ", in file, do not exist"))
                
            }
            
        }
        
        if (!(sep %in% c(";", ",", "\t"))) {
            
            stop("sep is not a valid delimiter")
            
        }
        
        if (!(dec %in% c(".", ","))) {
            
            stop("dec is not a valid decimal marker")
            
        }
        
        # Load data --------------------
        
        data <- file %>% 
            read_delim(delim = sep, 
                       na = c("", " ", "NA"), 
                       col_types = cols(.default = "c"), 
                       locale = locale("en", 
                                       date_format = "%Y-%m-%d", 
                                       decimal_mark = dec), 
                       trim_ws = TRUE) %>% 
            as_tibble
        
        # Select data --------------------
        
        output <- data
        
        # Transform variables --------------------
        
        output <- output %>% 
            mutate_all(str_to_upper) %>% 
            mutate_all(str_squish)
        
        # Tidy output --------------------
        
        output <- output %>%
            transmute(match_id = as.integer(match_id),
                      criteria = factor(criteria, 
                                        levels = c("LCE1",
                                                   "LCE2",
                                                   "LCE3",
                                                   "LCI1"),
                                        ordered = FALSE),
                      bandeirante_id = as.integer(bandeirante_id),
                      name = as.character(name),
                      street_type = as.character(street_type),
                      street_name = as.character(street_name),
                      street = as.character(street),
                      city = as.character(city),
                      uf = as.character(uf),
                      state = as.character(state))
        
        # Return output --------------------
        
        output
        
    },
    
    #' Tidy the qualocep raw dataset
    #'
    #' @param file A string with the csv file name for the
    #'  raw dataset.
    #' @param sep A string with the field separator in "file".
    #' @param dec A string with the decimal point in "file".
    #' 
    #' @return a tibble.
    #' 
    #' @noRd
    
    qualocep = function(file,
                        sep = "|",
                        dec = ".") {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(readr)
        require(stringr)
        
        # Check arguments --------------------
        
        for (i in file) {
            
            if (!(file.exists(i))) {
                
                stop(paste(i, ", in file, do not exist"))
                
            }
            
        }
        
        if (!(sep %in% c(";", ",", "\t", "|"))) {
            
            stop("sep is not a valid delimiter")
            
        }
        
        if (!(dec %in% c(".", ","))) {
            
            stop("dec is not a valid decimal marker")
            
        }
        
        # Load data --------------------
        
        ## Read data
        
        suppressWarnings(
            data <- file %>% 
                read_delim(delim = sep, 
                           na = c("", " ", "NA"), 
                           col_types = cols(.default = "c"), 
                           locale = locale("en", 
                                           date_format = "%Y-%m-%d", 
                                           decimal_mark = dec), 
                           trim_ws = TRUE) %>% 
                as_tibble
        )
        
        ## Correct encoding
        
        data <- data %>% 
            mutate_all(function(x) iconv(x, to="latin1")) %>%
            mutate_all(function(x) iconv(x, from="latin1", to="UTF-8"))

        # Transform variables --------------------
        
        output <- data %>% 
            mutate_all(str_to_upper) %>% 
            mutate_all(str_trim)
        
        # Tidy output --------------------
        
        suppressWarnings(
            
            output <- output %>%
                transmute(postal_code = as.character(CEP),
                       street_type = as.character(tipo_logradouro),
                       street_name = as.character(Logradouro),
                       complement = as.character(Complemento),
                       place = as.character(local),
                       neighborhood = as.character(bairro),
                       city = as.character(cidade),
                       city_code = as.numeric(cod_cidade),
                       uf = as.character(UF),
                       state = as.character(estado),
                       state_code = as.integer(cod_estado))
            
        )
        
        # Return output --------------------
        
        output
        
    }
    
)