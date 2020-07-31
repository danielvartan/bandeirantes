#' A list of functions for matching analysis
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


match_functions <- list (
    
    #' Match bandeirantes names with qualocep street names
    #'
    #' @param data A list with the bandeirantes and qualocep datasets.
    #' 
    #' @return A tibble.
    #' 
    #' @noRd
    
    main = function(data) {
        
        # Load packages --------------------
        
        require(magrittr)
        require(dplyr)
        require(stringr)
        require(stringi)
        
        # Check arguments --------------------
        
        for (i in c("auxiliary_functions")) {
            
            if (!(i %in% ls(envir = parent.frame()))) {
                
                stop(paste(i, "is not loaded"))
                
            }
        }
        
        for (i in c("data")) {
            
            if (!(is.list(get(i)))) {
                
                stop(paste(i, "value is not list"))
                
            }
        }
        
        for (i in names(data)) {
            
            if (!(i %in% c("bandeirante", "qualocep"))) {
                
                stop(paste(i, "dataset is not in data"))
                
            }
            
        }
        
        for (i in data) {
            
            if (!(is.data.frame(i))) {
                
                stop(paste0(i, "is not a data frame"))
                
            }
            
        }
        
        rm(i)
        
        # Load functions --------------------
        
        unlist_ <- parent.frame()$auxiliary_functions$unlist_
        
        normalize <- function(x) stri_trans_general(str = x,
                                                    id = "Latin-ASCII")
        
        # Unlist data --------------------
        
        list2env(data, environment())
        
        # Aggregate names and variations --------------------
        
        bandeirante <- bandeirante %>% 
            unlist_ %>% 
            mutate(all_names = if_else(is.na(variations), 
                                       name, 
                                       paste0(name, ", ", variations))) %>%
            mutate(all_names = na_if(all_names, "NA"))
        
        # Compute analysis --------------------
        
        ## Load progress bar
        
        message(paste0("\n",
                       "Computing analysis. This may take a while."))
        
        pb <- progress_bar$new(
            format = "[:bar] :current/:total (:percent) (:eta) (:elapsedfull)",
            total = nrow(bandeirante), 
            clear = FALSE, 
            show_after = 0)
        
        pb$tick(0)
        
        ## Create template
        
        output <- qualocep %>% 
            filter(FALSE) %>%
            mutate(bandeirante_id = as.integer(),
                   name = as.character())
        
        ## Match values
        
        for (i in seq_len(nrow(bandeirante))) {
            
            all_names <- 
                str_trim(str_split(bandeirante$all_names[i], ",")[[1]])
            
            for (j in all_names) {
                
                ### "^test | test$| test |^test$"
                pattern <- paste0("^", normalize(j), " ", "|",
                                  " ", normalize(j), "$", "|",
                                  " ", normalize(j), " ", "|",
                                  "^", normalize(j), "$")
                
                match_id <- qualocep %>% 
                    filter(str_detect(normalize(street_name), pattern)) %>%
                    mutate(bandeirante_id = bandeirante$bandeirante_id[i],
                           name = all_names[1])

                if (!(nrow(match_id) == 0)) {
                    
                    output <- bind_rows(output, match_id)
                    
                }
                
                pb$tick()
                
            }
            
        }
        
        # Adjust output --------------------
        
        replacement <- list(
            c("^RAPOSO TAVARES KM$" , "RAPOSO TAVARES"),
            c("^RAPOSO TAVARES, S/N VIA KM22,140", "RAPOSO TAVARES"),
            c("^RAPOSO TAVARES, KM" , "RAPOSO TAVARES"),
            c("^04, S/Nº - ALTURA DA RODOVIA RAPOSO TAVARES KM$", 
              "RAPOSO TAVARES"),
            c("^RAPOSO TAVARES, S/Nº, KM$", 
              "RAPOSO TAVARES"),
            c("^BR-381 FERNÃO DIAS$", 
              "FERNÃO DIAS"),
            c("^FERNÃO DIAS - BR$", 
              "FERNÃO DIAS"),
            c("^FERNÃO DIAS KM$", 
              "FERNÃO DIAS"),
            c("^FERNÃO DIAS - KM$", 
              "FERNÃO DIAS"),
            c("^RODOVIA ANHANGUERA KM$", 
              "RODOVIA ANHANGUERA")
        )
        
        for (i in c("street_name", "street")) {
            
            for (j in replacement) {
                
                output <- output %>% mutate(!!as.symbol(i) := case_when(
                    str_detect(!!as.symbol(i), j[1]) ~ 
                        str_replace_all(!!as.symbol(i), j[1], j[2]),
                    TRUE ~ !!as.symbol(i)))
                
            }
            
        }
        
        remove <- c("DOUTOR", "PROFESSOR", "PREFEITO", "VEREADOR", 
                    "DEPUTADO", "PASTOR", "CONSTRUTOR", "DESEMBARGADOR",
                    "CHOFER", "POLÍCIA MILITAR")
        
        for (i in remove) {
            
            output <- output %>%
                filter(!(str_detect(street_name, i)))
            
        }
        
        
        ### variable "street_name"
        remove <- c("CARLOS RAPOSO TAVARES",
                    "PEDRO ÁLVARES CABRAL")
        
        for (i in remove) {
            
            output <- output %>%
                filter(!(str_detect(street_name, i)))
            
        }
        
        output <- output %>%
            distinct(street_type, street_name, city, .keep_all = TRUE) %>%
            mutate(street = paste(street_type, street_name),
                   match_id = as.integer(row_number())) %>%
            select(match_id, bandeirante_id, name, street_type, 
                   street_name, street, city, uf, state)
        
        # Return output --------------------
        
        output
        
    }
    
)