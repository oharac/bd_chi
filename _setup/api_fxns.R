### Functions for accessing IUCN API

### `get_from_api()` is a function to access the IUCN API, given a url 
### (specific to data sought), parameter, and key (stored in ohi/git-annex), 
### as well as a delay if desired (to ease the load on the IUCN API server). 
### `mc_get_from_api()` runs the `get_from_api()` function across multiple 
### cores for speed...


library(parallel)
library(jsonlite)

### api_key stored on git-annex so outside users can use their own key
api_file <- file.path(dir_M, 'git-annex/globalprep/spp_ico', 
                      'api_key.csv')
api_key <- scan(api_file, what = 'character')

get_from_api <- function(url, param, api_key, delay, verbose = FALSE) {
  
  i <- 1; tries <- 5; success <- FALSE
  
  while(i <= tries & success == FALSE) {
    if(verbose) {
      message('try #', i)
    }
    ### be kind to the API server? later attempts wait longer.  Stick a random
    ### increment onto i to space out parallel pings
    index_rnd <- i + runif(n = 1, min = -.5, max = +.5)
    Sys.sleep(delay * index_rnd) 
    
    api_info <- fromJSON(sprintf(url, param, api_key)) 
    if (class(api_info) != 'try-error') {
      success <- TRUE
    } else {
      warning(sprintf('try #%s: class(api_info) = %s\n', i, class(api_info)))
    }
    if(verbose) {
      message('... successful? ', success)
    }
    i <- i + 1
  }
  
  if (class(api_info) == 'try-error') { ### multi tries and still try-error
    api_return <- data.frame(param_id  = param,
                             api_error = 'try-error after multiple attempts')
  } else if (class(api_info$result) != 'data.frame') { ### result isn't data frame for some reason
    api_return <- data.frame(param_id  = param,
                             api_error = paste('non data.frame output: ', 
                                               class(api_info$result), 
                                               ' length = ', length(api_info$result)))
  } else if (length(api_info$result) == 0) { ### result is empty
    api_return <- data.frame(param_id  = param,
                             api_error = 'zero length data.frame')
  } else {
    api_return <- api_info %>%
      data.frame(stringsAsFactors = FALSE)
  }
  
  return(api_return)
}

mc_get_from_api <- function(url, param_vec, api_key, 
                            cores = NULL, delay = 0.5, 
                            add_param_val = FALSE,
                            verbose = FALSE) {
  
  if(is.null(cores)) 
    numcores <- ifelse(Sys.info()[['nodename']] == 'mazu', 12, 1)
  else 
    numcores <- cores
  
  out_list <- parallel::mclapply(param_vec, 
                                 function(x) {
                                   get_from_api(url, x, api_key, delay, verbose)
                                 },
                                 mc.cores   = numcores,
                                 mc.cleanup = TRUE) 
  
  if(any(sapply(out_list, class) != 'data.frame')) {
    error_list <- out_list[sapply(out_list, class) != 'data.frame']
    if(verbose) {
      message('List items are not data frame: ', paste(sapply(error_list, class), collapse = '; '))
      message('might be causing the bind_rows() error; returning the raw list instead')
    }
    return(out_list)
  }
  
  if(add_param_val) {
    out_df <- out_list %>%
      setNames(param_vec) %>%
      bind_rows(.id = 'param_val')
  } else {
    out_df <- out_list %>%
      bind_rows()
  }
  out_df <- out_df %>%
    setNames(names(.) %>%
               str_replace('result.', ''))
  
  return(out_df)
}
