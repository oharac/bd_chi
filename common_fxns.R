### Support functions for this project

knitr::opts_chunk$set(fig.width = 6, fig.height = 4, fig.path = 'figs/',
                      echo = FALSE, message = FALSE, warning = FALSE)

### * IUCN API functions
### * Simple Features and Raster common functions
### * Other random helper functions


### setup common directories
dir_git     <- here()
dir_setup   <- file.path(dir_git, '_setup')
dir_data    <- file.path(dir_git, '_data')
dir_spatial <- file.path(dir_git, '_spatial')
dir_output  <- file.path(dir_git, '_output')
dir_bd_anx   <- file.path(dir_O, 'git-annex/bd_chi')


### rewrite message: cat if not knitting; message if knitting

message <- function(x, ...) {
  if(is.null(knitr:::.knitEnv$input.dir)) {
    ### not in knitr environment, so use cat()
    base::cat(x, ..., '\n')
  } else {
    ### in knitr env, so use message()
    base::message(x, ...)
  }
  return(invisible(NULL))
}

### Get API version

### api_key stored on git-annex so outside users can use their own key
api_file <- file.path(dir_M, 'git-annex/globalprep/spp_ico', 
                      'api_key.csv')
api_key <- scan(api_file, what = 'character')

api_version_current <- jsonlite::fromJSON('http://apiv3.iucnredlist.org/api/v3/version') %>%
  .$version
api_version <- '2020-1'
if(api_version_current != api_version) {
  message('IUCN API at ', api_version_current, ' but using ', api_version)
} else {
  message('IUCN API at ', api_version, '; most current version.')
}

