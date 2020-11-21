### Support functions for this project

### * IUCN API functions
### * Simple Features and Raster common functions
### * Other random helper functions


### setup common directories
dir_git     <- here()
dir_setup   <- here('_setup')
dir_data    <- here('_data')
dir_spatial <- here('_spatial')
dir_output  <- here('_output')
dir_bd_anx   <- '~/git-annex/bd_chi'

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
api_key <- Sys.getenv('IUCN_KEY')

# api_version_current <- jsonlite::fromJSON('http://apiv3.iucnredlist.org/api/v3/version') %>%
#   .$version
api_version <- '2020-1'
# if(api_version_current != api_version) {
#   message('IUCN API at ', api_version_current, ' but using ', api_version)
# } else {
#   message('IUCN API at ', api_version, '; most current version.')
# }

### Create table of species included in this assessment
### * Threatened or near threatened
### * Mapped
### * Comprehensively assessed taxa
### * threatened by one or more stressors (flag)

get_str_cats <- function() {
  x <- read_csv(here('_raw', 'iucn_threat_to_stressor_lookup.csv'),
                col_types = cols(.default = 'c')) %>%
    select(stressor, category) %>%
    filter(!is.na(stressor)) %>%
    mutate(stressor = str_split(stressor, ';')) %>%
    unnest(stressor) %>%
    distinct()
  
  return(x)
}

get_incl_spp <- function(api_V = api_version, include_all_risk = FALSE) {
  ### Load spp info for sensitivity, risk, comprehensive status, and maps.

  sens_file <- here('_output', sprintf('spp_sensitivity_%s.csv', api_V))
  risk_file <- here('_data', sprintf('iucn_risk_current_%s.csv', api_V))
  maps_file <- here('_data', sprintf('spp_marine_maps_%s.csv', api_V))
  comp_file <- here('_data', sprintf('iucn_comp_assessed_%s.csv', api_V))

  spp_maps <- read_csv(maps_file, col_types = cols('iucn_sid' = 'i',
                                                   'subpop'   = 'c')) %>%
    filter(presence != 5) %>%
    select(iucn_sid) %>% ### just need ID numbers here
    distinct()
  
  spp_risk <- read_csv(risk_file, col_types = cols('iucn_sid' = 'i')) %>%
    select(iucn_sid, sciname, comname, cat_score)
  
  if(!include_all_risk) {
    spp_risk <- spp_risk %>%
      filter(!is.na(cat_score) & !cat_score %in% c(0, 1))
  }
  
  spp_sens <- read_csv(sens_file, col_types = cols('iucn_sid' = 'i')) %>%
    filter(sens == TRUE) %>%
    select(-sens)
  
  spp_comp <- read_csv(comp_file, col_types = cols('iucn_sid' = 'i')) %>%
    select(-sciname)
  
  spp_incl <- spp_maps %>%
    inner_join(spp_risk, by = 'iucn_sid') %>%
      ### if include_all_risk == FALSE, drop LC, EX, DD
    left_join(spp_comp, by = c('iucn_sid')) %>%
    filter(!is.na(assess_gp)) %>%
      ### drop non-comp-assessed
    left_join(spp_sens, by = 'iucn_sid') %>%
      ### add stressor sensitivities; NA means not sensitive
    left_join(get_spp_taxon(), by = c('iucn_sid', 'assess_gp')) %>%
    mutate(taxon = tolower(rank)) %>%
      ### add taxonomic groupings
    distinct()
  
  # spp_incl$iucn_sid %>% unique() %>% length()
  # spp_incl %>% filter(!is.na(stressor)) %>% .$iucn_sid %>% unique() %>% length()
  ### 1273 species included here: mapped, threatened/NT, comp assessed.
  ### * 1006 of which are sensitive to one or more stressors
  return(spp_incl)
  
}

### a keyed datatable join for speed
dt_join <- function(df1, df2, by, type) {
  a <- case_when(type == 'left' ~ c(FALSE, TRUE, FALSE), ### all, all.x, all.y
                 type == 'full' ~ c(TRUE, TRUE, TRUE),
                 type == 'inner' ~ c(FALSE, FALSE, FALSE))
  
  ### if all = FFF, behaves like inner join; if all = TTT,
  ### behaves like full join; if all = FTF, behaves like left_join?
  dt1 <- data.table::data.table(df1, key = by)
  dt2 <- data.table::data.table(df2, key = by)
  dt_full <- merge(dt1, dt2, all = a[1], all.x = a[2], all.y = a[3])
  return(as.data.frame(dt_full))
}

map_to_rast <- function(map_df, cell_val) {
  # map_rast <- raster::subs(cell_id_rast, as.data.frame(map_df), 
  #                          by = 'cell_id', which = val)
  if(!exists('cell_id_rast', envir = .GlobalEnv)) {
    assign('cell_id_rast', 
           raster::raster(here('_spatial/cell_id_mol.tif')),
           envir = .GlobalEnv)
  }
  map_df <- map_df %>%
    select(cell_id, val = cell_val) %>%
    filter(!is.na(val))
  map_rast <- raster(cell_id_rast) ### creates NA raster
  cell_vec <- map_df$cell_id
  
  if(length(cell_vec) > 0) { 
    ### if any non-NA cells, replace values
    values(map_rast)[map_df$cell_id] <- map_df$val
  }
  return(map_rast)
}

rast_to_map <- function(rast, cell_val = NULL) {
  df <- data.frame(val = values(rast),
                   cell_id = 1:ncell(rast)) %>%
    filter(!is.na(val))
  if(!is.null(cell_val)) {
    names(df)[names(df) == 'val'] <- cell_val
  }
  return(df)
}

get_spp_range <- function(id = NULL) {
  ### read the range file created in setup script 5c.  If ids are
  ### supplied, filter to just those ids, otherwise return the whole
  ### dataframe.
  range_f <- here('_raw/range_from_rasts.csv')
  if(!file.exists(range_f)) {
    stop('No range file found.  Please run setup script 5c_determine_spp_range.Rmd')
  }
  ranges <- read_csv(range_f, col_types = c(iucn_sid = 'i', range_km2 = 'd'))
  if(!is.null(id)) {
    ranges <- ranges %>%
      filter(iucn_sid %in% id)
  }
  return(ranges)
}

get_spp_taxon <- function(id = NULL, all_cols = FALSE) {
  taxa_descs <- read_csv(here('_raw/taxa_descriptions.csv'),
                         col_types = cols(.default = 'c'))

  taxon_f <- here('_raw/spp_ranks.csv')
  if(!file.exists(taxon_f)) {
    stop('No taxa file found.  Please run setup script 5d_generate_taxon_list.Rmd')
  }
  taxa <- read_csv(taxon_f, col_types = c('iucn_sid' = 'i')) %>%
    left_join(taxa_descs, by = c('assess_gp', 'rank'))
  
  
  if(!is.null(id)) {
    taxa <- taxa %>%
      filter(iucn_sid %in% id)
  }
  if(!all_cols) {
    taxa <- taxa %>%
      select(iucn_sid, assess_gp, rank, desc)
  }
  return(taxa)
}

