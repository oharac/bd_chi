### Support functions for this project

### Simple Features functions

clip_to_globe <- function(x) {
  ### for SF features, transform to wgs84, clip to +-180 and +-90
  epsg <- st_crs(x)$epsg
  if(epsg != 4326 | is.na(epsg)) {
    message('Original EPSG = ', epsg, '; Proj4 = ', st_crs(x)$proj4string,
            '\n...converting to EPSG:4326 WGS84 for clipping')
    x <- st_transform(x, 4326)
  }
  x_bbox <- st_bbox(x)
  if(x_bbox$xmin < -180 | x_bbox$xmax > +180 |
     x_bbox$ymin <  -90 | x_bbox$ymax >  +90) {
    message('Some bounds outside +-180 and +-90 - clipping')
    z <- st_crop(x, y = c('xmin' = -180,
                          'ymin' =  -90,
                          'xmax' = +180,
                          'ymax' =  +90)) %>%
      st_cast('MULTIPOLYGON')
    # z <- as(x, 'Spatial') %>%
    #   raster::crop(raster::extent(-180, 180, -90, 90)) %>%
    #   st_as_sf()
    ### otherwise is sfc_GEOMETRY which doesn't play well with fasterize.
    ### The st_crop solution works great most of the time; but for
    ### spp 21132910 (at least) the crop turned things into linestrings
    ### that couldn't be converted with st_cast('MULTIPOLYGON').
    message('Smoothing to half degree max')
    z <- smoothr::densify(z, max_distance = 0.5)
  } else {
    message('All bounds OK, no clipping necessary')
    z <- x
  }
  return(z)
}

valid_check <- function(spp_shp) {
  valid <- st_is_valid(spp_shp)
  ### can return a vector if multiple polygons with same ID
  if(any(!valid)) {
    message('Found invalid geometries')
    
    bbox_shp <- st_bbox(spp_shp)
    if(bbox_shp$xmin < -180 | bbox_shp$xmax > 180) {
      message('Bounding box outside +/- 180; buffering with dist = 0')
      ### check area before and after buffering to make sure no loss
      area_pre  <- st_area(spp_shp) %>% as.numeric() / 1e6
      spp_shp   <- st_buffer(spp_shp, dist = 0) %>%
        st_cast('MULTIPOLYGON')
      area_post <- st_area(spp_shp) %>% as.numeric() / 1e6
      
      area_check <- all.equal(area_pre, area_post)
      area_ratio <- max(sum(area_pre) / sum(area_post),
                        sum(area_post) / sum(area_pre))
      
      
      ### error check to make sure the buffer didn't lose polygons
      if(area_check == FALSE | 
         (class(area_check) != 'logical' & area_ratio > 1.001)) {
        ### use all.equal() for near equality, and for comparing all 
        ### elements in case of a vector.  If a difference, choose an arbitrary
        ### threshold for "close enough".
        message('Error: area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; not equal!')
        stop('Area_pre and area_post not equal!')
      } else {
        message('Area check good!  area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; all equal!')
      }
    } else {
      message('bbox not exceeded; no need to fix polygon with buffer')
    }
  }
  return(spp_shp)
}

fix_fieldnames <- function(poly_sf) {
  if(!'sciname' %in% names(poly_sf)) {
    poly_sf <- poly_sf %>%
      rename(sciname = binomial)
  }
  
  if(!'subpop' %in% names(poly_sf)) {
    poly_sf$subpop <- NA_character_
    ### if shape doesn't have subpop column, add it as NA
  }
  if('id_no' %in% names(poly_sf)) {
    ### if id_no field, reset to iucn_sid
    poly_sf <- poly_sf %>%
      rename(iucn_sid = id_no)
  }
  if(!'presence' %in% names(poly_sf)) {
    poly_sf <- poly_sf %>%
      mutate(presence = 1)
  }
  return(poly_sf)
}

fix_turtle_polys <- function(shp) {
  message('Replacing REPTILES with buffered turtle polygons!!!')
  ### the turtle polys have issues with the western boundary - not quite at
  ### +180; some have issues on the east as well. Buffer problem spp by
  ### 0.25 degrees before clipping.  Near the equator this adds an error of
  ### ~ 28 km!  But clearly the shitty polygons are creating an error as well.
  ### Identify the problem ones:
  ### Caretta caretta 3897;          Dermochelys coriacea 6494; 
  ### Eretmochelys imbricata 8005;   Chelonia mydas 4615
  ### I'll just buffer all subpops, for ease.  Land gets clipped later.
  
  turtles_buffered_file <- file.path(dir_bd_anx, 'tmp/reptiles_shp_buffered.shp')
  
  if(!file.exists(turtles_buffered_file)) {
    message('Creating a temporary buffered reptiles shapefile...')
    polys_rept <- read_sf(shp, type = 6) %>%
      janitor::clean_names() %>%
      fix_fieldnames()
    
    polys_rept_buff <- polys_rept %>%
      filter(iucn_sid %in% c(3897, 6494, 8005, 4615)) %>%
      st_buffer(dist = 0.25)
    polys_rept_non_buff <- polys_rept %>%
      filter(!iucn_sid %in% c(3897, 6494, 8005, 4615))
    polys_rept_fixed <- rbind(polys_rept_non_buff, polys_rept_buff)
    st_write(polys_rept_fixed, turtles_buffered_file, delete_layer = TRUE)
  }
  
  ### now read in as polys_all the fixed buffered file
  polys_all <- st_read(turtles_buffered_file)
  return(polys_all)
}

match_to_map <- function(poly_sf, maps_df) {
  ### Fix the shapefile IUCN id vs. subpop IUCN id.  The inner_join
  ### keeps only the polygon features of species still to be
  ### rasterized (from the id_fix dataframe).
  id_fix <- maps_df %>%
    filter(shp_file == shp) %>%
    select(shp_iucn_sid, iucn_sid, subpop, max_depth) %>%
    distinct()
  
  polys_match <- poly_sf %>%
    select(shp_iucn_sid = iucn_sid, sciname, subpop, presence, geometry) %>%
    mutate(presence = ifelse(presence == 0, 1, presence),
           subpop   = as.character(subpop)) %>%
    inner_join(id_fix, by = c('shp_iucn_sid', 'subpop')) 
  
  return(polys_match)
}

buffer_tiny_polys <- function(spp_shp) {
  ### Check that CRS is in meters.
  poly_crs <- st_crs(spp_shp)[1] %>%
    as.character()
  if(!str_detect(poly_crs, '\\+units=m')) {
    stop('For buffer_tiny_polys(), expecting units in meters')
  }
  
  ### Identify small polygons by their total area.  Most tiny zero-range
  ### spp have an area less than 100 km^2; one (Conus decoratus)
  ### is 124 km^2.  Use a little more than this as the size threshold.
  thresh_m2 <- units::set_units(130 * 1e6, m^2)
  ### Cell resolution is 10279.3 meters.  A buffer of ~half
  ### that would nearly guarantee all polygons to result in at least
  ### one raster cell, unless the polygon is near a corner.
  poly_area_m2 <- st_area(spp_shp$geometry)
  buffer_dist <- 5000 ### in meters
  
  
  if(poly_area_m2 < thresh_m2) {
    msg_stem <- 'Tiny polygon (%s km^2 < %s km2 threshold); buffering by %s km...'
    message(sprintf(msg_stem, round(poly_area_m2/1e6), thresh_m2/1e6, buffer_dist/1e3))
    spp_shp_buffered <- spp_shp %>%
      st_buffer(dist = buffer_dist)
    return(spp_shp_buffered)
  } else {
    return(spp_shp)
  }
}

clip_to_depth <- function(spp_rast, spp_shp) {
  ### depth clip if necessary; otherwise clip to bathy raster (which previously
  ### was clipped to area raster - so cells with any marine area will be kept,
  ### and non-marine cells will be dropped).
  ### Manual adds of shallow spp:
  spp_shallow <- c(133512)
  
  max_depth <- unique(spp_shp$max_depth)
  
  if(length(max_depth) != 1) stop('Non-unique max_depth field!')
  ### this shouldn't happen - each spp should have only one depth
  
  if(max_depth == '< 20 m') {
    ### intertidal, very shallow spp
    spp_rast <- mask(spp_rast, rast_shallow)
  } else if(max_depth == '< 200 m' | spp_shp$iucn_sid %in% spp_shallow) {
    ### spp on the continental shelf
    spp_rast <- mask(spp_rast, rast_neritic)
  } else {
    ### rast_bathy covers the entire ocean - effectively masks out land
    spp_rast <- mask(spp_rast, rast_bathy)
  }
  
  return(spp_rast)
}

drop_geom <- function(sf) {
  sf %>% as.data.frame() %>% select(-geometry)
}

### Raster functions

aggregate_to_cellid <- function(rast, cell_id_mol) {
  ### aggregates a CHI raster (Mollweide 1 km) to SPP CRS (GP 10 km)
  ### returns as a dataframe, which can then be used to do raster::subs()
  df <- data.frame(val      = values(rast),
                   cell_id  = values(cell_id_mol)) %>%
    filter(!is.na(cell_id)) %>%
    group_by(cell_id) %>%
    summarize(n_cells = sum(!is.na(val)),
              mean_val = mean(val, na.rm = TRUE)) %>%
    ungroup()
  
  return(df)
}

calc_cv_thresh <- function(rast, pct = 0.95) {
  ### calculate threshold for value at or just above a given pct contour volume
  ### rast can be a single raster or a raster stack.
  
  y <- values(rast) ### if rast is actually a stack, this returns a matrix
  ### but if y is a matrix, this coerces it to a vector so all is good
  x <- y[!is.na(y)] %>% sort(decreasing = TRUE)
  
  df <- data.frame(x) %>%
    mutate(cum_x = cumsum(x),
           cum_pct = cum_x / last(cum_x)) %>%
    filter(cum_pct >= pct)
  thresh <- df$x[1]
  x_thresh <- x[x == thresh]
  p_x <- length(x_thresh) / length(x)
  p_x_val <- sum(x_thresh) / sum(x)
  if(length(x_thresh) > 1) {
    warning('Threshold value ', thresh, ' appears ', length(x_thresh), ' times;',
            '\nthis is ', round(p_x * 100, 3), '% of the cells and ',
            round(p_x_val * 100, 3), '% of the cumulative values!')
  }
  return(thresh)
}

map_contour_volume <- function(rast, pct = 0.95, thresh = NULL) {
  ### if thresh is not provided, calculate in function.  If
  ### it is provided, clip the raster (or stack) to the
  ### provided threshold value or greater.
  if(is.null(thresh)) {
    thresh <- calc_cv_thresh(rast, pct)
  }
  rast_cv <- rast
  values(rast_cv)[values(rast_cv) < thresh] <- NA
  return(rast_cv)
}