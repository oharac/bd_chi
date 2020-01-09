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
    cat_msg('Found invalid geometries')
    
    bbox_shp <- st_bbox(spp_shp)
    if(bbox_shp$xmin < -180 | bbox_shp$xmax > 180) {
      cat_msg('Bounding box outside +/- 180; buffering with dist = 0')
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
        cat_msg('Error: area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; not equal!')
        stop('Area_pre and area_post not equal!')
      } else {
        cat_msg('Area check good!  area_pre = ', round(sum(area_pre), 3), 
                '; area_post = ', round(sum(area_post), 3), 
                '; area_ratio = ', round(area_ratio, 5), '; all equal!')
      }
    } else {
      cat_msg('bbox not exceeded; no need to fix polygon with buffer')
    }
  }
  return(spp_shp)
}

drop_geom <- function(sf) {
  sf %>% as.data.frame() %>% select(-geometry)
}

calc_cv_thresh <- function(rast, pct = 0.95) {
  ### calculate threshold for value at or just above a given pct contour volume
  x <- values(rast)[!is.na(values(rast))] %>% sort(decreasing = TRUE)
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

map_contour_volume <- function(rast, pct = 0.95) {
  thresh <- calc_cv_thresh(rast, pct)
  rast_cv <- rast
  values(rast_cv)[values(rast_cv) < thresh] <- NA
  return(rast_cv)
}
