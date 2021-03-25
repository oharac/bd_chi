### interesting spp
source('https://raw.githubusercontent.com/oharac/src/master/R/common.R')

source(here::here('common_fxns.R'))
library(raster)
# cellid_mol <- raster('_spatial/cell_id_mol.tif')
# ocean_mol <- raster('_spatial/ocean_area_mol.tif')
# values(cellid_mol)[is.na(values(ocean_mol))] <- NA
# blank_latlong <- raster(ext = extent(c(-180, 180, -90, 90)), crs = '+init=epsg:4326', res = .1)
# cellid_latlong <- projectRaster(cellid_mol, blank_latlong, method = 'ngb')
# writeRaster(cellid_latlong, '_spatial/cellid_latlong.tif', overwrite = TRUE)
cellid_latlong <- raster('_spatial/cellid_latlong.tif')

ca_cellid_latlong <- crop(cellid_latlong, extent(c(-130, -115, 30, 42)))
ca_cells <- values(ca_cellid_latlong) %>% .[!is.na(.)]




### A few identified species
spp_of_note <- c(46967817, 46967863, 8005, 39385, 19488,	
                 17028,	4162,	8099,	22697742, 
                 21858 , 21860, 170341, 193734, 10088)
imps <- get_incl_spp() %>%
  filter(iucn_sid %in% spp_of_note) %>%
  select(iucn_sid, sciname, comname, cat_score) %>% #, assess_gp, desc, taxon) %>%
  distinct() %>% 
  left_join(read_csv('_output/imp_range_by_spp_2013.csv'), by = 'iucn_sid') %>%
  filter(impact_km2 > 0) %>%
  mutate(imp_pct = round(impact_km2 / range_km2 * 100, 3),
         inc2_pct = round(incr2_km2 / range_km2 * 100, 3),
         inc3_pct = round(incr3_km2 / range_km2 * 100, 3),
         dec2_pct = round(decr2_km2 / range_km2 * 100, 3),
         dec3_pct = round(decr3_km2 / range_km2 * 100, 3)) %>%
  select(-ends_with('km2'), range_km2) %>%
  distinct()
