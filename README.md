To do:

[ ] Update species list and maps for IUCN API version 2019-3 or maybe 2020-1
[ ] Update threat listings based on narrative text and habitat info
    [ ] Critical for fisheries! differentiate pel_hb, pel_lb, dem_dest, dem_nondest_hb, dem_nondest_lb
    [ ] Critical for climate stressors - OA, SST, UV, SLR
[ ] Update species-stressor intersections based on updated species and threats
* From intersections, calculate impacts for all spp
    [ ] Impacted range, and pct range, per threat
    [ ] Impacted range, and pct range, cumulative over all threats
    [ ] Refugia range as (whole range - impacted range)
    [ ] Account for ocean cell area in calculations!
* From intersections, map cumulative impacts on threatened and near-threatened spp
    [ ] per cell: number of species impacted; proportion of species impacted
    [ ] per cell: number of species in refugia; proportion of species in refugia
* Plot ideas:
    [ ] Allan et al 2019, Fig. 3 (# of impacted spp)
    [ ] Allan et al 2019, Fig. 4 (# of unimpacted spp) 
    [ ] Allan et al 2019, Fig. 5 (% of impacted spp)
    [ ] compare range size vs. % area impacted (compare to protection targets differentiated by range size)
    [ ] compare threat status vs. % area impacted a la Allan et al. Fig. 2
    [ ] compare important threats by taxonomic group?
    
Scripts for CHI 2019: https://github.com/OHI-Science/impact_acceleration

Stressor files are on Mazu: M:/git-annex/impact_acceleration/stressors
