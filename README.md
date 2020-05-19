To do:

* [X] Update species list and maps for IUCN API version 2020-1
    * [X] Add a marine birds filter into the habitats script early on.
* [X] Recategorize stressors into land-based, fishing, climate, and other
    * [X] fix threat-to-stressor lookup categories
        * Fishing: 
            * commercial demersal destructive
            * commercial demersal nondestructive high bycatch
            * commercial demersal nondestructive low bycatch
            * pelagic high bycatch
            * pelagic low bycatch
            * artisanal
        * Climate change: sea surface temperature, ocean acidification, sea level rise
        * Ocean: shipping
        * Land-based: nutrient pollution, organic chemical pollution, direct human, light
    * [X] recalc cumulative stressors by categories
* [X] Check stressor maps for proper calculation of 95% contour volume
    * [X] Clip all stressor year ranges to 2002-2013
    * [X] Ocean acidification in particular!
    * [X] While you're at it, crop the SLR raster to just coastal areas!
    * [X] Check the shipping - horizontal routes are artifacts? nope, they're in there
* [X] Clip out arctic (sea ice) areas for SST due to satellite effects
    * this is done in `_setup/6_aggregate_stressors_to_10km.Rmd`
* [X] Update threat listings based on narrative text and habitat info
    * [X] Critical for fisheries! differentiate pel_hb, pel_lb, dem_dest, dem_nondest_hb, dem_nondest_lb
    * [X] Critical for climate stressors - OA, SST, SLR (UV not included)
* [x] Update species-stressor intersections based on updated species and threats
    * [x] Use just cell ID, not stressor value, for more nimble read/write
* [X] From intersections, calculate impact summaries for all spp
    * [X] Impact footprint per threat
    * [X] Impact footprint, cumulative over all threats (for 1+, 2+, and more than 2 stressors cooccurring)
    * [ ] Intensifying impact footprint over range
    * [X] Account for ocean cell area in calculations!
* From intersections, map cumulative impacts on threatened and near-threatened spp
    * [X] per cell: number of species impacted; proportion of species impacted
    * [X] per cell: number of species in refugia; proportion of species in refugia
* [ ] Check (in `plot_nspp_str_intsx.Rmd`) certain birds have greater marine ranges than total ranges!
* Plot ideas:
    * [ ] Allan et al 2019, Fig. 3 (# of impacted spp)
    * [ ] Allan et al 2019, Fig. 4 (# of unimpacted spp) 
    * [ ] Allan et al 2019, Fig. 5 (% of impacted spp)
    * [ ] compare range size vs. % area impacted (compare to protection targets differentiated by range size)
    * [ ] compare threat status vs. % area impacted a la Allan et al. Fig. 2
    * [ ] compare important threats by taxonomic group?
* [ ] Look into Jenkins methods and refs:
    * [ ] Woods et al 2008
    * [ ] Jetz e al 2008, Loiselle et al 2003 (and others from IUCN/AM)
    * [ ] Turtles - Halpin 2009, Kot 2015
    * [ ] See how they assessed "marine impact per species" - "we calculated from Halpern et al (2015a) the mean value that occurred within the species range" - the mean value of total impact? any reference to sensitivity/spp-specific impacts?
    * [ ] they include threatened, DD (can I do this? not to find specific impacts), and small ranges - how is small range defined?
    * [ ] Priority score: proportion of spp range unprotected divided by ln(range area):
        * increases as range size decreases "in accordance with the well-established relationship between range area and extinction risk" (Manne and Pimm 2001; Manne et al 1999; Purvis et al 2000)
        * for each cell, id which species occur, and sum their priority scores
* [ ] Some sort of visualization/chart of how many species in each taxa and/or range size class are impacted by various stressors 
    * [ ] e.g. small scale spp threatened by cc vs large scale threatened by fishing

Scripts for CHI 2019: https://github.com/OHI-Science/impact_acceleration

Stressor files are on Mazu: M:/git-annex/impact_acceleration/stressors
