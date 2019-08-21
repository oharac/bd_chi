To do:

* [X] Create a function to take a CHI layer (Mollweide, ~ 1 km) and reproject/aggregate into spp CRS (Gall-Peters, ~ 10 km).
    * dataframe style: map cell IDs from G-P to Moll projection, then data.frame(cell_id, value)
    * crosstab style: map cell IDs into a raster, then crosstab? not great for continuous variables though?
    * how to aggregate stressor intensity values?  Mean? max/min/median?
* [X] Apply this to all CHI stressor layers (latest) 
* [ ] Calculate stressor trend layers (is this already calculated?)
    * calculate at CHI native res, or at SPP native res? check the math...
    * Figure out which stressors match IUCN threats (see below) to narrow down which layers to process
* [ ] "Quick" analysis a la Selig 2014:
  * Determine top X% of stressor intensity (or of cumulative impacts)
  * Determine top X% of biodiversity from spp risk layers (n_spp, n_spp_rr, risk, risk_rr, thr, thr_rr)
* [ ] Map stressors to IUCN spp threats by class and by narrative.
* [ ] For each spp:
    * compare range to proportion of range impacted by various stressors
    * is this weighted? e.g. weighted by threat score
    * becomes a map of relative stress from all sources (additive)
    * this is easily turned into a simple threat/refuge calculation but allows more flexibility for further comparisons
    
Scripts for CHI 2019: https://github.com/OHI-Science/impact_acceleration

Stressor files are on Mazu: M:/git-annex/impact_acceleration/stressors
