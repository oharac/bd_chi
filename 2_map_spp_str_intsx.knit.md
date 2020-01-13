---
title: 'Map species stressor intersections'
author: "*Compiled on Mon Jan 13 08:35:06 2020 by ohara*"
output: 
  html_document:
    code_folding: hide
    toc: true
    toc_depth: 3
    toc_float: yes
    number_sections: true
    theme: cerulean
    highlight: haddock
    includes: 
      in_header: '~/github/src/templates/ohara_hdr.html'
  pdf_document:
    toc: true
---


```r
library(raster)
source('https://raw.githubusercontent.com/oharac/src/master/R/common.R')

source(here('common_fxns.R'))

dir_str_95cv <- file.path(dir_bd_anx, 'layers/stressors_10km_95cv')
dir_spp_maps <- file.path(dir_bd_anx, 'spp_rasts_mol_2019')
```


# Summary

Here we will calculate, for each species, a map of impacted area for each of the stressors to which the species is sensitive; stressor ranges may change over time as well.  These maps will be saved as a .csv of `cell_id`, stressor value `str_val`, and `str_year`.  The stressor values can easily be normalized later if desired.

Maps (.csvs) will be saved for each species in a folder for each stressor.  For non-impacted species, a place-holder will be saved, perhaps with `cell_id = NA`.

The matrix method would work well for calculating cumulative impact, but we will skip that for now to enable more resolution at the species and stressor level.

## Methods summary

Briefly, the method will loop over each stressor and species; if the species has a non-zero sensitivity to the stressor, the overlap of species range and stressor range will be mapped.  If the species is not sensitive, it will still be logged with a placeholder, for process checking...

Note that the intersection cells include only those cells where the species AND stressor are both present in a given year for a given stressor.  Range cells outside the stressor area are dropped and are assumed to be unimpacted (refugia); stressor cells outside the range area are dropped and assumed to have no impact on this species.

# Methods

## Get the species sensitivities

The species sensitivity dataframe is created in script 1: `1_calc_spp_stressor_sensitivities.Rmd`.  Load this and use it to identify included species and stressors.



## Loop over stressors

Let's create a function to take a stressor name and use that to pull all the stressor layers (all years) into a raster stack.  Let's also create a function to turn the raster stack into a dataframe of cell IDs and filter to non-NA cells.



Using this function, loop through each of the included stressors, build the stack, convert to df for easier handling, and then loop over all species to create a map of cell ID, stressor value, and year for all stressor occurrences on the species range.





