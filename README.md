# README

## At-risk marine biodiversity faces extensive, expanding, and intensifying human impacts

Repository for code and generated data for "At-risk marine biodiversity faces extensive, expanding, and intensifying human impacts" by Casey C. O'Hara, Melanie Frazier, Benjamin S. Halpern.

External data are freely available from:

* Species range maps: [IUCN Red List of Threatened Species spatial data download](https://www.iucnredlist.org/resources/spatial-data-download)
* Bird species range maps: [BirdLife International Data Zone](http://datazone.birdlife.org/species/requestdis)
* Species information: [IUCN Red List API](https://apiv3.iucnredlist.org/)
* Stressor distributions: [Recent pace of change in human impact on the world's ocean: Cumulative impacts. Knowledge Network for Biocomplexity](doi:10.5063/F12B8WBS)

To replicate the analysis, download necessary data and set up an external directory for these datasets and large files generated during the analysis process.

Run all scripts in the `_setup` directory in numeric order.  It will be necessary to point the scripts to the proper locations in your data directory.  The `common_fxns.R` script may be helpful for setting filename objects to various data locations.

Run all scripts in the root project directory, in numeric order.

Finally, if you wish to generate the figures, run the appropriate scripts in the `ms_figs` directory.

Overview of file structure:

* `_setup`: scripts for pre-processing of data on threats, species range distributions, and stressor distributions.
* `_raw`: lookup tables created by the project team to facilitate the analysis.
* `_data`: data drawn from the IUCN API; note that the setup scripts also place larger IUCN files on the external data directory.
* `_spatial`: spatial data and maps generated in the setup scripts.  Again, note that the setup scripts place larger spatial files on the external data directory.
* `_output`: all finalized datasets are stored here, including rasters of impacts, intensification, stressor footprints, and taxonomic species richness maps.
* `figs`: temporary figure storage
* `ms_figs`: code to generate figures for manuscript
* `ms_tables`: code to generate tables for manuscript



