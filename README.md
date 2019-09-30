# fire
## Flammability and vegetation models for NSC BioHeritage Project Farming and Nature Conservation   
30.09.2019

This repository contains all of the R code necessary for running the community flammbility models as they stand at September 2019. <br/>
Please note that these models are still under development and that current outputs should be treated with some caution.

Analyis and modelling should take place in the following order:

1. Run cover_summaries.Rmd to summarise raw cover abundance data and calculate species importance values per plot
2. Run community.Rmd to estimate species-level flammability values, fill in missing species data, analyse plant traits and flammability, and combine species' flammability scores with species' importance values per plot to calculate community weighted mean (flammability score) per plot. 
3. Run model_runs.Rmd to set up the initial conditions for landscape modelling and run replanting scenarios. The model code itself is stored in landscape_model.R and this should not be edited except with extreme care! 

Other files in this repository are:<br/>
fire.Rproj - the R project file<br/>
graphs.Rmd - script for creating graphs of preliminary model results<br/>
jen_ggbiplot.R - modified ggbiplot function for plotting PCA results, called by graphs.Rmd<br/>
traits_lookup.R - web scraping script to search for morphological traits of all species in a given list on the NZ ecotraits database https://ecotraits.landcareresearch.co.nz/
