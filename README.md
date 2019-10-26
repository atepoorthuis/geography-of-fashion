
<!-- README.md is generated from README.Rmd. Please edit that file -->
geographyoffashion
==================

[![Binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/atepoorthuis/geography-of-fashion/master?urlpath=rstudio)

This repository contains the data and code for our paper:

> Poorthuis, A, D. Power and M. Zook, (2019). *Attentional Social Media: Mapping the Spaces and Networks of the Fashion Industry*. Annals of the American Association of Geographers <https://doi.org/10.1080/24694452.2019.1664887>

An interactive version of the maps related to this paper (including all keywords, instead of the limited number of figures in the paper) is available at: <https://geography-of-fashion.netlify.com>.

This repository contains all the data and code needed to reproduce the results and figures in our paper. Although we are not able to share the raw Twitter data publicly, the aggregated counts per hexagon for each fashion keyword, with which all the results can be reproduced, are found in `analysis/data/derived_data/`. The steps taken to produce this aggregated dataset can be found in:

-   [analysis/00a-hexagonal-grid-prep.Rmd](analysis/00a-hexagonal-grid-prep.md): Preparation of hexagonal global grid
-   [analysis/00b-twitter-data-prep.Rmd](analysis/00b-twitter-data-prep.md): Read in raw Twitter data and perform spatial join to hexagonal grid
-   [analysis/00c-twitter-data-group-totals.Rmd](analysis/00c-twitter-data-group-totals.md): Calculate odds ratios, diversity metrics and group totals for each hexagonal cell
-   [analysis/00d-twitter-data-city-level.Rmd](analysis/00d-twitter-data-city-level.md): Prepare city-level data

The steps needed to recreate the figures found in the paper can be found in:

-   [analysis/01-figures-global.Rmd](analysis/01-figures-global.md): Global figures
-   [analysis/02-figures-cities.Rmd](analysis/02-figures-cities.md): City-level figures

### How to download or install

You can download the compendium as a zip from from [this URL](https://github.com/atepoorthuis/geography-of-fashion/archive/master.zip).

Or you can install this compendium as an R package, geographyoffashion, from GitHub with:

``` r
# install.packages("devtools")
remotes::install_github("atepoorthuis/geography-of-fashion")
```

### Licenses

Text + figures and data: [CC-BY-4.0](http://creativecommons.org/licenses/by/4.0/)

Code: See the [DESCRIPTION](DESCRIPTION) file
