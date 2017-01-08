# R Client for Seven Bridges Genomics API

[![Build Status](https://travis-ci.org/road2stat/sbgr.svg?branch=master)](https://travis-ci.org/road2stat/sbgr)

**Note:** We recommend the users to use the [sevenbridges](https://bioconductor.org/packages/sevenbridges) package instead, since we have migrated from API v1 to API v2. This repository is mostly for archiving purpose.

The `sbgr` package provides an R client for accessing the Seven Bridges Genomics API v1.

## Installation

Install the latest development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("road2stat/sbgr")
```

To load the package in R, simply call

```r
library("sbgr")
```

## Manual and Vignettes

Please check out the package [page](https://www.bioconductor.org/packages/devel/bioc/html/sbgr.html) for more information, and two tutorials

- [Easy Cascading API for End Users](https://www.bioconductor.org/packages/devel/bioc/vignettes/sbgr/inst/doc/easy_api.html)
- [Tutorial with Functional API](https://www.bioconductor.org/packages/devel/bioc/vignettes/sbgr/inst/doc/sbgr.html)

<hr>

© 2016 Seven Bridges Genomics, Inc. Licensed under the Apache License 2.0.
