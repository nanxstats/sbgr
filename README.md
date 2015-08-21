# R Client for Seven Bridges Genomics API

[![Build Status](https://travis-ci.org/road2stat/sbgr.png?branch=master)](https://travis-ci.org/road2stat/sbgr)

The `sbgr` package provides an R client for accessing the [Seven Bridges Genomics API](http://developer.sbgenomics.com/).

## Installation

To download and install the `sbgr` package from Bioconductor, type the following commands in R:

```
source("http://bioconductor.org/biocLite.R")
biocLite("sbgr")
```

It is possible that the package is not availble in the `release` branch right after being pushed to Bioconductor, you may switch to the `devel` branch to install it and switch back to the `release` branch (if you were using the `release` branch at first):

```
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sbgr")
useDevel(devel = FALSE)
```

Alternatively, you can install the cutting-edge development version of the package from GitHub:

```
# install.packages("devtools") if devtools was not installed
library("devtools")
install_github("road2stat/sbgr")
```

To load the package in R, simply use

```
library("sbgr")
```

and you are all set. See the [package vignette](http://www.bioconductor.org/packages/devel/bioc/vignettes/sbgr/inst/doc/sbgr.html) for a quickstart example.

<hr>

© Seven Bridges Genomics 2012 - 2015. Licensed under the MIT license.