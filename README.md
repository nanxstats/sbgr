# R Client for Seven Bridges Genomics API

[![Build Status](https://travis-ci.org/road2stat/sbgr.png?branch=master)](https://travis-ci.org/road2stat/sbgr)

The `sbgr` package provides an R client for accessing the [Seven Bridges Genomics API](http://developer.sbgenomics.com/).

## Installation

The package is currently not availble in the Bioconductor `release` branch yet until next Bioc release cycle, please switch to the `devel` branch by following code to install.

```
source("http://bioconductor.org/biocLite.R")
useDevel(devel = TRUE)
biocLite("sbgr")
```

Alternatively, you can install the latest development version of the package from GitHub too:

```
# install.packages("devtools") if devtools was not installed
library("devtools")
install_github("road2stat/sbgr")
```

To load the package in R, simply call

```
library("sbgr")
```

## Manual and Vignettes

Please check out the package [page](https://www.bioconductor.org/packages/devel/bioc/html/sbgr.html) for more information, and two tutorials

- [Easy Cascading API for end useres](https://www.bioconductor.org/packages/devel/bioc/vignettes/sbgr/inst/doc/easy_api.html)
- [Tutorial with functional API](http://www.bioconductor.org/packages/devel/bioc/vignettes/sbgr/inst/doc/sbgr.html) 



<hr>

© Seven Bridges Genomics 2012 - 2015. Licensed under the MIT license.