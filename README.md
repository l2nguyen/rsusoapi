# susoapir
  <!-- badges: start -->
  [![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
  <!-- badges: end -->
  
A R package with wrapper functions to facilitate the use of the Survey Solutions API.

## Prerequisites
These programs uses R to interact with the Survey Solutions API so you will need to install R and R Studio on your computer.

* [R](https://cran.rstudio.com/)
* [RStudio](https://www.rstudio.com/products/rstudio/download/)

You will also need to make an API user account on your survey server. Learn how to make an API user account on the [support page](https://support.mysurvey.solutions/headquarters/api/survey-solutions-api/).

## Installation
To install this package, run the following code:
``` r
if (!requireNamespace("devtools")){install.packages("devtools")}
devtools::install_github("l2nguyen/susoapir")
```

Unfortunately, this package can only currently be installed from GitHub and not CRAN. We hope to submit this package to CRAN in the future.

## Issues or suggestions
If you encounter any issues, please report it in the [issues tab](https://github.com/l2nguyen/susoapir/issues). Suggestions for features additions are also appreciated and can also be added in the Issues tab.

Contributions are highly encouraged. If you would like to add some fixes or features to the package, please submit a pull request.

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By participating in this project you agree to abide by its terms.
