
<!-- README.md is generated from README.Rmd. Please edit that file -->

# GIFT <img src="man/figures/logo.png" align="right" alt="" width="200" />

<!-- badges: start -->

[![licence](https://img.shields.io/badge/Licence-GPL--3-blue.svg)](https://www.r-project.org/Licenses/GPL-3)
<!-- badges: end -->

This **R package** contains the function LBI to calculate the
Latitudinal Bias Index, as described in Sanczuk et al.

# :arrow_double_down: Installation

When available on CRAN, you can install and load it using the following
commands:

``` r
install.packages("latbias")
library("latbias")
```

To install the development version from the GitHub repository:

``` r
# install.packages("devtools")
remotes::install_github("https://github.com/pierredenelle/latbias",
dependencies = TRUE)
library("latbias")
```

# :bug: Find a bug?

Thank you for finding it. Head over to the GitHub Issues tab and let us
know about it. Alternatively, you can also send us an email. We will try
to get to it as soon as possible!

# References and dependencies

`latbias` depends on `dplyr`, `geosphere`, `reshape2`, `sf`, `sp`,
`stats`, `terra`, `tidyr`, `units` and `utils`.
