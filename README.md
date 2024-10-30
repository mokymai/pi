Paketas ***pi***
================

<!-- README.md is generated from README.Rmd. Please edit that file -->
<!-- # Package **_bio_** -->
<!-- badges: start -->

[![GitHub
version](https://img.shields.io/badge/GitHub-0.0.1-brightgreen.svg)](https://github.com/mokymai/pi)
[![R-CMD-check](https://github.com/mokymai/pi/workflows/R-CMD-check/badge.svg)](https://github.com/mokymai/pi/actions)
[![Updated-on](https://img.shields.io/badge/Updated%20on-2024--10--30-yellowgreen.svg)](/commits/master)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![R-CMD-check](https://github.com/mokymai/pi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mokymai/pi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

<!-- [![GitHub last commit](https://img.shields.io/github/last-commit/mokymai/pi)](https://github.com/mokymai/pi) -->

Patogumo funkcijos pasikliautiniesiems intervalams (PI) skaičiuoti.

Apie klaidas, stringtis bei pageidaujamus tobulinimus praneškite
svetainėje <https://github.com/mokymai/pi/issues>. Nurodykite jūsų
naudojamą operacinę sistemą.

# Diegimas

Diegimas iš saugyklos, panašios į CRAN (rekomenduojama):

``` r
repos <- c("https://mokymai.github.io/download/", getOption("repos"))
install.packages("pi", repos = repos)
```

<details>
<summary>
Diegti iš „GitHub“
</summary>

Diegti iš „GitHub“:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("mokymai/pi", dependencies = TRUE)
```

Pastaba! Norint įsidiegti iš „*GitHub*“ sistemai „*Windows*“ reikia
įrankių „*RTools*“.

</details>

# Pavyzdžiai

``` r
pi::ci_binom(x = 20, n = 101)
```

Arba:

``` r
library(pi)
ci_binom(x = 20, n = 101)
```
