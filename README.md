
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Build
Status](https://travis-ci.org/ncordon/imbalance.svg?branch=master)](https://travis-ci.org/ncordon/smartdata)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.4.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0.0-orange.svg?style=flat-square)](https://github.com/ncordon/imbalance/commits/master)

# smartdata

Package that integrates preprocessing algorithms for oversampling,
instance/feature selection, normalization, discretization, space
transformation, and outliers/missing values/noise cleaning.

## Installation

You can install smartdata from github with:

``` r
# install.packages("devtools")
devtools::install_github("ncordon/smartdata")
```

and load it into an R session with:

``` r
library("smartdata")
```

## Examples

`smartdata` provides the following wrappers:

  - `instance_selection`
  - `feature_selection`
  - `normalize`
  - `discretize`
  - `space_transformation`
  - `clean_outliers`
  - `impute_missing`
  - `clean_noise`

To get the possible methods available for a certain wrapper, we can do:

``` r
which_options("instance_selection")
#> Possible methods are: 'CNN', 'ENN', 'multiedit', 'FRIS'
```

To get information about the parameters available for a method:

``` r
which_options("instance_selection", "multiedit")
#> For more information do: ?class::multiedit 
#> Parameters for multiedit are: 
#>   * k: Number of neighbors used in KNN 
#>        Default value: 1 
#>   * num_folds: Number of partitions the train set is split in 
#>                Default value: 3 
#>   * null_passes: Number of null passes to use in the algorithm 
#>                  Default value: 5
```

First let’s load a bunch of datasets:

``` r
data(iris0,  package = "imbalance")
data(ecoli1, package = "imbalance")
data(nhanes, package = "mice")
```

#### Oversampling

``` r
super_iris <- iris0 %>% oversample(method = "MWMOTE", ratio = 0.8, filtering = TRUE)
```

#### Instance selection

``` r
super_iris <- iris %>% instance_selection("multiedit", k = 3, num_folds = 2, 
                                          null_passes = 10, class_attr = "Species")
```

#### Feature selection

``` r
super_ecoli <- ecoli1 %>% feature_selection("Boruta", class_attr = "Class")
```

#### Normalization

``` r
super_iris <- iris %>% normalize("min_max", exclude = c("Sepal.Length", "Species"))
```

#### Discretization

``` r
super_iris <- iris %>% discretize("ameva", class_attr = "Species")
```

#### Space transformation

``` r
super_ecoli <- ecoli1 %>% space_transformation("lle_knn", k = 3, num_features = 2)
```

#### Outliers

``` r
super_iris <- iris %>% clean_outliers("multivariate", type = "adj")
```

#### Missing values

``` r
super_nhanes <- nhanes %>% impute_missing("gibbs_sampling")
```

#### Noise

``` r
super_iris <- iris %>% clean_noise("hybrid", class_attr = "Species", 
                                   consensus = FALSE, action = "repair")
```
