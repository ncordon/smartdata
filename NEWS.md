# smartdata 1.0.2
  * Corrects a bug for the instance selection wrapper applied for methods *information_gain*, *gain_ratio* and *sym_uncertainty* which gave failure when there was no categorical attribute in the dataset, apart from the class one.
  * Changes `num_attrs` parameter for `num_features` in feature selection to standardize parameters w.r.t. space transformation wrapper.
  * Corrects a bug for lle space transformation: result was a matrix instead of a dataset.

# smartdata 1.0.1
  * Corrects the titles of the vignettes so they appear correctly on CRAN
  * Corrects issue compiling one of the vignettes (issue regarding fancyvrb / xcolor with options)

# smartdata 1.0.0
  * First release
  * Methods for instance selection, feature selection, normalization, discretization, space transformation, clean outliers, impute missing values or clean noise instances
