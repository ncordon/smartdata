## Version 1.0.2
Corrects the following bugs:
  * The instance selection wrapper applied for methods *information_gain*, *gain_ratio* and *sym_uncertainty* returned error when there was no categorical attribute in the dataset, apart from the class one.
  * Changes `num_attrs` parameter for `num_features` in feature selection to standardize parameters w.r.t. space transformation wrapper.
  * Corrects output of lle space transformation: result was a matrix instead of a dataset.

## Test environments
* ubuntu 14.04 (on travis-ci), release
* local Arch Linux 4.14.90-1-lts, devel and release
* win-builder (devel and release)

## R CMD check results
There were no ERRORs or WARNINGs. 
    
## Downstream dependencies
I have also run R CMD check on downstream dependencies of imbalance:
    
    * No ERRORs or WARNINGs found

