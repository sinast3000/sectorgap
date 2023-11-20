# sectorgap

'sectorgap' enables the estimation of a large Bayesian state space model for economic trend-cycle decomposition. Economic output is decomposed into potential output and the output gap, consistent with individual sectors of the economy and labor market and inflation dynamics.

Details on the methodology can be found here:

[KOF Working Paper](https://www.research-collection.ethz.ch/handle/20.500.11850/642427)

If you use 'sectorgap' in your paper, please cite it properly, see `citation("sectorgap")` in R, or above link to the paper.

## Details

some details...

## Main features

- state space model definition
- Bayesian estimation via Gibbs sampling
- visiualization of the results

## Install the package
You can install the package from ‘Github’ using the **install_github** function from the **devtools** package.
``` 
library(devtools)
install_github('sinast3000/sectorgap')
```