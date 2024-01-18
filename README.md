# sectorgap

'sectorgap' enables the estimation of a large Bayesian state space model for economic trend cycle decomposition. Economic output is decomposed into potential output and the output gap, consistent with individual sub-sectors of the economy and a set of economic indicators, e.g. regarding labor market and inflation dynamics.

Details on the methodology can be found here:

[KOF Working Paper 514](https://www.research-collection.ethz.ch/handle/20.500.11850/653682)

A related paper that uses the above methodology can be found here:

[KOF Working Paper 513](https://www.research-collection.ethz.ch/handle/20.500.11850/642427)

If you use 'sectorgap' in your paper, please cite it properly, see `citation("sectorgap")` in R, or above link to the paper.

## Details

Determining potential output and the output gap - two inherently unobservable variables - is a major challenge for macroeconomists. This paper presents the R package sectorgap, which features a flexible modeling and estimation framework for a multivariate Bayesian state space model identifying economic output fluctuations consistent with subsectors of the economy. The proposed model is able to capture various correlations between output and a set of aggregate as well as subsector indicators. Estimation of the latent states and parameters is achieved using a simple Gibbs sampling procedure and various plotting options facilitate the assessment of the results. 

## Main features

- data preparation
- state space model definition
- prior initialization
- Bayesian estimation via Gibbs sampling
- visualization of the results

## Install the package
You can install the package from ‘Github’ using the **install_github** function from the **devtools** package.
``` 
library(devtools)
install_github('sinast3000/sectorgap')
```

***
Streicher, S. (2024). sectorgap: An R package for consistent economic trend cycle decomposition. KOF Working Papers 514.

Rathke A. and S. Streicher (2023). Improving output gap estimation---a bottom-up approach. KOF Working Papers 513.