# RDarwin <img src='vignettes/img/RDarwin.png' align="right" style = "float:right; height: 150px;" alt = "RDarwin logo"/>

## Overview

`RDarwin` is an R package designed to facilitate the usage of [pyDarwin](https://certara.github.io/pyDarwin/html/index.html) with the Certara NLME pharmacometric modeling engine from the R command line. The Python package, pyDarwin, is a powerful tool for using machine learning algorithms for model selection.

`RDarwin` defines a structure for NLME-pyDarwin usage by providing a set of R functions that can be used to specify search options across observation, dosepoints, structural parameters, fixed effects (thetas), random effects (omegas), and covariate effects. Additionally, users may execute a pyDarwin search directly from R using the function `run_pyDarwin()` (supports both NLME and NONMEM modeling engines).

## Installation

```r
install.packages("Certara.RDarwin", 
  repos = c("https://certara.jfrog.io/artifactory/certara-cran-release-public/", 
  "https://cloud.r-project.org"), method = "libcurl")

```

*Note: Additional [installation of pyDarwin](https://certara.github.io/pyDarwin/html/Install.html#install-pydarwin) and Certara's NLME-Engine used by RsNLME is required. * <a href="mailto:certara.licensing@certara.com?subject=RsNLME Trial License&body=Hello, I would like to request a 30-day trial license of the NLME-Engine to be used with RsNLME and pyDarwin: %0d%0a%0d%0a
    Name: %0d%0a
    Email: %0d%0a
    Organization/Institution:">Request NLME-Engine</a>.
    

