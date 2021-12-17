# subgroupsem: Subgroup Discovery in Structural Equation Models
[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](https://www.repostatus.org/badges/latest/wip.svg)](https://www.repostatus.org/#wip)

subgroupsem is an R package for discovering interesting subgroups with distinct sets of parameters in structural equation models (SEM). It can be installed via GitHub.

## Installation
`subgroupsem` is currently not on CRAN. The development version of `subgroupsem` can be installed directly from this GitHub repository using the additional package `devtools`. Under Windows, please make sure Rtools (http://cran.r-project.org/bin/windows/Rtools) are installed and no older version of `subgroupsem` is currently loaded:

```
install.packages("devtools")
library(devtools)

install_github("chkiefer/subgroupsem")
```

## Run subgroupsem
The main function of the package is `subgroupsem()`. The documentation of this function helps to introduce you to its functionality.
