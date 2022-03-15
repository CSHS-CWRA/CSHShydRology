[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html) 
[![CRAN
status](https://www.r-pkg.org/badges/version/CSHShydRology)](https://cran.r-project.org/package=CSHShydRology)
[![Travis build status](https://travis-ci.org/CSHS-CWRA/CSHShydRology.svg?branch=master)](https://travis-ci.org/CSHS-CWRA/CSHShydRology)
[![license](https://img.shields.io/badge/license-GPL3-lightgrey.svg)](https://choosealicense.com/)
# CSHShydRology

## Installing this package
The package may be downloaded directly from [CRAN](https://cran.r-project.org/package=CSHShydRology) with:
``` r
install.packages("CSHShydRology")
```

Previous versions of the package and manuals can be found by clicking on **releases**. The latest version of the package may also be installed directly from this repository. The procedure is
1. Install the package "devtools" - you only have to do this once. Note that this will also install several dependancies
2. Load the devtools library
3. Install the package.

The commands are:
``` R
if (!require(devtools)) install.packages("devtools")
library(devtools)
install_github("CSHS-CWRA/CSHShydRology")
```

## What is CSHS Hydrology?
This is an R package of functions used by Canadian hydrologists. The name is in recognition of the support provided by the Canadian Association Society for Hydrological Sciences (CSHS) which is an affiliated society of the Canadian Water Resources Association (CWRA). The CSHS website is https://cwra.org/en/affiliates-programs/cshs/.

## Themes
This package contains functions which are grouped into themes. Currently the themes include:
- Statistical hydrology (trend detection, data screening, frequency analysis, regionalization),
- Basic data manipulations (input/conversion/adapter functions, missing data infilling),
- Visualization (data visualization, standardized plotting functions),
- Spatial hydrology (basin delineation, landscape data analysis, working with GIS),
- Streamflow measurement analysis (rating curve analysis, velocity profiles, naturalization).

Other themes, such as Network design/analysis (homogeneity assessment), and Ecohydrology (fisheries and ecological analysis) will also be added if there is interest.

## Coding standards
### General standards
The general coding standards are as specified in the Google R coding guide at https://google.github.io/styleguide/Rguide.xml.


### External packages
To reduce errors, it is a good idea to reduce reliance on other packages as much as is possible. Please check through the functions already referenced in the **DESCRIPTION** file before adding additional functions. When it is necessary to add a reference to an additional package, please do the following:
Make the package suggested, rather than required.
Make a note in the function’s Roxygen documentation that the function uses the package.
Make sure that the version number of the package is referenced properly.
When more than one function requires a given package, it can be made a required packages.
### Devtools
To make package development easier, we will use the package **devtools**, which provides several tools. This package is explained at https://github.com/hadley/devtools.
#### Checking
The devtools check function will be used check each function for errors in syntax and style. Each function must pass the checks with zero warnings, errors or notes. Note that once you have loaded **devtools**, the check function can be accessed from a menu or from the build tab in Rstudio. It is also a good idea to manually run the **check** function from the command line, i.e. 
```R
R CMD check
```
as this can catch other problems.
#### Documentation
All functions need to be documented using the **Roxygen** package, which allows you to create the help files and documentation using comments in your function. The package is explained at https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html. Rstudio has a very nice quick reference for **Roxygen** which you can access with Help|Roxygen Quick Reference.
Each function must include the name of the authors(s). Please include your email address, and any publications that you want to reference.
Note that the package itself also has a function. This contains documentation about the package as a whole, as well as settings which are used to create the package **NAMESPACE** file. Note that this means that the **NAMESPACE** file should _never_ be edited directly by anyone.
When the package is compiled and tested, **Roxygen** will create a .Rd file for each function, the package, and for the included data files. These files should never be edited manually. If you want to check how your .Rd file will look, you can load it into Rstudio, which can render it.
#### Vignettes
Vignettes are long form documentation which can be included in R packages. They can include long descriptions and worked examples, and are very useful to thoroughly explain functions to the users. The creation process is explained at http://r-pkgs.had.co.nz/vignettes.html. Even if you don’t write code, you can contribute to writing vignettes for functions.
#### Testing
The testthat package allows unit tests to be created for each function. The function is executed, using specified data, and the results are compared to specified values. Unit tests should be used whenever possible, as they will identify problems caused by changes in upstream packages. The testthat package is explained in detail in http://r-pkgs.had.co.nz/tests.html.
#### Test data
Each function is required to have an example, which should be executable code. The only exception should be for code which is intended to access data which may not be available, such as data downloaded from a server.
The example, vignettes, and unit tests will all require test data sets. The data are stored in the /data folder of the package, and should be stored as .Rdata (binary) files, unless reading in data from another format is the point of the function. CRAN has a maximum limit of 1 MB for test data, so we will need to minimise the datasets we include. We should minimise the size of each test dataset and re-use the same data in as many functions as possible. If a function requires data output by another function, both functions can be included in the example.
Each dataset needs to be documented in the same way as a function. The creation of datasets and their documentation is explained at http://r-pkgs.had.co.nz/data.html.

## Working with GitHub
There is a lot of information available on using git and GitHub in R. The best starting point is https://support.rstudio.com/hc/en-us/articles/200532077-Version-Control-with-Git-and-SVN which shows how to integrate git and GitHub with RStudio.


## Citation

The package citation may be generated with the following command in R.
```{r}
citation("CSHShydRology")
```


