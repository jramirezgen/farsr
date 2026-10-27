# farsr

[![Build Status](https://app.travis-ci.com/jramirezgen/farsr.svg?branch=main)](https://app.travis-ci.com/jramirezgen/farsr)

`farsr` reads, summarizes and maps the data of the US National Highway Traffic
Safety Administration's Fatality Analysis Reporting System (FARS), a nationwide
census of fatal injuries in motor vehicle crashes. The package includes the FARS
files for 2013 to 2015.

## Installation

```r
# install.packages("devtools")
devtools::install_github("jramirezgen/farsr", build_vignettes = TRUE)
```

## Example

```r
library(farsr)

# The data files shipped with the package
setwd(system.file("extdata", package = "farsr"))

fars_summarize_years(c(2013, 2014))
fars_map_state(1, 2013)
```

See `vignette("farsr")` for a longer description.
