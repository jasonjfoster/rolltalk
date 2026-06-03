# rolltalk

[![](https://github.com/jasonjfoster/rolltalk/actions/workflows/check-standard.yaml/badge.svg)](https://github.com/jasonjfoster/rolltalk/actions/workflows/check-standard.yaml)

## Overview

'rolltalk' provides a presentation of rolling and expanding statistics for time-series data.

The 'rolltalk' package demonstrates the rolling and expanding statistics of the 'roll' package in a 'Quarto' presentation that is rendered to a PDF file with the `roll_talk()` function.

## Installation

Install the released version from CRAN:

```r
# install.packages("rolltalk")
```

Or the development version from GitHub:

```r
# install.packages("pak")
pak::pak("jasonjfoster/rolltalk")
```

## Usage

Load the package and render the presentation:

```r
library(rolltalk)

# rolling and expanding statistics
roll_talk()
```
