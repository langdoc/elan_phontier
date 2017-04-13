#!/bin/sh

Rscript -e "knitr::knit('README.Rmd', 'README.md')"
Rscript -e "knitr::knit('README.Rmd', 'README.html')"
