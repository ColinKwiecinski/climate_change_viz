# Need to use this method of knit because rstudio doesn't work properly.
# This code should knit the index.Rmd file with analysis.R properly
source("analysis.R")
library(knitr)
rmarkdown::render("index.Rmd")
