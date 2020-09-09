#' ---
#' title: "Report for editting"
#' output:
#'   html_document:
#'     toc: true
#'date: '`r format(Sys.time(), "%d %B, %Y")`'     
#' ---
#'
#+warning=FALSE,message=FALSE,echo=FALSE
library(knitr)
#'

#'#  Bathymetry {.tabset .tabset-fade .tabset-pills} 

#'## Elevation relative to sea level 

#+echo=FALSE,warning=FALSE,message=FALSE, out.width='100%' 

knitr::include_graphics('plots/Bathymetry_Spatial.png')

#'
