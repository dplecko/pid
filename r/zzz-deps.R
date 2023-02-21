pkgs <- c("readr", "ggplot2", "Rcpp", "reshape2", "cowplot", "parallel",
          "assertthat", "gmp", "igraph", "causaleffect", "plotly", "index0",
          "data.table")

if (!all(vapply(pkgs, requireNamespace, logical(1L)))) {
  stop("Packages {pkgs} are required in order to proceed.")
  if (!interactive()) q("no", status = 1, runLast = FALSE)
}

library(index0)

`[.index0` <- function (x, i, j, k, l, ...) 
{
  if (!missing(i)) 
    i <- i + 1
  if (!missing(j)) 
    j <- j + 1
  if (!missing(k)) 
    k <- k + 1
  if (!missing(l)) 
    l <- l + 1
  as.index0(NextMethod())
}

`[<-.index0` <- function (x, i, j, k, l, ..., value) 
{
  if (!missing(i)) 
    i <- i + 1
  if (!missing(j)) 
    j <- j + 1
  if (!missing(k)) 
    k <- k + 1
  if (!missing(l)) 
    l <- l + 1
  as.index0(NextMethod())
}

library(ggplot2)
library(reshape2)
library(readr)
library(Rcpp)
library(cowplot)
library(assertthat)
library(gmp)
library(igraph)
library(causaleffect)
library(parallel)
library(plotly)
library(data.table)
