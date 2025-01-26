library(tidyverse)
library(knitr)
library(kableExtra)
library(gtsummary)
library(labelled)
library(patchwork)
library(readxl)

refmt <- function(var, fmt){
  plyr::revalue(as.factor(var), fmt)
}
miss99 <- function(x){
  as.numeric(if_else(x == 99, NA, x))
}
