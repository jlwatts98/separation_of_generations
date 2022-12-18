##### header #####

# This is the header. Run this before anything else.

# Clear workspace
rm(list = ls())
graphics.off()

# load libraries
library(checkmate)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(phytools)
library(readr)
library(stringr)
library(taxize)
library(urltools)
library(stats)
library(SDMtune)
library(CoordinateCleaner)
library(raster)
