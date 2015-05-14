# manipulation
library(dplyr)
library(readr)
library(lubridate)
library(stringr)

# scraper
library(XML)

# graphics
library(ggplot2)
library(scales)

# networks
library(network)
library(sna)
library(tnet)

# network plots
library(GGally)
library(animation)

# folders
dir.create("data", showWarnings = FALSE)
dir.create("data/pages", showWarnings = FALSE)
dir.create("data/covers", showWarnings = FALSE)
dir.create("plots", showWarnings = FALSE)

# scripts
source("code/0-set-themes.r")
source("code/1-get-covers.r")
source("code/2-one-mode.r")
source("code/3-two-mode.r")

# kthxbye
