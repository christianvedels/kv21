# Clean valgdatabase  
# Date last modified:   2020-02-05
# Auhtor            :   Christian Vedel 
# Purpose           :   This script loads, cleans and collects data from valgdatabase.dst.dk
#
# Details:
# 

# =================================
# Libraries and preparation
# =================================

rm(list = ls())

library(tidyverse)

source("01_00_Functions.R")

# =================================
# Read_raw
# =================================

meta_data = read.csv2("Valgdatabase_meta.csv")

elections = list()

for(i in 1:NROW(meta_data)){
  path = paste(
    sep = "/",
    "../Valgdatabase",
    meta_data$Folder[i]
  )
  
  elections[[i]] = read_one(path)
  names(elections)[i] = meta_data$Folder[i]
  attr(elections[[i]],"elec_str") = meta_data$Folder[i]
}

# =================================
# Format valgdata
# =================================
for(i in 1:length(elections)){
  elections[[i]] = clean_valgdata(
    elections[[i]]
  )
}

# ============================
# Make partinavn assoc. cloud
# ============================
# Make loop to find all parti names
for(i in 1:length(elections)){
  
}

