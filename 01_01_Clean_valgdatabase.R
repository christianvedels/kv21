# Clean valgdatabase  
# Date started      :   2020-02-05
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
  elections[[i]] = clean_valgdata(elections[[i]])
  pretty_status(i, length(elections))
}

# =================================
# Collect all data
# =================================
valgdata = elections$EV04$Valgdata %>% filter(Stemmer == -10)
persolige_stemmer = elections$EV04$Valgdata_personlige_stemmer %>% filter(Stemmer == -10)
kandidater = elections$EV04$Kandidater_stamdata %>% filter(Orden == -10)
  
for(i in 1:length(elections)){
  valgdata = valgdata %>% bind_rows(elections[[i]]$Valgdata)
  if(is.null(elections[[i]]$Kandidater_stamdata)){
    next # Handle cases where personlige stemmer is missing
  }
  persolige_stemmer = persolige_stemmer %>% bind_rows(elections[[i]]$Valgdata_personlige_stemmer)
  kandidater = kandidater %>% bind_rows(elections[[i]]$Kandidater_stamdata)
}


# =================================
# Save data
# =================================

valgdata %>% write.csv2("../valgdata.csv", row.names = FALSE)
persolige_stemmer %>% write.csv2("../personlige_stemmer.csv", row.names = FALSE)
kandidater %>% write.csv2("../kandidater.csv", row.names = FALSE)
  
  
  
  
  