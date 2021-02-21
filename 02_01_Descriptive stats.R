# Clean valgdatabase  
# Date started      :   2020-02-21
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

personlige_stemmer = read.csv2("../personlige_stemmer.csv")


# =================================
# Libraries and preparation
# =================================

personlige_stemmer %>% 
  filter(KandidatNavn == "Carl Valentin") %>% 
  filter(Kommune.navn == "København") %>% 
  select(KandidatNavn, Kommune.navn, Valgsted.navn, Stemmer) %>% 
  mutate(
    pct_kbh = pretty_pct(Stemmer/sum(Stemmer)) 
  ) %>% 
  arrange(-Stemmer)
