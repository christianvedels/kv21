# Functions  
# Date last modified:   2020-02-05
# Auhtor            :   Christian Vedel 
# Purpose           :   Contains custom functions pertaining to this project.
#
# Details:
#
#
# =================================
# read_one(path)
# =================================
# This function read the data from one election
#
# Arguments:
# path:   The path of data for this election
read_one = function(path){
  # Find all files on path
  the_files = list.files(path)
  
  # Load all files
  result = list()
  for(i in the_files){
    fname_i = paste(sep = "/", path, i)
    suppressWarnings({
      result[[i]] = read.csv2(fname_i)
    })
  }
  
  # Clean names
  names(result) = gsub(".csv", "", names(result))
  names(result) = gsub("_-", "", names(result))
  
  return(result)
}

# =================================
# clean_valgdata(frame, election)
# =================================
# Cleans valgdata
#
# Arguments:
# frame:    Frame of type 'valgdata'
# elec_str: String for what election e.g. "EV04"

clean_valgdata = function(frame, elec_str){
  # frame = elections$EV04$Valgdata
  # elec_str = "EV04"
  
  # Handle missing frame
  if(is.null(frame)){
    warning("Frame was NULL. clean_valgdata() will return NULL")
    return(NULL)
  }
  
  # Misc preparation
  elec_str20 = paste(
    sep = "",
    substr(elec_str,1,2),
    "20",
    substr(elec_str,3,4)
  )
  
  elec_str20_dash = paste(sep = "", elec_str20, "...")
  
  cnames = names(frame)
  pivot_cols = cnames[which(grepl(elec_str20,cnames))]
  
  # Replace "-" with 0 and make numeric
  for(i in 1:NROW(frame)){
    for(j in 1:NCOL(frame)){
      if(frame[i,j]=="-"){
        frame[i,j] = 0
      }
    }
  }
  
  frame = frame %>% mutate_all(as.numeric)
  
  # pivoting
  frame = frame %>% 
    pivot_longer(
      cols = pivot_cols,
      names_to = "Parti",
      values_to = "Stemmer"
    ) %>% 
    mutate(
      Parti = gsub(elec_str20_dash,"",Parti)
    )
  
  warning("Partinavne er nogle gange bogstaver - nogle gange navne")
  
  return(frame)
}









# These are som random notes:

# Målgruppetanker: Unger hipsters under uddannelse

# Kan man finde statistik på usikker boligsituation?

# Kort over valgresultater

# Prøv at lave forudsigelse

# Unge kandidater tal?

# Vælgerundersøgelser

# 


