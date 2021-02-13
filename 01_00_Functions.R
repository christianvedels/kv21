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









# These are som random notes:

# Målgruppetanker: Unger hipsters under uddannelse

# Kan man finde statistik på usikker boligsituation?

# Kort over valgresultater

# Prøv at lave forudsigelse

# Unge kandidater tal?

# Vælgerundersøgelser

# 


