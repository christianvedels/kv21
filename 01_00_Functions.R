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
# Cleans all the different elements and merges things together
#
# Arguments:
# frames:    Frame of type 'valgdata'
# elec_str: String for what election e.g. "EV04"

clean_valgdata = function(frames){
  
  # Valgdata
  
  # frames$Valgdata = elections$EV04$Valgdata
  # elec_str = "EV04"
  
  elec_str = attr(frames, "elec_str")
  
  # Handle missing frames$Valgdata
  if(is.null(frames$Valgdata)){
    warning("frames$Valgdata was NULL")
  }
  
  # Misc preparation
  elec_str20 = paste(
    sep = "",
    substr(elec_str,1,2),
    "20",
    substr(elec_str,3,4)
  )
  
  elec_str20_dash = paste(sep = "", elec_str20, "...")
  
  cnames = names(frames$Valgdata)
  pivot_cols = cnames[which(grepl(elec_str20,cnames))]
  
  # Replace "-" with 0 and make numeric
  for(i in 1:NROW(frames$Valgdata)){
    for(j in 1:NCOL(frames$Valgdata)){
      if(frames$Valgdata[i,j]=="-"){
        frames$Valgdata[i,j] = 0
      }
    }
  }
  
  frames$Valgdata = frames$Valgdata %>% mutate_all(as.numeric)
  
  # pivoting
  frames$Valgdata = frames$Valgdata %>% 
    pivot_longer(
      cols = all_of(pivot_cols),
      names_to = "Parti",
      values_to = "Stemmer"
    ) %>% 
    mutate(
      Parti = gsub(elec_str20_dash,"",Parti),
      Election = elec_str
    )
  
  
  # Joining on Valg_stamdata
  if(is.null(frames$Valg_stamdata)){
    warning("frames$Valg_stamdata was NULL")
  }
  
  if(NROW(frames$Valg_stamdata)>1){
    stop("Valg_stamdata longer than expected")
  }
  
  frames$Valg_stamdata = frames$Valg_stamdata %>% 
    mutate(
      Electio_date = as.Date(as.character(Valgdag), format = "%Y%m%d", origin = "1970-01-01")
    )
  
  frames$Valgdata = frames$Valgdata %>% 
    mutate(
      ValgID = frames$Valg_stamdata$ValgId,
      Kategori = frames$Valg_stamdata$Kategori,
      Valgdag = frames$Valg_stamdata$Valgdag,
      Vej.dato = frames$Valg_stamdata$Vej.dato,
      Statistik.dato = frames$Valg_stamdata$Statistik.dato,
    )
  
  frames$Valg_stamdata = NULL
  
  # Joining on geo
  frames$Geografiske_stamdata = frames$Geografiske_stamdata %>% rename(ValgstedId = Valgsted.Id)
  frames$Valgdata = frames$Valgdata %>% full_join(frames$Geografiske_stamdata, by = "ValgstedId")
  
  frames$Geografiske_stamdata = NULL
  
  # Udregningstabel 
  frames$Udregningstabel = frames$Udregningstabel %>% rename(ValgstedId = Valgsted.Id)
  frames$Valgdata = frames$Valgdata %>% left_join(frames$Udregningstabel, by = "ValgstedId")
  
  frames$Udregningstabel = NULL
  
  # Personlige stemmer
  if(is.null(frames$Valgdata_personlige_stemmer)){ # Only if available
    return(frames)
  }
  
  frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
    rename(
      bd_string = Fødselsdag
    ) %>% 
    mutate(
      birth_year = substrRight(bd_string, 4)
    ) %>% 
    rowwise() %>% 
    mutate(
      date_month = substrRight_inv(bd_string, 4)
    ) %>% 
    mutate(
      birth_month = substrRight(date_month, 2),
      birth_day = substrRight_inv(date_month, 2)
    ) %>% 
    select(
      -bd_string,
      -date_month
    ) %>% 
    mutate(
      birth_day = str_pad(birth_day, 2, pad = "0")
    ) %>% 
    mutate(
      birthdate = paste(
        sep = "-",
        birth_year,
        birth_month,
        birth_day
      ) 
    ) %>% 
    mutate(
      birthdate = as.Date(birthdate)
    )
  
  pivot_cols = names(frames$Valgdata_personlige_stemmer) 
  pivot_cols = pivot_cols[which(grepl(elec_str20,pivot_cols))]
  
  # frames$Valgdata_personlige_stemmer = 
  
  
  x = suppressWarnings({
     frames$Valgdata_personlige_stemmer %>% 
      mutate_all(as.numeric)
  }) %>% pivot_longer(
    cols = pivot_cols,
    names_to = "Parti",
    values_to = "Stemmer"
  ) %>% 
    mutate(
      Parti = gsub(elec_str20_dash, "", Parti)
    ) %>% 
    filter(!grepl("ersonlige",Parti)) %>% 
    mutate(
      tmp = strsplit(Parti, "\\.")
    ) %>% 
    rowwise() %>% 
    mutate(
      Parti = tmp[1],
      Placering = tmp[2]
    ) %>% 
    ungroup() %>% 
    mutate(
      Placering = as.numeric(Placering)
    ) %>% 
    select(-tmp) %>% 
    full_join( # Join on kandidatdata
      frames$Kandidater_stamdata, by = c("Parti","Placering"="Orden")
    )
  
  
  return(frames)
}



# =================================
# print_pretty_vector(x, sep = ",\n")
# =================================
# This function read the data from one election
#
# Arguments:
# x:   Vector
# sep: Sep to use between each element

print_pretty_vector = function(x, sep = ",\n"){
  res = paste(
    collapse = sep,
    x
  )
  
  cat(res)
}

# =================================
# substrRight(x, n)
# =================================
# substr() but from the right
#
# Arguments:
# x:    Character string
# n:    n elements from the right

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}


# subtrRight_inv()
# Returns the rest
substrRight_inv = function(x, n){
  substr(x, 1, nchar(x)-n)
}













# These are som random notes:

# Målgruppetanker: Unger hipsters under uddannelse

# Kan man finde statistik på usikker boligsituation?

# Kort over valgresultater

# Prøv at lave forudsigelse

# Unge kandidater tal?

# Vælgerundersøgelser

# 


