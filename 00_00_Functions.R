# Functions  
# Date started      :   2020-02-05
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
  
  # =========
  # Valgdata:
  # =========
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
      Election_date = as.Date(as.character(Valgdag), format = "%Y%m%d", origin = "1970-01-01")
    )
  
  frames$Valgdata = frames$Valgdata %>% 
    mutate(
      ValgID = frames$Valg_stamdata$ValgId,
      Kategori = frames$Valg_stamdata$Kategori,
      Valgdag = frames$Valg_stamdata$Valgdag,
      Vej.dato = frames$Valg_stamdata$Vej.dato,
      Statistik.dato = frames$Valg_stamdata$Statistik.dato,
      Election_date = frames$Valg_stamdata$Election_date
    )
  
  # Joining on geo
  frames$Geografiske_stamdata = frames$Geografiske_stamdata %>% rename(ValgstedId = Valgsted.Id)
  frames$Valgdata = frames$Valgdata %>% full_join(frames$Geografiske_stamdata, by = "ValgstedId")
  
  # Udregningstabel 
  frames$Udregningstabel = frames$Udregningstabel %>% rename(ValgstedId = Valgsted.Id)
  frames$Valgdata = frames$Valgdata %>% left_join(frames$Udregningstabel, by = "ValgstedId")
  
  # ===================
  # Personlige stemmer:
  # ===================
  if(is.null(frames$Valgdata_personlige_stemmer)){ # Only if available
    
    frames$Geografiske_stamdata = NULL
    frames$Udregningstabel = NULL
    frames$Valg_stamdata = NULL
    return(frames)
  }
  
  frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
    rename(
      bd_string = Fødselsdag
    )
  
  # Which bd format is used? Handle accordingly
  valg_meta = read.csv2("Valgdatabase_meta.csv")
  valg_meta = valg_meta %>% filter(Folder == elec_str)
  
  if(valg_meta$bd_format == "DDMMYYYY"){
    frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
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
  } else if(valg_meta$bd_format == "YYYYMMDD"){
    frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
      mutate(
        birth_year = substr(bd_string, 1, 4)
      ) %>% 
      rowwise() %>% 
      mutate(
        date_month = substring(bd_string, 5)
      ) %>% 
      mutate(
        birth_month = substring(date_month, 1, 2),
        birth_day = substring(date_month, 3)
      ) %>% 
      select(
        -bd_string,
        -date_month
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
  } else if(valg_meta$bd_format == "missing"){
    frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
      mutate(
        birth_year = NA,
        birth_month = NA,
        birth_day = NA,
        birthdate = NA
      ) %>% 
      select(-bd_string)
  }
  
  # Pivoting
  pivot_cols = names(frames$Valgdata_personlige_stemmer) 
  pivot_cols = pivot_cols[which(grepl(elec_str20,pivot_cols))]
  
  frames$Valgdata_personlige_stemmer = suppressWarnings({
     frames$Valgdata_personlige_stemmer %>% 
      mutate_all(as.numeric)
  }) %>% pivot_longer(
    cols = all_of(pivot_cols),
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
    select(-tmp)
  
  
  frames$Valgdata_personlige_stemmer = frames$Valgdata_personlige_stemmer %>%
    full_join( # Join on kandidatdata
      frames$Kandidater_stamdata, by = c(
        "Parti",
        "Placering"="Orden", 
        "StorKredsNr"="Storkreds.Nr"
      )
    ) %>% 
    drop_na(Stemmer)
  
  # Joining on Valg_stamdata
  frames$Valgdata_personlige_stemmer = frames$Valgdata_personlige_stemmer %>% 
    mutate(
      ValgID = frames$Valg_stamdata$ValgId,
      Kategori = frames$Valg_stamdata$Kategori,
      Valgdag = frames$Valg_stamdata$Valgdag,
      Vej.dato = frames$Valg_stamdata$Vej.dato,
      Statistik.dato = frames$Valg_stamdata$Statistik.dato,
      Election_date = frames$Valg_stamdata$Election_date
    )
  
  # Joining geo to pers
  frames$Valgdata_personlige_stemmer = frames$Valgdata_personlige_stemmer %>%
    full_join(frames$Geografiske_stamdata, by = "ValgstedId") %>% 
    coerce_if_same()
  
  # Joining udregningstabel
  frames$Valgdata_personlige_stemmer = frames$Valgdata_personlige_stemmer %>% 
    left_join(frames$Udregningstabel, by = "ValgstedId") %>% 
    coerce_if_same()
  
  # Delete data not used any further
  frames$Geografiske_stamdata = NULL
  frames$Udregningstabel = NULL
  frames$Valg_stamdata = NULL
  
  # Info to stamdata
  frames$Kandidater_stamdata = frames$Kandidater_stamdata %>% 
    mutate(
      ValgID = frames$Valg_stamdata$ValgId,
      Kategori = frames$Valg_stamdata$Kategori,
      Valgdag = frames$Valg_stamdata$Valgdag,
      Vej.dato = frames$Valg_stamdata$Vej.dato,
      Statistik.dato = frames$Valg_stamdata$Statistik.dato,
      Election_date = frames$Valg_stamdata$Election_date
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

# =================================
# coerce_if_same(frame)
# =================================
# When joining causes .x and .y versions of the same column this function
# tests if they have the same content and if this is the case then only one is
# kept.
# If one column contains NA the function perform imputations based on the other column
# 
#
# Arguments:
# frame:  frame to perform this on
coerce_if_same = function(frame, verbose = FALSE){
  cnames = names(frame)
  
  to_fix = data.frame(
    names.x = cnames[grep("\\.x",cnames)] %>% sort(),
    names.y = cnames[grep("\\.y",cnames)] %>% sort()
  )
  
  if(NROW(to_fix)==0){
    if(verbose){
      warning("Apparently nothing to fix. Returning frame without changes.")
    }
    return(frame)
  }
  
  to_fix = to_fix %>% 
    mutate(
      plain_name = gsub("\\.x","",names.x)
    ) %>% 
    mutate(
      test = gsub("\\.x","",names.x) == gsub("\\.y","",names.y)
    )
  
  # Test
  if(any(!to_fix$test)){
    stop("Somehow matched columns which are not the same")
  }
  
  # Init col
  to_fix$same = NA
  
  # Fixing
  for(i in 1:NROW(to_fix)){
    data_i = frame[,c(to_fix$names.x[i],to_fix$names.y[i])]
    
    # Fix if 0 is used as NA
    x1 = data_i[,1] %>% unlist() %>% na.omit()
    x2 = data_i[,2] %>% unlist() %>% na.omit()
    
    if(length(x1)>0){
      if(all(x1==0)){
        data_i[,1] = NA
      }
    }
    
    if(length(x2)>0){
      if(all(x2==0)){
        data_i[,2] = NA
      }
    }
    
    # NA imputing
    na2 = is.na(data_i[,2] %>% unlist())
    
    x1 = data_i[,1] %>% unlist()
    x2 = data_i[,2] %>% unlist()
    
    data_i[,2] = ifelse(na2, x1, x2)
    
    na1 = is.na(data_i[,1] %>% unlist())
    x2 = data_i[,2] %>% unlist()
    
    data_i[,2] = ifelse(na1, x1, x2)
    
    # Check if same
    x1 = data_i[,1] %>% unlist()
    x2 = data_i[,2] %>% unlist()
    
    same_i = all(sameNA(x1, x2))
    
    to_fix$same[i] = same_i
    
    # Write back after fix
    frame[,c(to_fix$names.x[i],to_fix$names.y[i])] = data_i
    
    # Change if same
    if(same_i){
      cnames_i = colnames(frame)
      frame = frame[,-which(cnames_i == to_fix$names.y[i])]
      colnames(frame)[which(cnames_i == to_fix$names.x[i])] = to_fix$plain_name[i]
    }
  }
  
  # Test
  if(!(length(cnames) - sum(to_fix$same)) == NCOL(frame)){
    print_pretty_vector(to_fix$plain_name)
    stop("Number of cols in final frame and cols coereced does not make sense")
  }
  
  return(frame)
  
}




# =================================
# sameNA(x, y)
# =================================
# Test if x and y are the same or both NA

sameNA = function(x, y){
  res1 = x == y
  res2 = is.na(x) & is.na(y)
  
  return(
    res1 | res2 # If either or both, then TRUE
  )
  
}

# ====================
# pretty_pct(x)
# ====================
# Returns a fractions as a pretty pct string
#
# Args:
# x:    Fractio to print
#
pretty_pct = function(x, digits = 4){
  x = signif(x, digits = digits)
  x = x*100
  x = paste(x,"%",sep ="")
  return(x)
}


# ====================
# pretty_status(x)
# ====================
# Writes pretty status in loops
#
# Args:
# i:            Current i
# total_length: Total length of the loop
#
pretty_status = function(i, total_length){
  to_print = paste(
    sep = "",
    "\ri=",i," of ", total_length,
    " ::: Progress: ", pretty_pct(i/total_length),
    "          "
  )
  
  cat(to_print)
}




# These are som random notes:

# Målgruppetanker: Unger hipsters under uddannelse

# Kan man finde statistik på usikker boligsituation?

# Kort over valgresultater

# Prøv at lave forudsigelse

# Unge kandidater tal?

# Vælgerunders?gelser

# 


