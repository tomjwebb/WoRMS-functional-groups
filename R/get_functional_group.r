#' Function to get functional group info for an Aphia ID

get_worms_fgrp <- function(AphiaID){
  
  #' First, test if the species has attribute data
  attr_dat <- try(wm_attr_data(
    AphiaID, include_inherited = TRUE), silent = TRUE)
  
  if(!identical(class(attr_dat), "try-error")){
    #' if attribute data exists, test if functional group is there
    if("Functional group" %in% attr_dat$measurementType){
      fg_dat <- attr_dat %>% filter(measurementType == "Functional group")
      
      #' Insert if statement here about $children empty
      #' assign the $children - so that it can be used 
      children <- data.frame(fg_dat$children)
      
      if(length(children) > 0 ){
        #' Extract the life stage information from the $children field
        life_stage <- bind_rows(fg_dat$children) %>%
          dplyr::select(measurementValue) %>%
          rename(stage = measurementValue) %>% bind_cols(., fg_dat)
        
       #' create the output to return
       out <- tibble(AphiaID = as.numeric(life_stage$AphiaID),
         stage = life_stage$stage, fun_grp = life_stage$measurementValue)
      
      } else{
        #' If no life stage info, assume stage is adult
        out <- tibble(AphiaID = as.numeric(fg_dat$AphiaID),
          stage = "adult", fun_grp = fg_dat$measurementValue)
      }
      
    } else if ("Paraphyletic group" %in% attr_dat$measurementType) {
    #' get paraphyletic group info if available  
      out <- tibble(AphiaID = AphiaID, stage = "adult",
        fun_grp = first(attr_dat$measurementValue[
          attr_dat$measurementType == "Paraphyletic group"])
          )
      
    } else {
      #' check taxonomy for other groups
      taxo_dat <- wm_classification(AphiaID)
      fg <- case_when(
        "Aves" %in% taxo_dat$scientificname ~ "birds",
        "Mammalia" %in% taxo_dat$scientificname ~ "mammals",
        "Reptilia" %in% taxo_dat$scientificname ~ "reptiles",
        TRUE ~ as.character(NA)
      )
      out <- tibble(AphiaID = AphiaID, stage = "adult", fun_grp = fg)
    }
  } else {
    #' check taxonomy for other groups
    taxo_dat <- wm_classification(AphiaID)
    fg <- case_when(
      "Aves" %in% taxo_dat$scientificname ~ "birds",
      "Mammalia" %in% taxo_dat$scientificname ~ "mammals",
      "Reptilia" %in% taxo_dat$scientificname ~ "reptiles",
      TRUE ~ as.character(NA)
      )
      
    out <- tibble(AphiaID = AphiaID, stage = "adult", fun_grp = fg)
  }
    
  #' output the fg data
  out
  
}
