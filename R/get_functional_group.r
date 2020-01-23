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
      children <- fg_dat$children
      
      if(max(lengths(children)) > 0 ){
        #' Extract the life stage information from the $children field
        life_stage <- children %>% bind_rows() %>%
          dplyr::select(measurementValue) %>%
          rename(stage = measurementValue)
        #' add in rows for instances missing children
        idx <- which(lengths(children) == 0)
        if(length(idx) > 0){
          life_stage_null <- tibble(stage = rep(as.character(NA), length(children)))
          idy <- (1:length(children))[-idx]
          life_stage_null[idy,] <- life_stage
          life_stage <- life_stage_null
        }
        life_stage <- life_stage %>% bind_cols(., fg_dat)
        
        #' deal with cases where multiple records are returned for the same life stage:
        #' add a suffix to subsequent identical stages (adult_2, etc.)        
        life_stage <- life_stage %>% group_by(stage) %>% mutate(nth_stage_val = 1:n()) %>% 
          ungroup() %>% 
          mutate(stage = case_when(
            nth_stage_val == 1 ~ stage,
            TRUE ~ paste(stage, nth_stage_val, sep = "_")
          )
          )
        
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
    taxo_dat <- try(wm_classification(AphiaID), silent = TRUE)
    if(identical(class(taxo_dat), "try-error")){
      fg <- as.character(NA)
    } else {
      fg <- case_when(
        "Aves" %in% taxo_dat$scientificname ~ "birds",
        "Mammalia" %in% taxo_dat$scientificname ~ "mammals",
        "Reptilia" %in% taxo_dat$scientificname ~ "reptiles",
        TRUE ~ as.character(NA)
        )
    }
    out <- tibble(AphiaID = AphiaID, stage = "adult", fun_grp = fg)
  }
  
  #' what if there are duplicate rows?
  out <- out[!duplicated(out), ]
  
  #' do some tidying of the output: tidy up functional_group text
  #' and spread to give one column per life stage
  out <- out %>% mutate(functional_group = case_when(
    str_detect(fun_grp, ">") ~ tolower(word(fun_grp, -1)),
    fun_grp == "Pisces" ~ "fish",
    TRUE ~ tolower(fun_grp)
  )) %>%
  dplyr::select(-fun_grp) %>%
  spread(stage, functional_group)
 
  #' output the fg data
  out
  
}
