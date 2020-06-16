#' Function to get functional group info for an Aphia ID

get_worms_fgrp <- function(AphiaID){
  
  #' First, test if the species has attribute data
  attr_dat <- try(wm_attr_data(
    AphiaID, include_inherited = TRUE), silent = TRUE)
  
  #' set up out as null for later use
  out <- NULL
  
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
          ) %>% 
          select(-nth_stage_val)
        
       #' create the output to return
       out <- tibble(AphiaID = as.numeric(life_stage$AphiaID),
         stage = life_stage$stage, fun_grp = life_stage$measurementValue)
      
      } else{
        #' If no life stage info, assume stage is adult
        out <- tibble(AphiaID = as.numeric(fg_dat$AphiaID),
          stage = "adult", fun_grp = fg_dat$measurementValue)
       #' Deal with cases where multiple records are returned (i.e. 2 or more adult fun_grps)
        if(nrow(out) > 1){
          out <- out %>% group_by(stage) %>% mutate(nth_stage_val = 1:n()) %>% 
            ungroup() %>% 
            mutate(stage = case_when(
              nth_stage_val == 1 ~ stage,
              TRUE ~ paste(stage, nth_stage_val, sep = "_")
              )
              ) %>% 
          select(-nth_stage_val)
        } 
      } 
    }
    
   #' add Pisces, from paraphyletic group: this takes priority over the above
    #' (e.g. class something as Pisces if it is a fish even if it is also listed as benthos)
    if ("Paraphyletic group" %in% attr_dat$measurementType) {
      if(first(
        attr_dat$measurementValue[attr_dat$measurementType == "Paraphyletic group"] == "Pisces")){
        out <- tibble(AphiaID = AphiaID, stage = "adult",
                      fun_grp = first(attr_dat$measurementValue[
                        attr_dat$measurementType == "Paraphyletic group"]))
        }
      }
    }
  
#' check taxonomy for other groups
    if(is.null(out)){
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
