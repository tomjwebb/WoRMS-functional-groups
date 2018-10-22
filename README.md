# WoRMS-functional-groups
Use the `worrms` package to access and format functional group data for marine species

The function `get_worms_fgrp` is designed to access the attributes data stored in the World Register of Marine Species ([WoRMS](https://marinespecies.org)) to get broad functional group information for a marine species identified by its WoRMS Aphia ID. It depends on the [`worrms`](https://github.com/ropensci/worrms) package to access the WoRMS API. You will need to install and load `worrms` before running the functional group function:
```R
# More stable CRAN version
install.packages("worrms")
```
```R
# Development version
devtools::install_github("ropensci/worrms")
```
```R
library("worrms")
```
You will also need to load the [`tidyverse`](https://tidyverse.tidyverse.org) suite of packages:
```R
# Install from CRAN
install.packages("tidyverse")
library(tidyverse)
```
To run for a single species, just use a valid Aphia ID. If you just have a species name, you can get its Aphia ID using the `worrms::wm_name2id` function, e.g. for the common mussel, *Mytilus edulis*:
```R
wm_name2id("Mytilus edulis")
#> [1] 140480
```
Then run the `get_worms_fgrp` function:
, e.g.
```R
get_worms_fgrp(AphiaID = 100803)
#> # A tibble: 1 x 3
#>   AphiaID stage fun_grp
#>     <dbl> <chr> <chr>  
#> 1  140480 adult benthos
```
You can also run the funtion over a list of species. This code assumes that the species Aphia IDs are included as a variable names `AphiaID` in a data frame. First create an example data frame, which includes species from various functional groups, as well as one where there is no relevant attribute data in WoRMS, to show how the function deals with missing data:
```R
species <- tibble(AphiaID = c(584932, 107381, 100373, 145724, 126436, 137131, 463089, 104016))
```
Then to run the function for each species, and to tidy the results so that each species occupies a single row, with separate columns for the functional groups of different life stages, if relevant:
```R
spp_attr <- species %>%
  group_by(AphiaID) %>%
  do(get_worms_fgrp(AphiaID = .$AphiaID)) %>%
    mutate(functional_group = case_when(
      str_detect(fun_grp, ">") ~ tolower(word(fun_grp, -1)),
      fun_grp == "Pisces" ~ "fish",
      TRUE ~ tolower(fun_grp)
  )) %>%
  dplyr::select(-fun_grp) %>%
  spread(stage, functional_group)
```
This produces the following output:
```R
spp_attr
#> # A tibble: 7 x 3
#> # Groups:   AphiaID [7]
#>   AphiaID adult       larva      
#>     <dbl> <chr>       <chr>      
#> 1  100373 <NA>        <NA>       
#> 2  104016 algae       <NA>
#> 3  107381 benthos     zooplankton
#> 4  126436 fish        <NA>       
#> 5  137131 birds       <NA>       
#> 6  145724 macroalgae  <NA>       
#> 7  463089 benthos     <NA>       
#> 8  584932 zooplankton zooplankton
```
At present, the functional groups returned are those stored as 'functional group' in WoRMS attributes (e.g. benthos, zooplankton, phytoplankton - for details see http://www.marinespecies.org/traits/) - as well as certain paraphyletic groups (fish - everything in WoRMS [superclass Pisces](http://www.marinespecies.org/aphia.php?p=taxdetails&id=11676), macroalgae) and taxonomic groups (birds, mammals, reptiles).
