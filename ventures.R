### libraries ----
vt.lib <- "C:/Users/stoiana/win-library"
.libPaths(new = vt.lib)
require(plyr)
require(dplyr)
require(tidyr)
require(purrr)
require(readr)
require(rgears)
require(sp)
require(rgeos)
require(rgdal)
require(readxl)
require(tidyverse)
require(lubridate)
require(tidyquant)
require(shiny)
# environment cleanup
rm(list = ls()); gc()

### directories ----
dir.data <- "C:/R/gambling/data/raw"
dir.output <- "C:/R/gambling/data"
dir.fn <- "C:/R/gambling/R"
dir.tools <- "C:/R/gambling/tools"
dir.geo <- "C:/R/gambling/data/geo"
dir.bound <- "C:/R/gambling/data/geo/ESRI_Shapefile_2016_Digital_Boundaries_Generalised_Clipped"
dir.stats <- "C:/R/gambling/data/stats"
### utility functions ----
source(file.path(dir.fn, "utility functions.R"))
source(file.path(dir.fn, "bbc_style.R"))



df.society_list <- read.csv(file.path(dir.output,"SOCIETY_NAME.csv"),stringsAsFactors = FALSE) %>%
   as.tibble() %>% 
   mutate(VENUE_NAME = toupper(str_trim(VENUE_NAME))) %>% 
   mutate(VENUE_PHYSICAL_ADDRESS = toupper(str_trim(VENUE_PHYSICAL_ADDRESS))) %>% 
   mutate(TA = toupper(str_trim(TERRITORIAL_AUTHORITY))) %>% 
   mutate(TA = gsub("district", "District", TA)) %>% 
   mutate(TA = gsub("city", "City", TA)) %>% 
   mutate(TA = tolower(TA)) %>% 
   select(-TERRITORIAL_AUTHORITY)
   

df.List_of_Venues <- read.csv(file.path(dir.output,"List_of_Venues.csv"),stringsAsFactors = FALSE) %>% 
   as.tibble() %>% 
   change_names(from = "ï..TA",to = "TA") %>% 
   mutate(DATE = ymd(DATE)) %>% 
   unique() %>% 
   filter(TA != "average gmp per venue for quarter to september 2010") %>% 
   arrange(DATE) %>% 
   group_by(TA,AREA_CLUSTER_OF_AREAS,VENUE_NAME,VENUE_NUMBER) %>% 
   mutate(INDEX = 1:n())
   

# average gmp per venue for quarter to september 2010
# A tibble: 1,539 x 1 select(VENUE_NUMBER) %>% unique()
# A tibble: 2,091 x 1 select(VENUE_NAME) %>% unique()
# A tibble: 1,576 x 3 select(VENUE_NUMBER,DATE,INDEX) %>%group_by(VENUE_NUMBER) %>%  filter(INDEX == max(INDEX))
# A tibble: 37 x 3 select(VENUE_NUMBER,DATE,INDEX) %>% group_by(VENUE_NUMBER) %>%  filter(INDEX !=1)


db.complete_venues <- df.List_of_Venues %>%
   arrange(DATE) %>% 
   group_by(VENUE_NUMBER) %>% 
   mutate(INDEX = 1:n()) %>% 
   filter(INDEX == max(INDEX)) %>% 
   select(-INDEX) %>% 
   ungroup() %>% 
   mutate(VENUE_NAME = toupper(str_trim(VENUE_NAME)))
   
vt.max_date <- db.complete_venues %>% select(DATE) %>% filter(DATE == max(DATE)) %>% unique() %>% .[["DATE"]] %>% as.character()

df.complete_names_venue <- df.List_of_Venues %>% 
   unique() %>% 
   arrange(DATE) %>%
   group_by(VENUE_NUMBER,VENUE_NAME) %>% 
   mutate(MIN = min(DATE),
          MAX = max(DATE)) %>% 
   select(VENUE_NUMBER,VENUE_NAME,MIN,MAX) %>% 
   mutate(INT = interval(MIN,MAX)) %>% 
   mutate(DUR = round(as.numeric(duration(INT),"years"),digits = 1)) %>% 
   select(VENUE_NUMBER,VENUE_NAME,DATE_MIN = MIN, DATE_MAX = MAX,DUR) %>% 
   unique() %>% 
   mutate(EXIST = DATE_MAX == vt.max_date)
          



df.current_venues <- db.complete_venues %>% 
   filter(DATE == vt.max_date) %>% 
   mutate(VENUE_NAME = case_when(VENUE_NAME == "LEROYÂ€™S SALOON"         ~ "LEROY’S SALOON",
                                 VENUE_NAME == "JACK DUSTYÂ€™S ALE HOUSE" ~ "JACK DUSTY’S ALE HOUSE",
                                 VENUE_NAME == "WELLINGTON SPORTS CAFÃ©"  ~ "WELLINGTON SPORTS CAFÉ",
                                 VENUE_NAME == "BRIDIEÂ€™S BAR & BISTRO"  ~ "BRIDIE’S BAR & BISTRO",
                                 TRUE ~ VENUE_NAME)) %>% 
   arrange(VENUE_NAME) %>% 
   bind_cols(df.society_list %>% 
                select(COUNCIL_TA = TA,SOC_VENUE_NAME = VENUE_NAME, everything()) %>% 
                arrange(SOC_VENUE_NAME)) %>% 
   mutate(JOIN = paste0(VENUE_NUMBER,"_",VENUE_NAME)) %>% 
   left_join(df.complete_names_venue %>% select(-VENUE_NAME),by = "VENUE_NUMBER") %>% 
   select(-JOIN) %>% 
   select( "TA",
           "AREA_CLUSTER_OF_AREAS",
           "VENUE_NUMBER",
           VENUE_NAME= "VENUE_NAME.x",
           "COUNCIL_TA",
           "SOCIETY_NAME",
           "VENUE_PHYSICAL_ADDRESS",
           "COUNT_OF_EGM",
           "DATE_INITIAL" = "DATE_MIN",
           "DATE_CURRENT" = "DATE_MAX",
           "DURATION" = "DUR",
           "EXIST") %>% 
   filter(DATE_CURRENT == vt.max_date)


#Output

df.current_venues %>% saveRDS(file.path(dir.output,"current_venues.RData"))
