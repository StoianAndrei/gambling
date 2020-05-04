### libraries ----
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
# environment cleanup
rm(list = ls()); gc()

### directories ----
dir.data <- "data/raw"
dir.output <- "data"
dir.fn <- "R"
dir.geo <- "data/geo"
dir.bound <- "data/geo/ESRI_Shapefile_2016_Digital_Boundaries_Generalised_Clipped"
dir.stats <- "data/stats"
### utility functions ----
source(file.path(dir.fn, "utility functions.R"))

### data ----
# mutate(TIMESTAMP = as.POSIXct(strptime(TIMESTAMP, format = "%m-%d-%Y %H:%M"), tz = "Etc/GMT+12"))

## First part is to get our data source in shape. Name and date extracted from each Name. 

df.list.excel.xls <- list.files(dir.data,pattern = ".xls") %>% 
      as_tibble() %>% 
      mutate(LIST = value) %>% 
      separate(value,into = c("rest","date"),sep = "\\-") %>% 
      select(date,LIST) %>% 
   ## We observe that there is two versions of the data one is xls and one is newer version xlsx
      mutate(date = str_remove(date,pattern = ".xls")) %>% 
      mutate(FLAG_X = str_detect(date,pattern = "x")) %>% 
   ## Filter out the newer version so that we deal with it on its own. 
      filter(!FLAG_X) %>% 
   ## Extract YEAR and MONTH from the List Nama 
      mutate(YEAR = str_extract(date,pattern = "\\d++")) %>% 
      mutate(MONTH = str_extract(date,pattern = "\\w+")) %>% 
      mutate(MONTH = str_remove(MONTH,pattern = YEAR)) %>% 
   ## Create date 
      mutate(DATE = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
   ## Only select LIST Name and DATE
      select(LIST,DATE) 

## Explore what is the structure for the xls file. How many tabs and what is the name used for each sheet
vt.sheets <- readxl::excel_sheets(file.path(dir.data,"DetailedGMPdata-June2010.xls"))     


## Create two empty list so we can store our clean result. 
ls.EGM_and_GMP_at_suburb_level <- list()
ls.List_of_Venues <- list()


for (i in 1:n_distinct(df.list.excel.xls$LIST)) {
      ls.EGM_and_GMP_at_suburb_level[[i]] <- readxl::read_xls(file.path(dir.data,df.list.excel.xls$LIST[i]),sheet = vt.sheets[1],skip = 3,col_names = TRUE) %>%
            mutate(DATE = df.list.excel.xls$DATE[i]) %>%
            set_sensible_names() %>%
            fill(DISTRICT, .direction = "down")

}


for (i in 1:n_distinct(df.list.excel.xls$LIST)) {
      ls.List_of_Venues[[i]] <- readxl::read_xls(file.path(dir.data,df.list.excel.xls$LIST[6]),sheet = vt.sheets[2],skip = 2,col_names = TRUE) %>% 
            mutate(DATE = df.list.excel.xls$DATE[i]) %>% 
            set_sensible_names() %>% 
            fill(DISTRICT, .direction = "down")
     
}


### low bottom lid in auckland will reduce the number of venues.
## if you close your club another person will not open the 

### more machines is more average money or is it where it is? 

df.EGM_and_GMP_at_suburb_level <- bind_rows(ls.EGM_and_GMP_at_suburb_level) %>% 
      mutate(TOTAL_GMP_NZD = `TOTAL_GMP_$`) %>% 
      select(-`TOTAL_GMP_$`) %>% 
      select(DISTRICT,AREA_CLUSTER_OF_AREAS,TOTAL_NUMBER_OF_VENUES,TOTAL_NUMBER_OF_EGMS,TOTAL_GMP_NZD,DATE)
      


df.EGM_and_GMP_at_suburb_level[df.EGM_and_GMP_at_suburb_level == 0] <- NA

df.EGM_and_GMP_at_suburb_level_GT1 <- df.EGM_and_GMP_at_suburb_level %>%
   filter(str_detect(string = DISTRICT,pattern = "Grand Total"))
   # filter(DISTRICT == "Grand Total")
   


df.EGM_and_GMP_at_suburb_level_pp <- df.EGM_and_GMP_at_suburb_level %>% 
   fill(DISTRICT, .direction = "down") %>% 
   fill(AREA_CLUSTER_OF_AREAS  , .direction = "down") %>%
   filter(!is.na(DISTRICT)) %>% 
   filter(!is.na(DATE)) %>% 
   filter(!is.na(TOTAL_NUMBER_OF_VENUES)) %>% 
   filter(!str_detect(DISTRICT,"Average")) %>% 
   filter(!str_detect(DISTRICT,"Grand Total")) %>% 
   filter(!str_detect(DISTRICT,"(blank)")) %>% 
   group_by(DISTRICT,AREA_CLUSTER_OF_AREAS) %>% 
   mutate(INDEX = 1:n_distinct(DATE)) %>%
   arrange(desc(INDEX)) 

library(tidytext)

df.total_area <- df.EGM_and_GMP_at_suburb_level_pp$AREA_CLUSTER_OF_AREAS %>% unique()
df.total <- df.EGM_and_GMP_at_suburb_level_pp$DATE %>% arrange(DATE) %>% unique()

df.complete <- expand.grid(AREA_CLUSTER_OF_AREAS = df.total_area,AREA_CLUSTER_OF_AREAS1 = df.total_area,stringsAsFactors = FALSE) %>%
   as_tibble() %>% 
   unique() %>% 
   unnest_tokens(word,AREA_CLUSTER_OF_AREAS1) %>%
   mutate(AREA_LOWER = str_to_lower(AREA_CLUSTER_OF_AREAS)) %>% 
   mutate(FLAG = str_detect(AREA_LOWER,word)) %>% 
   filter(FLAG) %>% 
   unique() %>% 
   select(AREA_CLUSTER_OF_AREAS,word)

vt.unique_suburb <- df.EGM_and_GMP_at_suburb_level_pp %>%
   filter(INDEX==8) %>% 
   select(AREA_CLUSTER_OF_AREAS) %>% 
   unique() %>% 
   pull()


df.EGM_and_GMP_at_suburb_level_pp %>% 
   filter(!AREA_CLUSTER_OF_AREAS %in% vt.unique_suburb) %>% 
   view()

df.complete %>% 
   mutate(AREA = str_to_lower(AREA_CLUSTER_OF_AREAS)) %>% 
   
df.EGM_and_GMP_at_suburb_level_pp %>% bind_rows(df.EGM_and_GMP_at_suburb_level_pp2) %>% view()

df.clean <- df.EGM_and_GMP_at_suburb_level_pp %>%
   pivot_wider(id_cols = AREA_CLUSTER_OF_AREAS,names_from = INDEX,values_from = DATE)
   

df.EGM_and_GMP_at_suburb_level_pp %>% left_join(df.complete %>% select(AREA_CLUSTER_OF_AREAS, word )) %>% 
   filter(word == "johnsonville")

   mutate(flag = str_detect(AREA_CLUSTER_OF_AREAS,pattern = df.total_area))
   


df.List_of_Venues <- bind_rows(ls.List_of_Venues) %>% 
      filter(!is.na(VENUE_NUMBER))

## did any onf the venues change name what about VENUE_NUMBER is that the UNIQUE ID? if so when did they change? 


df.List_of_Venues[df.List_of_Venues == 0] <- NA

df.List_of_Venues_pp <- df.List_of_Venues %>%
      fill(DISTRICT, .direction = "down") %>% 
      fill(AREA_CLUSTER_OF_AREAS  , .direction = "down") %>% 
      fill(VENUE_NAME , .direction = "down") %>% 
      group_by(DISTRICT,AREA_CLUSTER_OF_AREAS,VENUE_NAME,VENUE_NUMBER) %>% 
      mutate(INDEX = 1:n_distinct(DATE)) %>% 
      filter(!is.na(VENUE_NUMBER)) %>% 
      arrange(desc(INDEX))

## save the results 

df.EGM_and_GMP_at_suburb_level_pp %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_at_suburb_level_pp.csv"))
df.List_of_Venues_pp %>% write_excel_csv(file.path(dir.output,"List_of_Venues_pp1.csv"))
df.EGM_and_GMP_at_suburb_level_GT1 %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_GT1.csv"))




##xlsx

df.list.excel <- list.files(dir.data,pattern = "xlsx") %>% 
      as_tibble() %>% 
      mutate(LIST = value) %>% 
      separate(value,into = c("rest","date"),sep = "\\-") %>% 
      select(date,LIST) %>% 
      mutate(date = str_remove(date,pattern = ".xlsx")) %>% 
      mutate(YEAR = str_extract(date,pattern = "\\d++")) %>% 
      mutate(MONTH = str_extract(date,pattern = "\\w+")) %>% 
      mutate(MONTH = str_remove(MONTH,pattern = YEAR)) %>% 
      mutate(DATE = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
      select(LIST,DATE)


vt.sheets <- readxl::excel_sheets(file.path(dir.data,"DetailedGMPdata-December2011.xlsx"))     

ls.EGM_and_GMP_at_suburb_level <- list()
ls.List_of_Venues <- list()


for (i in 1:n_distinct(df.list.excel$LIST)) {
      ls.EGM_and_GMP_at_suburb_level[[i]] <- readxl::read_xlsx(file.path(dir.data,df.list.excel$LIST[i]),sheet = vt.sheets[1],skip = 3,col_names = TRUE) %>% 
            mutate(DATE = df.list.excel$DATE[i]) %>% 
            set_sensible_names() %>% 
            fill(DISTRICT, .direction = "down")
      
}


for (i in 1:n_distinct(df.list.excel$LIST)) {
      ls.List_of_Venues[[i]] <- readxl::read_xlsx(file.path(dir.data,df.list.excel$LIST[i]),sheet = vt.sheets[2],skip = 2,col_names = TRUE) %>% 
            mutate(DATE = df.list.excel$DATE[i]) %>% 
            set_sensible_names() %>% 
            fill(DISTRICT, .direction = "down")
      
}




### more machines is more average money or is it where it is? 

df.EGM_and_GMP_at_suburb_level <- bind_rows(ls.EGM_and_GMP_at_suburb_level) %>% 
      mutate(TOTAL_GMP_NZD = `TOTAL_GMP_$`) %>% 
      select(-`TOTAL_GMP_$`) %>% 
      select(DISTRICT,AREA_CLUSTER_OF_AREAS,TOTAL_NUMBER_OF_VENUES,TOTAL_NUMBER_OF_EGMS,TOTAL_GMP_NZD,DATE)



df.EGM_and_GMP_at_suburb_level[df.EGM_and_GMP_at_suburb_level == 0] <- NA


df.EGM_and_GMP_at_suburb_level_pp2 <- df.EGM_and_GMP_at_suburb_level %>% 
   fill(DISTRICT, .direction = "down") %>% 
   fill(AREA_CLUSTER_OF_AREAS  , .direction = "down") %>% 
   filter(!is.na(DATE)) %>% 
   filter(!is.na(TOTAL_NUMBER_OF_VENUES)) %>% 
   filter(AREA_CLUSTER_OF_AREAS != "(blank)") %>% 
   filter(AREA_CLUSTER_OF_AREAS != "#N/A") %>% 
   mutate(DISTRICT = toupper(DISTRICT)) %>% 
   mutate(FLAG_GT = str_detect(DISTRICT,pattern = "TOTAL")) %>% 
   filter(!FLAG_GT) %>% 
   as.tibble() %>% 
   add_count(DISTRICT,AREA_CLUSTER_OF_AREAS) %>% 
   select(DISTRICT,AREA_CLUSTER_OF_AREAS,TOTAL_NUMBER_OF_VENUES, TOTAL_NUMBER_OF_EGMS, TOTAL_GMP_NZD, DATE, INDEX =n) %>% 
      # mutate(INDEX = 1:n_distinct(DATE))
   arrange(desc(INDEX)) %>% 
   ungroup()

   



df.EGM_and_GMP_at_suburb_level_GT2 <- df.EGM_and_GMP_at_suburb_level %>%
   mutate(DISTRICT = toupper(DISTRICT)) %>% 
   mutate(FLAG_GT = str_detect(DISTRICT,pattern = "TOTAL")) %>%
   filter(FLAG_GT) %>% 
   mutate(DISTRICT = "GRAND TOTAL") %>% 
   select("DISTRICT","AREA_CLUSTER_OF_AREAS","TOTAL_NUMBER_OF_VENUES","TOTAL_NUMBER_OF_EGMS","TOTAL_GMP_NZD","DATE")
   


## did any onf the venues change name what about VENUE_NUMBER is that the UNIQUE ID? if so when did they change? 


df.List_of_Venues2 <- bind_rows(ls.List_of_Venues) %>% 
   filter(!is.na(VENUE_NUMBER))


df.List_of_Venues2[df.List_of_Venues2 == 0] <- NA

df.List_of_Venues_pp2 <- df.List_of_Venues2 %>%
   fill(DISTRICT, .direction = "down") %>% 
   fill(AREA_CLUSTER_OF_AREAS  , .direction = "down") %>% 
   fill(VENUE_NAME , .direction = "down") %>% 
   filter(!is.na(VENUE_NUMBER)) %>% 
   select( DISTRICT,AREA_CLUSTER_OF_AREAS, VENUE_NAME,VENUE_NUMBER, DATE) %>% 
   add_count(DISTRICT,AREA_CLUSTER_OF_AREAS,VENUE_NAME,VENUE_NUMBER) %>% 
      # group_by(DISTRICT,AREA_CLUSTER_OF_AREAS,VENUE_NAME,VENUE_NUMBER) %>% 
      # mutate(INDEX = 1:n_distinct(DATE)) %>% 
   select( DISTRICT,AREA_CLUSTER_OF_AREAS, VENUE_NAME,VENUE_NUMBER, DATE,   INDEX = n) %>% 
   arrange(desc(INDEX)) 



df.EGM_and_GMP_at_suburb_level_pp2 %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_at_suburb_level_pp2.csv"))
df.List_of_Venues_pp2 %>% write_excel_csv(file.path(dir.output,"List_of_Venues_pp2.csv"))
df.EGM_and_GMP_at_suburb_level_GT2 %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_GT2.csv"))


df.EGM_and_GMP_at_suburb_level <- df.EGM_and_GMP_at_suburb_level_pp %>% 
      bind_rows(df.EGM_and_GMP_at_suburb_level_pp2) %>%
      mutate(TA = tolower(DISTRICT)) %>% 
      ungroup() %>% 
      select(TA, everything(),-DISTRICT) %>% 
   filter(!grepl("local board area", TA)) %>% 
   filter(!grepl("region", TA)) %>% 
   mutate(TA = gsub("district", "District", TA)) %>% 
   mutate(TA = gsub("city", "City", TA)) %>% 
   mutate(TA = gsub("auckland City", "auckland", TA)) %>% 
   mutate(TA = gsub("tauranga District", "tauranga city", TA)) %>% 
   mutate(TA = gsub("wanganui District", "whanganui district", TA)) %>%
   filter(TA != "average gmp per venue for quarter to september 2010") %>% 
   filter(AREA_CLUSTER_OF_AREAS != "Summarised") %>% 
   mutate(TA = gsub("hurunui District", "hurunui district", TA)) %>% 
   mutate(TA = gsub("christchurch City including banks peninsula ward", "christchurch city", TA)) %>% 
   mutate(TA = tolower(TA)) 
   
                   

      
df.List_of_Venues <- df.List_of_Venues_pp %>% bind_rows(df.List_of_Venues_pp2) %>% 
      mutate(TA = tolower(DISTRICT)) %>% 
      ungroup() %>% 
      select(TA, everything(),-DISTRICT)


df.EGM_and_GMP_at_suburb_level %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_at_suburb_level.csv"))
df.List_of_Venues %>% write_excel_csv(file.path(dir.output,"List_of_Venues.csv"))
df.EGM_and_GMP_at_suburb_level_GT1 %>% bind_rows(df.EGM_and_GMP_at_suburb_level_GT2) %>% write_excel_csv(file.path(dir.output,"EGM_and_GMP_GT2.csv"))


# df.raw_pop <- read_csv(file.path(dir.data, "Population Projection.csv"))
# df.raw_pop <- read_csv(file.path(dir.geo, "2018-census-population-age-groups-by-sa22018.csv"))
df.raw_pop <- read_csv(file.path(dir.stats, "TABLECODE7548_Data_bfe525db-c6d6-4d08-bc8d-347d55354e0c.csv"))

spldf.raw_nz <- readOGR(dsn = file.path(dir.bound), layer = "TA2016_GV_Clipped")


df.raw_nz_TA <- spldf.raw_nz@data %>% select(TA = TA2016_NAM) %>% unique() %>% 
   filter(!grepl("local board area", TA)) %>% 
   filter(!grepl("region", TA)) %>% 
   mutate(TA = gsub("district", "District", TA)) %>% 
   mutate(TA = gsub("city", "City", TA)) %>% 
   # mutate(TA = gsub("Wanganui", "Whanganui", TA)) %>% 
   mutate(TA = tolower(TA)) %>% 
   mutate(INDEX_TA = row_number(TA)) %>% 
   arrange(INDEX_TA) %>% 
   as.tibble()

df.EGM_and_GMP_at_suburb_level %>%
   select(TA) %>% 
   anti_join(df.raw_nz_TA %>% select(TA)) %>% unique()
   filter(is.na(TA)) %>% select(TA) %>% unique()
   # filter(is.na(INDEX_TA)) %>% select(TA) %>% unique()


### data processing ----
## population and MD data

df.input_pop <- df.raw_pop %>% 
   change_names(names(df.raw_pop), toupper(names(df.raw_pop))) %>% 
   change_names("YEAR AT 30 JUNE", "YEAR") %>% 
   change_names("AREA", "TA") %>% 
   filter(SEX %in% "Total people, sex") %>%
   # filter(AGE %in% c("Total people, all ages"
   #                   , "45-49 years"
   #                   , "50-54 years"
   #                   , "55-59 years"
   #                   , "60-64 years"
   #                   , "65-69 years"
   #                   , "70-74 years"
   #                   , "75-79 years"
   #                   , "80-84 years")) %>% 
   filter(!TA %in% "Total, New Zealand by territorial authority") %>%
   # filter(YEAR != 2013) %>%
   select( -FLAGS) %>% 
   filter(!grepl("local board area", TA)) %>% 
   filter(!grepl("region", TA)) %>% 
   mutate(TA = gsub("district", "District", TA)) %>% 
   mutate(TA = gsub("city", "City", TA)) %>% 
   # mutate(TA = gsub("Wanganui", "Whanganui", TA)) %>% 
   mutate(TA = tolower(TA))


# df.input_pop <- df.raw_pop %>% 
#       change_names(names(df.raw_pop), toupper(names(df.raw_pop))) %>% 
#       # change_names("YEAR AT 30 JUNE", "YEAR") %>% 
#       change_names("SA22018_V1_NAME", "TA") %>% 
#       select(TA,POP_TOTAL_2018
#              ,AGE__LIFE_CYCLE_GROUPS_UNDER_15_YEARS_2018
#              ,AGE__LIFE_CYCLE_GROUPS_15_29_YEARS_2018
#              ,AGE__LIFE_CYCLE_GROUPS_30_64_YEARS_2018
#              ,AGE__LIFE_CYCLE_GROUPS_65_YEARS_AND_OVER_2018) %>% 
#       # filter(SEX %in% "Total people") %>% 
#       # filter(AGE %in% c("Total people, all ages"
#       #                   , "45-49 years"
#       #                   , "50-54 years"
#       #                   , "55-59 years"
#       #                   , "60-64 years"
#       #                   , "65-69 years"
#       #                   , "70-74 years"
#       #                   , "75-79 years"
#       #                   , "80-84 years")) %>% 
#       # filter(ETHNICITY %in% c("European or Other (including New Zealander)"
#       #                         , "Asian"
#       #                         , "Total New Zealand population")) %>% 
#       filter(!TA %in% "Total, New Zealand by territorial authority") %>%
#       # filter(YEAR != 2013) %>%
#       # select(-SEX, -FLAGS) %>% 
#       filter(!grepl("local board area", TA)) %>% 
#       filter(!grepl("region", TA)) %>% 
#       mutate(TA = gsub("district", "District", TA)) %>% 
#       mutate(TA = gsub("city", "City", TA)) %>% 
#       mutate(TA = gsub("Wanganui", "Whanganui", TA)) %>% 
#       mutate(TA = tolower(TA))
# 



# 
# df.input_national <- df.input_pop %>% 
#       filter(ETHNICITY %in% "Total New Zealand population") %>%
#       filter(AGE %in% "Total people, all ages") %>% 
#       change_names("VALUE", "BASE") %>% 
#       select(-ETHNICITY, -AGE)
# 
# 
# df.db_input <- df.input_pop %>%
#       filter(!ETHNICITY %in% "Total New Zealand population") %>%
#       filter(!AGE %in% "Total people, all ages") %>%
#       left_join(df.raw_MD) %>% 
#       mutate(MD = VALUE * PROP_MD) %>% 
#       group_by(YEAR, TA, PROJECTION) %>% 
#       summarise(MD = sum(MD, na.rm = TRUE),
#                 VALUE = sum(VALUE, na.rm = TRUE)) %>%
#       left_join(df.input_national) %>% 
#       mutate(MD_STD = MD / BASE)

## shapefiles
spldf.db_nz <- spldf.raw_nz %>% 
      spTransform(CRS("+init=epsg:4326"))

spldf.db_nz@data <- spldf.db_nz@data %>% 
      select(TA2016_NAM) %>% 
      change_names("TA2016_NAM", "TA") %>% 
      mutate(TA = tolower(TA))

# "chatham islands territory"
spldf.db_nz <- subset(spldf.db_nz, !TA %in% c("area outside territorial authority"))

spldf.simplify <- gSimplify(spldf.db_nz, topologyPreserve = TRUE, tol = 0.01)
spldf.db_nz@polygons <- spldf.simplify@polygons



### export ----
# save(df.db_input, df.db_meta, spldf.db_nz, file = file.path(dir.output, "dashboard input.rda"))



df.db_input <- df.EGM_and_GMP_at_suburb_level %>% 
   mutate(YEAR = year(DATE)) %>% 
   arrange(YEAR) %>% 
   mutate(PROJECTION = case_when(
      TOTAL_GMP_NZD < median(TOTAL_GMP_NZD) ~ "Low",
      TOTAL_GMP_NZD == median(TOTAL_GMP_NZD) ~ "Medium",
      TOTAL_GMP_NZD > median(TOTAL_GMP_NZD) ~ "High",
      TRUE ~ "Other"
   )) %>% 
   mutate(
      MD = TOTAL_GMP_NZD/100
      ,VALUE = TOTAL_GMP_NZD/10
      ,BASE = TOTAL_GMP_NZD 
      ,MD_STD = sd(TOTAL_GMP_NZD)
   ) %>% 
   select(YEAR
          ,TA
          ,PROJECTION
          ,MD
          ,VALUE
          ,BASE
          ,MD_STD)


save(df.db_input,df.EGM_and_GMP_at_suburb_level, df.List_of_Venues, spldf.db_nz, file = file.path(dir.output, "dashboard_gambling input.rda"))



### southern and northen society is owning the blue rugby club  who ows the pub. 

