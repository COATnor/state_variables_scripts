#### ------------------------------------------------------------------------------------------------------------ ####
### CALCULATE STATE VARIABLE - EXAMPLE
### Date: 17.11.2022
### Author: Hanna Böhner
#### ------------------------------------------------------------------------------------------------------------ ####


## this script is an example for calculating a the state variable

## the script downloads the dataset 'V_air_temperature_snowbed' from the COAT dataportal
## calculates the state variable and plots snowmelt of new years to visually check if the calculated date looks correct
## creates a new version of the state variable
## uploads the state variable to the COAT dataportal

## The part of the script that calculates the state variable differs for every dataset and has to be adapted

## the development version of the ckanr package has to be installed (remotes::install_github("ropensci/ckanr"))


## ---------------------------------- ##
## SETUP
## ---------------------------------- ##

## clear workspace
rm(list = ls())

## load libraries, missing packages will be installed
if (!require("remotes")) install.packages("remotes")
if (!require("ckanr")) remotes::install_github("ropensci/ckanr"); library("ckanr")
if (!require("tidyverse")) install.packages("tidyverse"); library("tidyverse")
if (!require("lubridate")) install.packages("tidyverse"); library("lubridate")
if (!require("arrow")) install.packages("tidyverse"); library("arrow")


## setup the connection to the data portal
COAT_url <- "https://data.coat.no/" # write here the url to the COAT data portal
COAT_key <- Sys.getenv("API_coat") # write here your API key if you are a registered user, continue without API key if you are not registered
# the API can be found on you page on the COAT data portal (log in and click on your name in the upper right corner of the page)
# The use of an API key allows the user to access also non-public data

ckanr_setup(url = COAT_url, key = COAT_key) # set up the ckanr-API

## download functions from github
source("https://github.com/COATnor/data_management_scripts/blob/master/download_parquet_files_from_coat_data_portal.R?raw=TRUE")
source("https://github.com/COATnor/data_preprocessing_scripts/blob/master/function_preprocessing_image_classifications_small_mamma_camara_traps.R?raw=TRUE")

## ---------------------------------- ##
## DEFINE FUNCTIONS
## ---------------------------------- ##

download_coat_data <- function(COAT_key = COAT_key, 
                               name = name, 
                               version = version,
                               filenames = filenames,
                               store = "session",
                               out.dir = out.dir) {
  
  ## setup the connection to the data portal
  ckanr_setup(url =  "https://data.coat.no", key = COAT_key)
  
  ## search for the dataset
  pkg <- package_search(q = list(paste("name:", name, sep = "")), fq = list(paste("version:", version, sep = "")), include_private = TRUE)$results[[1]]
  urls <- pkg$resources %>% sapply("[[", "url") # get the urls to the files included in the dataset
  filenames_dataset <- pkg$resources %>% sapply("[[", "name") # get the filenames
  
  ## check if all files are available
  if (!all(filenames %in% filenames_dataset)){
    print("Error: files not found")
    break
  }
  
  ## urls of filenames that should be downloaded
  chrono <- order(filenames_dataset)
  filenames_dataset <- filenames_dataset[chrono]
  urls <- urls[chrono]
  
  urls2 <- urls[which(filenames_dataset %in% filenames)]
  
  ## download all files 
  mylist <- c() # empty object for the files
  
  for (i in 1:length(filenames)) {
    mylist[[i]] <- ckan_fetch(urls2[i],
                              store = store,
                              path = paste(out.dir, name, filenames[i], sep = "/"),
                              sep = ";",
                              header = TRUE,
                              format = "txt"
    )
  }
  
  #myfile <- do.call(rbind, mylist)
  
  return(mylist)
}




## ---------------------------------- ##
## DOWNLOAD DATA
## ---------------------------------- ##

## define year(s) for which the data should be downloaded
years_ko <- 2016:2024  # komagdalen lemming blocks
years_vj <- 2019:2024  # vestre jakobselv lemming blocks
years_rv <- 2021:2024  # river valleys

## specify filenames
names_class_lb <- c(paste0("V_rodents_cameratraps_image_classification_lemming_blocks_komagdalen_", years_ko, ".parquet"),
                    paste0("V_rodents_cameratraps_image_classification_lemming_blocks_vestre_jakobselv_", years_vj, ".parquet"))

names_meta_lb <- c(paste0("V_rodents_cameratraps_image_metadata_lemming_blocks_komagdalen_", years_ko, ".parquet"),
                    paste0("V_rodents_cameratraps_image_metadata_lemming_blocks_vestre_jakobselv_", years_vj, ".parquet"))

names_class_rv <- c(paste0("V_rodents_cameratraps_image_classification_intensive_quadrats_komagdalen_", years_rv, ".parquet"),
                    paste0("V_rodents_cameratraps_image_classification_intensive_quadrats_vestre_jakobselv_", years_rv, ".parquet"))

names_meta_rv <- c(paste0("V_rodents_cameratraps_image_metadata_intensive_quadrats_komagdalen_", years_rv, ".parquet"),
                   paste0("V_rodents_cameratraps_image_metadata_intensive_quadrats_vestre_jakobselv_", years_rv, ".parquet"))


## download image classification and metadata for lemming blocks
classification_lb_list <- download_coat_parquet(COAT_key = Sys.getenv("api_COAT"),
                                         name = "v_rodents_cameratraps_image_classification_lemming_blocks_v4",
                                         version = 4, 
                                         filenames = names_class_lb) 

metadata_lb_list <- download_coat_parquet(COAT_key = Sys.getenv("api_COAT"),
                                             name = "v_rodents_cameratraps_image_metadata_lemming_blocks_v4",
                                             version = 4, 
                                             filenames = names_meta_lb) 


## download image classification and metadata for river valleys
classification_rv_list <- download_coat_parquet(COAT_key = Sys.getenv("api_COAT"),
                                             name = "v_rodents_cameratraps_image_classification_intensive_quadrats_v3",
                                             version = 3, 
                                             filenames = names_class_rv) 

metadata_rv_list <- download_coat_parquet(COAT_key = Sys.getenv("api_COAT"),
                                       name = "v_rodents_cameratraps_image_metadata_intensive_quadrats_v3",
                                       version = 3, 
                                       filenames = names_meta_rv) 



## ---------------------------------- ##
## PREPROCESS DATA 
## ---------------------------------- ##

## lemming blocks
lb_processed <- c()

for (i in 1:length(classification_lb_list)) {
  lb_processed[[i]] <- preprocess_classifications(dat_name = classification_lb_list[[i]], meta_name = metadata_lb_list[[i]], is.dir = FALSE)
  lb_processed[[i]]$t_year <- sub(".*_(\\d{4})\\.parquet$", "\\1", names_class_lb[i])  # add year
}

## ko 2024 -> 2 missing images -> all classes NA -> ok
## vj 2023 -> image with tow animals -> ok

lb_dat <- do.call(rbind, lb_processed)


## river valleys
rv_processed <- c()

for (i in 1:length(classification_rv_list)) {
  rv_processed[[i]] <- preprocess_classifications(dat_name = classification_rv_list[[i]], meta_name = metadata_rv_list[[i]], is.dir = FALSE)
  rv_processed[[i]]$t_year <- sub(".*_(\\d{4})\\.parquet$", "\\1", names_class_rv[i])  # add year
}

rv_dat <- do.call(rbind, rv_processed)

## ko 2022 -> needs checking because of one time lapse image with a mink -> is ok


## combine both files
dat_all <- rbind(lb_dat, rv_dat)



## ---------------------------------- ##
## CALCULATE STATE VARIABLE
## ---------------------------------- ##

years <- unique(dat_all$t_year) 
state_var_names <- c()

for (i in 1:length(years)) {
  
  ## filter data per year
  dat_year <- filter(dat_all, t_year == years[i])
  
  ## calculate number of passings per week
  dat_week <- dat_year %>% mutate(t_date = ymd(t_date)) %>% 
    filter(!is.na(t_date)) %>% 
    mutate(t_week = week(ymd(t_date)), t_year = year(t_date)) %>% 
    mutate(t_year_week = paste(t_year, t_week, sep = "_")) %>% 
    mutate(t_year_week = factor(t_year_week, levels = unique(t_year_week))) %>% 
    dplyr::group_by(sn_region, sn_locality, sc_type_of_sites_ecological, sn_site, t_year, t_week, t_year_week, v_class_id) %>% 
    dplyr::summarise(v_abundance = sum(v_presence, na.rm = TRUE)) %>% 
    arrange(sn_site, t_year_week) %>% 
    ungroup() %>% 
    select(-t_year_week)
  
  ## keep only mustelids
  dat_mustelid <- dat_week %>%  filter(v_class_id %in% c("mus_erm", "mus_niv"))
  
  ## save the file to a temporary directory (necessary for uploading it)
  state_var_names[i] <- paste0("V37_special_predators_mustelid_abundance_", years[i], ".txt")
  write.table(dat_mustelid, paste(tempdir(), state_var_names[i], sep = "/"), row.names = FALSE, sep = ";")
  print(paste("state variable calculated and saved to temporary directory:", state_var_names[i]))
  
  ## 
  #out.dir <- "C:/Users/hbo042/Box/COAT/Modules/Small rodent module/state_variable_developement/mustelide_abundance/data"
  #new_name <- paste0("state_variable_mustelid_abundance_", years[i], ".txt")
  #write.table(dat_mustelid, paste(out.dir, new_name, sep = "/"), row.names = FALSE, sep = ";")
  
  
}


## ---------------------------------- ##
## CREATE A NEW VERSION OF THE STATE VARIABLE
## ---------------------------------- ##

## you can either create a new version of the state variable or add the data to a already existing state variable (then you can skip this part)

## serach for your dataset
state_name <- "v37_special_predators_mustelid_abundance_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name # check the name

## modify metadata of the state variable
name_new <- "v37_special_predators_mustelid_abundance_v2" # write here the name of the new version (for example change v1 to v2)
version_new <- "2" # write here the new version
end_new <- "2021-08-31" # write here the (new) end date of the dataset

pkg_state$datasets # check with which dataset the current version of the state variable is associated
datasets_new <- "v_air_temperature_snowbed_v2" # write here the name (inlcuding the version) of the dataset the should be associated with the new version of the state variable

# These are the typlical modifications when creating a new version of a state variable before adding data of another year
# other modification can be made if necessary

# modify tags (necessary to avoid a validation error)
new_tags <- c()
for (i in 1:length(pkg$tags)) {
  new_tags[[i]] <- list(name = pkg$tags[[i]]$name)
}

## create the new version
package_create(
  name = name_new,
  title = pkg_state$title,
  private = TRUE, # this is default
  tags = new_tags,
  author = pkg_state$author,
  author_email = pkg_state$author_email,
  license_id = pkg_state$license_id,
  notes = pkg_state$notes,
  version = as.character(version_new),
  owner_org = pkg_state$owner_org,
  state = "active",
  type = "dataset",
  extras = list(
    topic_category = pkg_state$topic_category,
    # position = pkg_state$position,
    publisher = pkg_state$publisher,
    associated_parties = pkg_state$associated_parties,
    persons = pkg_state$persons,
    temporal_start = pkg_state$temporal_start,
    temporal_end = end_new,
    location = pkg_state$location,
    scientific_name = pkg_state$scientific_name,
    scripts = pkg_state$scripts,
    protocol = pkg_state$protocol,
    bibliography = pkg_state$bibliography,
    funding = pkg_state$funding,
    datasets = datasets_new
    # embargo = embargo_new
  )
)



## ---------------------------------- ##
## UPLOAD THE STATE VARIABLE
## ---------------------------------- ##

## The state variable has to be created on www.data.coat.no

## serach for your dataset
state_name <- "v37_special_predators_mustelid_abundance_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

for (i in state_var_names) {
  resource_create(
    package_id = pkg_state$id,
    description = NULL,
    upload = paste(tempdir(), i, sep = "/"),
    name = i,
    http_method = "POST"
  )
}


## ---------------------------------- ##
## UPDATE THE STATE VARIABLE
## ---------------------------------- ##

## here you can update the metadata of the sate variable
## you can for example change the sate from 'draft' to 'active (this is necessary if you created the state variable on data.coat.no and then added the datafiles via R)
## you can also change the visibility from private to public

pkg_state$name # check that the name is correct

## save metadata of the package as a list (as = table -> but the object will be a list)
pkg_updated <- package_show(pkg_state$id, as = "table", http_method = "POST")

## do the necessary modifications of the metadata
names(pkg_updated) # show the names of all metadata fields that can be updated
pkg_updated$private <- TRUE # set private = FALSE to publish a dataset
pkg_updated$state <- "active"

## discard empty metadata fields (they will cause a validation error)
pkg_updated <- discard(pkg_updated, is.null)

## update the package
package_update(pkg_updated, pkg_state$id, http_method = "POST")
