#### ------------------------------------------------------------------------------------------------------------ ####
### CALCULATE STATE VARIABLE - V85_snowbed_dwarf_shrub_abundance_intensive
### Date: 17.11.2022
### Author: Hanna Böhner
#### ------------------------------------------------------------------------------------------------------------ ####


## this script is used to calculate the state variable V33_rodents_norwegian_lemming_abundance_intensive

## the script downloads the camera trapping and snap trapping datasets from the COAT dataportal
## preprocesses the data to keep only one motion sensor image per trigger
## calculates abundance of lemming as logarithm of number of passings per week (camera traps) or logarithm of number of caught lemmings per trapping event
## creates a new version of the state variable
## uploads the state variable to the COAT dataportal

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


## download functions from github
source("https://github.com/COATnor/data_management_scripts/blob/master/download_data_from_coat_data_portal.R?raw=TRUE")


## ---------------------------------- ##
## DOWNLOAD DATA
## ---------------------------------- ##

## set up the connection to the coat data portal
ckanr_setup(url = "https://data.coat.no", 
            key = Sys.getenv("api_COAT")) 

## define years
years <- 2009:2024

## specify filenames
filenames <- paste0("V_snowbed_shrub_abundance_intensive_", years, ".txt")

## download data
dat_list <- download_coat_data(name = "v_snowbed_shrub_abundance_intensive_v6",
                                filenames = filenames)


## ---------------------------------- ##
## CALCULATE STATE VARIABLE
## ---------------------------------- ##

state_var_names <- c()

for (i in 1:length(years)) {
  
  ## correct year in data from 2024
  if (years[i] == 2024) dat_list[[i]]$t_year <- 2024
  
  ## correct bet_nan nd vac_vit (inverted) in data from 2022 and 2024
  if (years[i] %in% c(2022, 2024)) {
    dat_list[[i]]$v_species[dat_list[[i]]$v_species == "bet_nan" & dat_list[[i]]$sn_locality == "komagdalen"] <- "vac_vit_new"
    dat_list[[i]]$v_species[dat_list[[i]]$v_species == "vac_vit" & dat_list[[i]]$sn_locality == "komagdalen"] <- "bet_nan_new"
    
    dat_list[[i]]$v_species[dat_list[[i]]$v_species == "vac_vit_new" & dat_list[[i]]$sn_locality == "komagdalen"] <- "vac_vit"
    dat_list[[i]]$v_species[dat_list[[i]]$v_species == "bet_nan_new" & dat_list[[i]]$sn_locality == "komagdalen"] <- "bet_nan"
  }
  
  
  dat_new <- dat_list[[i]] %>% group_by(sn_region, sn_locality, sn_section, sn_site, sn_plot, sc_plot_treatment, t_year, v_species) %>% 
    dplyr::summarise(v_abundance = sum(v_presence, na.rm = TRUE))
  
  ## save the file to a temporary directory (necessary for uploading it)
  state_var_names[i] <- paste0("V85_snowbed_dwarf_shrub_abundance_intensive_", years[i], ".txt")
  write.table(dat_new, paste(tempdir(), state_var_names[i], sep = "/"), row.names = FALSE, sep = ";")
  print(paste("state variable calculated and saved to temporary directory:", state_var_names[i]))
  
  ## 
  #out.dir <- "C:/Users/hbo042/Box/COAT/Modules/Small rodent module/state_variable_developement/shrub_abundance/data"
  #new_name <- paste0("state_variable_shrub_abundance_", years[i], ".txt")
  #write.table(dat_new, paste(out.dir, new_name, sep = "/"), row.names = FALSE, sep = ";")
  
  
}


## ---------------------------------- ##
## CREATE A NEW VERSION OF THE STATE VARIABLE
## ---------------------------------- ##

## you can either create a new version of the state variable or add the data to a already existing state variable (then you can skip this part)

## serach for your dataset
state_name <- "v85_snowbed_dwarf_shrub_abundance_intensive_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name # check the name

## modify metadata of the state variable
name_new <- "v85_snowbed_dwarf_shrub_abundance_intensive_v2" # write here the name of the new version (for example change v1 to v2)
version_new <- "2" # write here the new version
end_new <- "2024-07-31" # write here the (new) end date of the dataset

pkg_state$datasets # check with which dataset the current version of the state variable is associated
datasets_new <- "v_snowbed_shrub_abundance_intensive_v6" # write here the name (inlcuding the version) of the dataset the should be associated with the new version of the state variable

# These are the typlical modifications when creating a new version of a state variable before adding data of another year
# other modification can be made if necessary

# modify tags (necessary to avoid a validation error)
new_tags <- c()
for (i in 1:length(pkg_state$tags)) {
  new_tags[[i]] <- list(name = pkg_state$tags[[i]]$name)
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
state_name <- "v85_snowbed_dwarf_shrub_abundance_intensive_v2" # write here the name including the version of the state variable you want to add data to
state_version <- "2" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

## upload the data
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
