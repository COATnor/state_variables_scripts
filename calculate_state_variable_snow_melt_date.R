#### ------------------------------------------------------------------------------------------------------------ ####
### State variable: SNOWMELT --------
### Date: 17.11.2022
### Author: Hanna Böhner
#### ------------------------------------------------------------------------------------------------------------ ####


## this script is used to calculate the state variable V_snow_melt_date_logger

## the script downloads the dataset V_air_temperature_snowbed from the COAT dataportal
## calculates the sate variable V_snow_melt_date_logger
## plots snowmelt of new years to visually check if the calculated date looks correct
## creates a new version of the state variable
## uploads the state variable to the COAT dataportal

## snow melt is calculated as the first day with a daily mean temperature over 1.1 deg C
## if the loggers a lying in melt water, temperatures often rise over 1.1 deg C and then stay constant at around 0 deg C for another couple of days
## some loggers did not reach a daily mean temperature over 1.1 deg C before they were collected. However, these loggers were collected just after the snow melted.
## Thus, the date when the loggers were collected will be used as the date of snow melt

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


## setup the connection to the data portal
COAT_url <- "https://data.coat.no" # write here the url to the COAT data portal
COAT_key <- Sys.getenv("API_coat") # write here your API key if you are a registered user, continue without API key if you are not registered
# the API can be found on you page on the COAT data portal (log in and click on your name in the upper right corner of the page)
# The use of an API key allows the user to access also non-public data

ckanr_setup(url = COAT_url, key = COAT_key) # set up the ckanr-API


## ---------------------------------- ##
## DOWNLOAD DATA
## ---------------------------------- ##

## list all datasets available on the COAT data portal
package_list()

## serach for your dataset
name <- "v_air_temperature_snowbed_v4" # write here the name including the version of the dataset you want to download
version <- "4" # write here the version of the dataset

pkg <- package_search(q = list(paste("name:", name, sep = "")), fq = list(paste("version:", version, sep = "")), include_private = TRUE)$results[[1]] # search for the dataset and save the results
urls <- pkg$resources %>% sapply("[[", "url") # get the urls to the files included in the dataset
filenames <- pkg$resources %>% sapply("[[", "name") # get the filenames

## specify if the downloaded files should be saved to your computer or imported into R
store <- "session" # "session" (imports data into R) or "disk" (saves the file to you computer)


## select temperature data files from one year

meta <- grep("readme|aux|coordinates|metadata", filenames)
urls2 <- urls[-meta]
filenames2 <- filenames[-meta]

## download aux file
aux_id <- grep("aux", filenames)
filenames[aux_id]
aux_url <- urls[aux_id]

aux <- ckan_fetch(aux_url,
  store = store,
  sep = ";",
  header = TRUE,
  format = "txt"
)
head(aux)

## download all files of the dataset
mylist <- c() # empty object for the files

for (i in 1:length(filenames2)) {
  mylist[[i]] <- ckan_fetch(urls2[i],
    store = store,
    path = paste(dest_dir, name, filenames2[i], sep = "/"),
    sep = ";",
    header = TRUE,
    format = "txt"
  )
  mylist[[i]]$t_year <- gsub("V_air_temperature_snowbed_|.txt", "", filenames2[i])  # needs to be modified to match other datasets
}


## ---------------------------------- ##
## CALCULATE SNOWMELT
## ---------------------------------- ##

## snowmelt will be calculated per site and year as described above

years <- unlist(map(mylist, function(x) unique(x$t_year)))
snowmelt <- c()
state_var_names <- c()

for (i in 1:length(years)) {
  ## calculate snowmelt (first day with daily mean temp over 1.1 deg C)
  snowmelt[[i]] <- mylist[[i]] %>%
    group_by(sn_region, sn_locality, sn_section, sc_type_of_sites_ecological, sn_site, sn_plot, t_year, t_date) %>%
    summarise(mean_temp = mean(as.numeric(v_temperature), na.rm = TRUE)) %>%
    filter(mean_temp > 1.1) %>%
    filter(grepl(years[i], t_date)) %>%
    group_by(sn_region, sn_locality, sn_section, sc_type_of_sites_ecological, sn_site, sn_plot, t_year) %>%
    summarise(t_date_snowmelt = min(t_date))

  ## add sites where the snow just melted before the logger was collected
  ## temperature didn't start to rise for these loggers and the snowmelt date will be set to the date when the logger was collected (one day after the last registered date)
  missing <- unique(mylist[[i]]$sn_site)[!unique(mylist[[i]]$sn_site) %in% unique(snowmelt[[i]]$sn_site)]

  if (length(missing) > 0) {
    add <- mylist[[i]][mylist[[i]]$sn_site %in% missing, 1:7] %>%
      group_by(sn_region, sn_locality, sn_section, sc_type_of_sites_ecological, sn_site, sn_plot) %>%
      summarise(t_date_snowmelt = max(t_date)) %>%
      mutate(t_date_snowmelt = as.character(ymd(t_date_snowmelt) + days(1))) %>%
      add_column(t_year = years[i])
    snowmelt[[i]] <- rbind(snowmelt[[i]], add)
  }

  ## add sites where the logger was not found/did not work with NA for t_snowmelt_date
  pot.sites <- aux$sn_site[is.na(aux$year_last) | aux$year_last >= years[i]]
  missing <- pot.sites[!pot.sites %in% snowmelt[[i]]$sn_site]

  if (length(missing) > 0) {
    add <- aux %>%
      filter(sn_site %in% missing) %>%
      select(sn_region, sn_locality, sn_section, sn_site) %>%
      add_column(sc_type_of_sites_ecological = "snowbed", sn_plot = 2, t_date_snowmelt = NA, t_year = years[i])  # needs to be modified to match other datasets
    snowmelt[[i]] <- rbind(snowmelt[[i]], add)
  }

  snowmelt[[i]] <- arrange(snowmelt[[i]], sn_site)
  
  ## set snow melt date to NA if logger stopped logging too early
  if (years[i] == "2019") snowmelt[[i]]$t_date_snowmelt[snowmelt[[i]]$sn_site == "vj_be_sn_11"] <- NA 
  
  ## correct snowmelt (after inspecting calculated snow melt dates visually)
  if (years[i] == "2023") snowmelt[[i]]$t_date_snowmelt[snowmelt[[i]]$sn_site == "ko_kj_sn_8"] <- "2023-05-26"

  ## save the file to a temporary directory (necessary for uploading it)
  state_var_names[i] <- paste0("C1_snow_melt_date_logger_varanger_snowbeds_", years[i], ".txt")
  write.table(snowmelt[[i]], paste(tempdir(), state_var_names[i], sep = "/"), row.names = FALSE, sep = ";")
  print(paste("state variable calculated and saved to temporary directory:", state_var_names[i]))
}

## check if snow melt date and year match
snowmelt %>% do.call(rbind, .) %>% 
  filter(t_year != year(ymd(t_date_snowmelt)))



## ---------------------------------- ##
## PLOT SNOW MELT
## ---------------------------------- ##

## plot data of new years to check if the snow melt date looks correct (you don't need to plot old years, they have been checked already)

year <- 2023

x <- which(years == year)

snowmelt_x <- snowmelt[[x]]
myfile_x <- mylist[[x]]

snowbeds <- unique(snowmelt_x$sn_site)

for (i in snowbeds) {
  ## prepare snowmelt data
  dat <- snowmelt_x %>%
    subset(sn_site == i) %>%
    mutate(t_date_snowmelt = ymd(t_date_snowmelt))

  if (is.na(dat$t_date_snowmelt)) next

  ## prepare temperature data
  temp_dat <- myfile_x %>%
    subset(sn_site == i) %>%
    mutate(t_date <- ymd(t_date)) %>%
    subset(t_date > ymd(paste0(year, "04_30")))

  ## plot temperature data and snowmelt date
  plot(v_temperature ~ ymd_hms(t_bintime), dat = temp_dat, type = "l", lwd = 1, xlab = "", main = paste(i, year))
  if (!is.na(dat$t_date_snowmelt)) abline(v = ymd_hms(paste(dat$t_date_snowmelt, "14:00:00")), col = "red", lwd = 2)
}


## ---------------------------------- ##
## CREATE A NEW VERSION OF THE STATE VARIABLE
## ---------------------------------- ##


## THIS DOES NOT WORK AT THE MOMENT -> associated dataset not included when creating the new version in R -> create the new version manually on data.coat.no
## you can either create a new version of the state variable or add the data to a already existing state variable (then you can skip this part)

## serach for your dataset
state_name <- "c1_snow_melt_date_logger_varanger_v1" # write here the name including the version of the state variable you want to add data to
state_version <- "1" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
pkg_state$name # check the name

## modify metadata of the state variable
name_new <- "c1_snow_melt_date_logger_varanger_v2" # write here the name of the new version (for example change v1 to v2)
version_new <- "2" # write here the new version
end_new <- "2023-08-31" # wirte here the (new) end date of the dataset

pkg_state$datasets # check with which dataset the current version of the state variable is associated
datasets_new <- "v_air_temperature_snowbed_v4" # write here the name (inlcuding the version) of the dataset the should be associated with the new version of the state variable

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
  datasets = datasets_new,
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
state_name <- "c1_snow_melt_date_logger_varanger_v2" # write here the name including the version of the state variable you want to add data to
state_version <- "2" # write here the version of the state variable

pkg_state <- package_search(q = list(paste("name:", state_name, sep = "")), fq = list(paste("version:", state_version, sep = "")), include_private = TRUE, include_drafts = TRUE)$results[[1]] # search for the dataset and save the results
filenames_state <- pkg_state$resources %>% sapply("[[", "name") # get the filenames
filenames_state # are there any files

# upload the files
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
