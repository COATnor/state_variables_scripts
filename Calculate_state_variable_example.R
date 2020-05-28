#  ********* CALCULATE A STATE VARIABLE FROM DATA DOWNLOADED FROM THE PORTAL    **********

# This script contains an examples of how the calculation of a simple state variable from a COAT dataset
# can be documented. AS example I use the public test data set V_dymmy_data located in the Cross-module dataset container.
# Created by Jane Uhd Jepsen May 2020

# Please note that the following terminology is used in ckan:
# "organization" = a module in the COAT data portal
# "package" = a data set in the COAT data portal
# "resource" = a file within a dataset in the COAT data portal
# "User" = individual persons registred as users in the COAT data portal

#------------------------------------------------------------------------------------------------------

# install ckanr package
install.packages("ckanr")
# import ckanr library
library(ckanr)
library(ggplot2)


# setup the connection to the data portal
COAT_url<-"https://coatdemo.frafra.no/"
# The use of an API key allows the user to access also non-public data
ckanr_setup(url = COAT_url, key = "xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx")
# Without an API key only public data can be reached
ckanr_setup(url = COAT_url)


#   @frafra Where do I find the API key?
#   @frafra state variables will also be based on non-public data (e.g. updated to most recent year). We want to be able to share a 
#            script showing how state variables are calculated from the data, but we do not want to share the api key to allow other to get to the 
#            non-public data. What is the best rutine? 




# Search for all datasets within a certain module. At present the V_dummy_data is the only public dataset available
mod_web_pkg<-package_search('cross-module-datasets')$results



#-------------download the dataset------------------------
# get the dataset name (which includes the version - update to a specific other version if needed)
dataset <- mod_web_pkg[[1]]$name
# Set the remote URL pointing to the .zip package containing all the resources (files) of a dataset
remote_zip <- paste(paste(COAT_url,"dataset/", sep=""), dataset, "/zip", sep = "")
# Set a local destination for data download
destdir<-"R://Prosjekter/COAT/Data Management/Formatted data/scripts/"
destination <- paste(destdir, paste(dataset,".zip",sep=""), sep = "")
# download the zip package
download.file(remote_zip, destination, mode="wb")
#I assume data are unzipped to the local directory for the record. 
files<-unzip(destination, overwrite=TRUE,exdir=paste(destdir,"unzipped",sep=""))

#    @frafra: I think most people will want to store a copy on a local directory when they download and work on a dataset. 
#             But if not, can the files be read directly from the portal, skipping local storage?

#----------open the data from local directory ------------------------------

dat<-read.delim(files[[1]]) #read 1st file
for (i in 2:length(files)){
  dat<-rbind(dat,read.delim(files[i]))  #append all other files to same data frame
}

#------------Calculate state variable ---------------------------------------------------------
#The dummy data contains annual data for 3 imiginary species sampled at 4 localities in one region. Each locality have 10 sampling sites. 
#The example state variable is here calculated as a simple average for each species over alle sites for each of the 4 localities separately.

#Aggregate the data to mean over t_year * sn_locality * v_species
dat.agg<-aggregate(dat$v_abundance,by=list(sn_locality=dat$sn_locality,t_year=dat$t_year,v_species=dat$v_species),FUN=mean, na.rm=TRUE)
colnames(dat.agg)[4]<-"v_abundance"

#Make a test plot
ggplot(aes(t_year,v_abundance, color=v_species),data=dat.agg)+geom_line()+facet_grid(~sn_locality)

#---------------Export state variable----------------------------------------
#Export to local disk
write.table(dat.agg,paste(destdir,"unzipped/ST_dummy_data.txt",sep=""),row.names = F)

#Push the state variable directly to the COAT server? 

#   @frafra Can this be done from R? In ckanr it should be possible to do something like below, but I get errors probably because
#           I need the API key. 

# create a package
res <- package_create("PackageName", author="PackageAuthor")
# then create a resource
file <- system.file("examples", "ResourceName.txt", package = "ckanr")
xx <- resource_create(package_id = res$id,
                       description = "my resource",
                       name = "ResourceName",
                       upload = file,
                       rcurl = "URL")







