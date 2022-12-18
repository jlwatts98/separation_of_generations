##### Process Data #####

# source header
source("header.R")

##### Nitta et al. 2016 Lifecycle matters #####

# load in site data
sites = read_csv("raw-data/Nitta_et_al_2016_lifecycle_matters/data_and_scripts/shared_data/sites.csv")

# load in sporophyte data
nitta_2017 = read.csv("raw-data/Nitta_et_al_2016_lifecycle_matters/data_and_scripts/Comm_Phylo_Analysis/data/all_plots.csv")

names(nitta_2017)[1] = "site"

# add lat and lon data by matching the site to the lat and lon
lat = sites$lat
lon = sites$long

nitta_2017$lat = rep(lat, 2)
nitta_2017$lon = rep(lon, 2)

# pivot long

nitta_2017 = tidyr::pivot_longer(nitta_2017,
                            cols = 2:147,
                            names_to = "species",
                            values_to = "abundance")

# filter 0s (not present)
nitta_2017 = nitta_2017 |>
  dplyr::filter(abundance != 0)

# make a lifestage column
nitta_2017 = nitta_2017 |>
  tidyr::separate(site, into = c("site", "lifestage"), sep = -2, convert = TRUE)

nitta_2017$lifestage = ifelse(nitta_2017$lifestage == "_S",
                            "Sporophyte",
                            "Gametophyte")

# add citation column
nitta_2017$citation = rep("Nitta, J. H., Meyer, J. Y., Taputuarai, R., & Davis, C. C. (2017). Life cycle matters: DNA barcoding reveals contrasting community structure between fern sporophytes and gametophytes. Ecological Monographs, 87(2), 278-296.", nrow(nitta_2017))

nitta_2017 = nitta_2017[, c(2:5,7)]

# change species name
nitta_2017$species = gsub("_", " ", nitta_2017$species)


##### Pinson et al Spatial Separation 2022 #####

# transcribe coordinates from table presented in the publication

species = rep("Callistopteris baldwinii", 7)
lifestage = c(rep("Sporophyte", 2), rep("Gametophyte", 5))
lat = c("21 30.09", "21 30.587", "21 19.39", "21 31.52",
             "21 18.47", "21 19.39", "21 20.331")
lon = c("158 09.01", "157 59.003", "157 48.39", "157 57.09",
              "157 46.47", "157 48.42", "157 48.003")

library(measurements)
# convert from decimal minutes to decimal degrees
lat = as.numeric(measurements::conv_unit(lat, 
                                   from = 'deg_dec_min', 
                                   to = 'dec_deg'))
lon = -1 * as.numeric(measurements::conv_unit(lon, 
                                    from = 'deg_dec_min', 
                                    to = 'dec_deg'))

# add citation
citation = rep("Pinson, J. B., Chambers, S. M., & Sessa, E. B. (2022). The spatial separation of Callistopteris baldwinii (Hymenophyllaceae) sporophytes and gametophytes along elevational gradients in Hawai ‘i. American Fern Journal, 112(1), 1-16.", 7)

# make dataframe
pinson_2022 = data.frame(species, lifestage, lat, lon, citation)


# load in Duffy et al 2015 wrightii
wrightii = readr::read_csv("raw-data/Duffy_et_al_2015_wrightii/occurrences.csv")

# load in Krippel 2005 hymenophyllum trichomanes
speciosum = readr::read_csv("raw-data/Krippel_2005_hymenophyllum_trichomanes/occurrences_t_speciosum.csv")

# load in LaBranch et al 2022
intricatum = readr::read_csv("raw-data/LaBranch_et_al_2022_C_intricatum_NY/occurrences.csv")

# load in Park et al 2020
jeju_island = readr::read_csv("raw-data/Park_et_al_2020/jeju_site_coordinates.csv")

jeju_island$citation = rep("Park, S. H., Kim, J. S., & Kim, H. T. (2020). Study of the independent gametophytes found on Jeju Island in South Korea and the first record of the obligate independent gametophyte of Antrophyum obovatum Baker. Ecology and Evolution, 10(14), 7826-7838.", nrow(jeju_island))

jeju_island = jeju_island[, c(2:6)]
# load in Lee et al 2020
lee = readr::read_csv("raw-data/Lee_et_al_2020/occurrences.csv")

# load in coordinates from google maps
google = readr::read_csv("raw-data/Google_maps/google_maps_raw.csv")


# separate latitude and longitude
google = google |> 
    tidyr::separate(lat_lon, c("lat","lon"),sep=",")
# remove site name
google = google[,c(1:5)]

# vittaria graminifolia sporophyte and V. appalachiana occurrences
# downloaded from gbif individually
vit_gram = readr::read_csv("raw-data/gbif/vittaria_graminifolia.csv") |>
    dplyr::select(species, decimalLatitude, decimalLongitude) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
vit_gram$citation = rep("GBIF.org (26 September 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.gnbdk9", nrow(vit_gram))
vit_gram$lifestage = rep("Sporophyte", nrow(vit_gram))

vit_appa = readr::read_csv("raw-data/gbif/vittaria_appalachiana.csv") |>
    dplyr::select(species, decimalLatitude, decimalLongitude) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
vit_appa$citation = rep("GBIF.org (26 September 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.a6crf7", nrow(vit_appa))
vit_appa$lifestage = rep("Gametophyte", nrow(vit_appa))

# combine all datasets
occurrences = rbind(lee, pinson_2022, google, speciosum, wrightii, jeju_island, intricatum, vit_appa, vit_gram)

# remove all species with numbers in them
nitta_2017 = nitta_2017[!grepl("1", nitta_2017$species),]
nitta_2017 = nitta_2017[!grepl("2", nitta_2017$species),]
nitta_2017 = nitta_2017[!grepl("3", nitta_2017$species),]
nitta_2017 = nitta_2017[!grepl(".jacobi", nitta_2017$species),]

# extract species names
gametophytes = dplyr::filter(occurrences, lifestage == "Gametophyte")

species_list = as.data.frame(unique(gametophytes$species))
nitta_gametophytes = nitta_2017 |>
    dplyr::filter(lifestage == "Gametophyte")
nitta_gams = as.data.frame(unique(nitta_gametophytes$species))
names(species_list)[1] = "species"
names(nitta_gams)[1] = "species"
species_list = rbind(nitta_gams, species_list)
species_list = as.data.frame(unique(species_list$species))
names(species_list)[1] = "species"

write.csv(species_list, "objects/species_list.csv")
# download gbif occurrences for the corresponding species
# sporophyte only data
# remember the closely related species to C intricatum is C schmidtianum
##### Download Gbif occurrences for all species #####
# https://data-blog.gbif.org/post/downloading-long-species-lists-on-gbif/


# fill in your gbif.org credentials 
user <- "jwatts" # your gbif.org username 
pwd <- "Jw017138@" # your gbif.org password
email <- "jwatts@colgate.edu" # your email 

library(dplyr)
library(purrr)
library(readr)  
library(magrittr) # for %T>% pipe
library(rgbif) # for occ_download
library(taxize) # for get_gbifid_

#file_url <- "https://data-blog.gbif.org/post/2019-07-11-downloading-long-species-lists-on-gbif_files/global_tree_search_trees_1_3.csv"

# match the names 
#gbif_taxon_keys <- 
#    species_list %>% 
#    pull("species") %>% # use fewer names if you want to just test 
#    taxize::get_gbifid_(method="backbone") %>% # match names to the GBIF backbone to get taxonkeys
#    imap(~ .x %>% mutate(original_sciname = .y)) %>% # add original name back into data.frame
#    bind_rows() %T>% # combine all data.frames into one
#    readr::write_tsv(path = "all_matches.tsv") %>% # save as side effect for you to inspect if you want
#    filter(matchtype == "EXACT" & status == "ACCEPTED") %>% # get only accepted and matched names
 #   filter(kingdom == "Plantae") %>% # remove anything that might have matched to a non-plant
 #   pull(usagekey) # get the gbif taxonkeys


# gbif_taxon_keys should be a long vector like this c(2977832,2977901,2977966,2977835,2977863)
# !!very important here to use pred_in!!

#occ_download(
#    pred_in("taxonKey", gbif_taxon_keys),
#    pred_in("basisOfRecord", c('PRESERVED_SPECIMEN')),
#    pred("hasCoordinate", TRUE),
#    pred("hasGeospatialIssue", FALSE),
#    pred_gte("year", 1900),
#    format = "SIMPLE_CSV",
#    user=user,pwd=pwd,email=email
#)

# load in sporophyte data and clean
gbif_sporophytes = readr::read_csv("raw-data/sporophytes/sporophytes.csv")
missing_sporos = readr::read_csv("raw-data/gbif/missing_sporos.csv")
tri_spec = readr::read_csv("raw-data/gbif/trichomanes_speciosum.csv")
grammitis = readr::read_csv("raw-data/gbif/moranopteris_nimbata.csv")
didymo = readr::read_csv("raw-data/gbif/didymo.csv")
cal_bald = readr::read_csv("raw-data/gbif/cal_bald.csv")

tri_spec = rbind(tri_spec, grammitis)
tri_spec = rbind(tri_spec, didymo)
tri_spec = rbind(tri_spec, cal_bald)

# exclude hymenophyllum wrightii occurrences from pacific northwest because no way to know if sporophyte or gametophyte
gbif_sporophytes = gbif_sporophytes |>
    dplyr::filter(species != "Hymenophyllum wrightii" | decimalLongitude > -105)

library(CoordinateCleaner)
library(rnaturalearthdata)

#flag problems
dat <- data.frame(gbif_sporophytes)
flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           #countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros")) # most test are on by default
#Exclude problematic records
gbif_clean <- dat[flags$.summary,] |>
    dplyr::select(species, decimalLatitude, decimalLongitude) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
gbif_clean$citation = rep("GBIF Occurrence Download https://doi.org/10.15468/dl.546waz Accessed from R via rgbif (https://github.com/ropensci/rgbif) on 2022-09-26", nrow(gbif_clean))
gbif_clean$lifestage = rep("Sporophyte", nrow(gbif_clean))

#flag problems
dat <- data.frame(missing_sporos)
flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           #countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros")) # most test are on by default
#Exclude problematic records
missing_clean <- dat[flags$.summary,] |>
    dplyr::select(species, decimalLatitude, decimalLongitude) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
missing_clean$citation = rep("GBIF.org (01 October 2022) GBIF Occurrence Download https://doi.org/10.15468/dl.aguzd6", nrow(missing_clean))
missing_clean$lifestage = rep("Sporophyte", nrow(missing_clean))

dat <- data.frame(tri_spec)
flags <- clean_coordinates(x = dat, 
                           lon = "decimalLongitude", 
                           lat = "decimalLatitude",
                           #countries = "countryCode",
                           species = "species",
                           tests = c("capitals", "centroids", "equal","gbif", "institutions",
                                     "zeros")) # most test are on by default
#Exclude problematic records
tri_clean <- dat[flags$.summary,] |>
    dplyr::select(species, decimalLatitude, decimalLongitude) |>
    dplyr::rename(lat = decimalLatitude,
                  lon = decimalLongitude)
tri_clean$citation = rep("GBIF.org (07 October 2022) GBIF Occurrence Download  https://doi.org/10.15468/dl.323svd", nrow(tri_clean))
tri_clean$lifestage = rep("Sporophyte", nrow(tri_clean))



gbif_clean = rbind(gbif_clean, missing_clean)
gbif_clean = rbind(gbif_clean, tri_clean)

nitta_sporos = missing_clean |>
    dplyr::filter(lon < -148 & lon > -150 & lat > -18 & lat < -16)

# append this to all other occurrences
occurrences = rbind(occurrences, gbif_clean)

# remove na from lat and lon, make numeric
occurrences = occurrences[!is.na(occurrences$lat),]
occurrences = occurrences[!is.na(occurrences$lon),]
occurrences$lat = as.numeric(occurrences$lat)
occurrences$lon = as.numeric(occurrences$lon)

##### load in climate and soil data #####

# bioclim data
bioclim = raster::getData("worldclim", var="bio", res = 2.5)

# soil data
# https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1247
# https://www.fao.org/soils-portal/data-hub/soil-maps-and-databases/harmonized-world-soil-database-v12/en/

library(raster)
library(ncdf4)
nc4s = list.files(path = "raw-data/climate_soil/HWSD_1247/data_reduced", pattern="\\.nc4$")
setwd("raw-data/climate_soil/HWSD_1247/data_reduced")
soil = raster::stack(nc4s)
setwd("~/R/separation_of_generations")


nitta_list = list.files(path = "raw-data/climate_soil/nitta", pattern="\\.tif$")
setwd("raw-data/climate_soil/nitta")
nitta_soil = raster::stack(nitta_list)
setwd("~/R/separation_of_generations")

# extract nitta et al with smaller soil grid - more accurate
# append to full dataframe and check for equivalence - 
#       divide nitta bulk density by 100
plot(nitta_soil[[1]])
points(points(nitta_2017$lon, nitta_2017$lat))
points(missing_clean$lon, missing_clean$lat)

nitta_2017 = rbind(nitta_2017, nitta_sporos)

coordinates(nitta_2017) = ~ lon + lat

# fill holes in soil data if bad...
#plot(soil[[2]])
#soil[[2]] = focal(soil[[2]], w=matrix(1,nrow = 55, ncol = 55), fun=mean, NAonly=F, na.rm=TRUE)
#plot(soil[[2]])


# Terraclimate vpd
# http://thredds.northwestknowledge.net:8080/thredds/catalog/TERRACLIMATE_ALL/summaries/catalog.html
vpd = raster("raw-data/climate_soil/terraclimate/vpd.nc")

# envirem 2.5 arcmins
# https://envirem.github.io/
tifs = list.files(path = "raw-data/climate_soil/envirem_2.5", pattern="\\.tif$")
setwd("raw-data/climate_soil/envirem_2.5")
envirem = stack(tifs)
setwd("~/R/separation_of_generations")

# topographic ruggedness index
#dem = raster("raw-data/climate_soil/dem25.tif")

#library(spatialEco)
#tri = tri(dem, s = 5, exact = F)
#tri = raster::focal(tri, w=matrix(1,nrow = 5, ncol = 5), fun=mean, NAonly=F, na.rm=TRUE)

# drought index from Chris
drought_index = raster("raw-data/climate_soil/drought_index.tif")
drought_index = raster::focal(drought_index, w=matrix(1,nrow = 5, ncol = 5), fun=mean, NAonly=F, na.rm=TRUE)

# ft_index
ft_index = raster("raw-data/climate_soil/ft_index.tif")
ft_index = raster::focal(ft_index, w=matrix(1,nrow = 5, ncol = 5), fun=mean, NAonly=F, na.rm=TRUE)

##### Extract values of raster to each occurrence point #####
# extract value of each raster to each occurrence
coordinates(occurrences) = ~ lon + lat

# extract value to point
bioclimvals = raster::extract(bioclim, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(occurrences, bioclimvals)

# extract value to point
soilvals = raster::extract(soil, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(environment_occurrences, soilvals)

# extract value to point
enviremvals = raster::extract(envirem, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(environment_occurrences, enviremvals)

# extract value to point
droughtvals = raster::extract(drought_index, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(environment_occurrences, droughtvals)

# extract value to point
ftvals = raster::extract(ft_index, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(environment_occurrences, ftvals)

# extract value to point
vpdvals = raster::extract(vpd, occurrences)

# combine the values and occurrences
environment_occurrences = cbind(environment_occurrences, vpdvals)

# nitta et al. 2017
# extract value to point
bioclimvals = raster::extract(bioclim, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(nitta_2017, bioclimvals)

# extract value to point
soilvals = raster::extract(nitta_soil, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(environment_nitta_2017, soilvals)

# extract value to point
enviremvals = raster::extract(envirem, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(environment_nitta_2017, enviremvals)

# extract value to point
droughtvals = raster::extract(drought_index, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(environment_nitta_2017, droughtvals)

# extract value to point
ftvals = raster::extract(ft_index, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(environment_nitta_2017, ftvals)

# extract value to point
vpdvals = raster::extract(vpd, nitta_2017)

# combine the values and nitta_2017
environment_nitta_2017 = cbind(environment_nitta_2017, vpdvals)

#write to a csv
write.table(environment_occurrences, file="raw-data/environment_occurrences.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

# nitta_2017
write.table(environment_nitta_2017, file="raw-data/environment_nitta_2017.csv", append=FALSE, sep= ",", row.names = FALSE, col.names=TRUE)

##### Combine data into final csv #####
# fix column names
#write to a csv
environment_occurrences = readr::read_csv("raw-data/environment_occurrences.csv")
environment_nitta_2017 = readr::read_csv("raw-data/environment_nitta_2017.csv")
unique(environment_occurrences$species)
unique(environment_nitta_2017$species)
names(environment_occurrences)
names(environment_nitta_2017)

names(environment_occurrences)[46] = "drought_index"
names(environment_occurrences)[47] = "ft_index"
names(environment_occurrences)[48] = "vpd"
names(environment_nitta_2017)[46] = "drought_index"
names(environment_nitta_2017)[47] = "ft_index"
names(environment_nitta_2017)[48] = "vpd"

names(environment_nitta_2017)[23] = "topsoil.bulk.density"
names(environment_nitta_2017)[24] = "topsoil.clay.fraction.by.percent.weight"
names(environment_nitta_2017)[25] = "topsoil.gravel.content.by.percent.volume"
names(environment_nitta_2017)[26] = "topsoil.organic.carbon.by.percent.weight"
names(environment_nitta_2017)[27] = "topsoil.pH.in.water"
names(environment_nitta_2017)[28] = "topsoil.sand.fraction.by.percent.weight"
names(environment_nitta_2017)[29] = "topsoil.silt.fraction.by.percent.weight"

# convert values to match each other
environment_nitta_2017$topsoil.bulk.density = environment_nitta_2017$topsoil.bulk.density / 100
environment_nitta_2017$topsoil.clay.fraction.by.percent.weight = environment_nitta_2017$topsoil.clay.fraction.by.percent.weight / 10
environment_nitta_2017$topsoil.gravel.content.by.percent.volume = environment_nitta_2017$topsoil.gravel.content.by.percent.volume / 10
environment_nitta_2017$topsoil.organic.carbon.by.percent.weight = environment_nitta_2017$topsoil.organic.carbon.by.percent.weight / 1000
environment_nitta_2017$topsoil.pH.in.water = environment_nitta_2017$topsoil.pH.in.water / 10
environment_nitta_2017$topsoil.sand.fraction.by.percent.weight = environment_nitta_2017$topsoil.sand.fraction.by.percent.weight / 10
environment_nitta_2017$topsoil.silt.fraction.by.percent.weight = environment_nitta_2017$topsoil.silt.fraction.by.percent.weight / 10

# crop dataframes
environment_occurrences = environment_occurrences[, c(1:50)]
environment_nitta_2017 = environment_nitta_2017[, c(1:50)]

# rbind
environment_occurrences = rbind(environment_occurrences, environment_nitta_2017)


gametos = environment_occurrences |>
    dplyr::filter(lifestage == "Gametophyte")
unique(gametos$species)
# remove nas
environment_occurrences = na.omit(environment_occurrences)

gametos = environment_occurrences |>
    dplyr::filter(lifestage == "Gametophyte")
unique(gametos$species)
# fix crepidomanes intricatum data
environment_occurrences$lifestage = ifelse(environment_occurrences$species == "Crepidomanes intricatum", "Gametophyte",
                                           environment_occurrences$lifestage)


#environment_occurrences$lifestage_pair = ifelse(environment_occurrences$species == "Vittaria appalachiana" | environment_occurrences$species == "Vittaria graminifolia", "Vittaria appalachiana",
#                                                ifelse(environment_occurrences$species == "Crepidomanes intricatum", "Crepidomanes schmidtianum",
#                                                       ifelse(environment_occurrences$species == "Vandenboschia speciosa", "Trichomanes speciosum",
#                                                            ifelse(has_gametophyte$count == 2, environment_occurrences$species, NA))))



environment_occurrences = na.omit(environment_occurrences)
unique((environment_occurrences |> dplyr::filter(lifestage == "Gametophyte"))$species)


plot(bioclim[[1]])
points(environment_occurrences$lon, environment_occurrences$lat, pch = 20)

readr::write_csv(environment_occurrences, "objects/environment_occurrences.csv")

