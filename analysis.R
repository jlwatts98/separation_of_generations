##### Analysis #####

# quantify climate and soil niche of each species 
# compare the gametophyte and sporophyte niches
# identify the variables driving the differences
# identify species with spatial separation between generations
# understand historical consequences of niche differences in Vittaria
# Guiding question from Krieg and Chambers
# Q1: How common is it for the gametophyte and sporophyte
# generations to substantially differ in geographic range?
#     What are the main environmental axes that unite and
# separate the realized niche between generations?

# some resources
# https://cran.r-project.org/web/packages/nicheROVER/vignettes/ecol-vignette.html
# https://rdrr.io/cran/iCAMP/man/dniche.html
# https://fukamilab.github.io/BIO202/06-C-matrix-comparison.html
# https://towardsdatascience.com/mahalonobis-distance-and-outlier-detection-in-r-cb9c37576d7d
# https://archetypalecology.wordpress.com/2018/02/21/permutational-multivariate-analysis-of-variance-permanova-in-r-preliminary/

# source header

source("header.R")

# load in occurrence and environmental data
environment_occurrences = readr::read_csv("objects/environment_occurrences.csv")
environment_occurrences <- subset(environment_occurrences, select = -current_2.5arcmin_monthCountByTemp10 )

species_list = readr::read_csv("objects/species_list.csv")
unique(environment_occurrences$species)

##### Supplementary Table 1: Species list, occurrence counts, citation #####

# make species lists
different = setdiff(gametos$species, sporos$species)
different

same = intersect(gametos$species, sporos$species)
same

# define species2 to manually match gametophytes and sporophytes
environment_occurrences = environment_occurrences %>%
    dplyr::mutate(species2 = case_when(
    species == 'Grammitis nimbata' ~ 'Moranopteris nimbata',
    species == 'Trichomanes speciosum' ~ 'Vandenboschia speciosa',
    species == 'Crepidomanes intricatum' ~ 'Crepidomanes schmidtianum',
    species == 'Vittaria appalachiana' ~ 'Vittaria graminifolia',
    TRUE ~ species))

# define gametophyte and sporophyte only datasets
gametos = environment_occurrences |>
    dplyr::filter(lifestage == "Gametophyte")
sporos = environment_occurrences |>
    dplyr::filter(lifestage == "Sporophyte")

# define species list with both gametophytes and sporophytes
allsp = intersect(gametos$species2, sporos$species2)
all = as.data.frame(allsp) |>
    dplyr::rename(
        species = allsp
    )

# count the number of occurrences for each generation
library(tidyr)
library(dplyr)
counts = environment_occurrences |>
    group_by(species2, lifestage) |>
    tally() |>
    rename(species = species2)


# reduced species list with counts
reduced_spec = merge(all, counts, by = "species")

# add unique citations
citations_bysp = environment_occurrences |>
    dplyr::group_by(species2, lifestage) |>
    dplyr::mutate(unique_citation = list(unique(citation))) |>
    dplyr::select(species2, unique_citation) |>
    summarise(citation = unique(unique_citation)) |>
    tidyr::separate(citation, into = c("citation1", "citation2", "citation3", 
                                       "citation4", "citation5", "citation6", 
                                       "citatation7", "citation8", 
                                       "citation9"), sep = "\"" ) |>
    dplyr::select(species2, lifestage, citation1, citation2,
                  citation4, citation6, citation8) |>
    dplyr::mutate(citation1 = case_when(
        citation1 == "c(" ~ citation2,
        TRUE ~ citation1)) |>
    dplyr::select(species2, lifestage, citation1,
                  citation4, citation6, citation8) |>
    dplyr::rename(
        species = species2,
        citation2 = citation4,
        citation3 = citation6,
        citation4 = citation8
    )

species_citations = merge(reduced_spec, citations_bysp, by = c("species", "lifestage"))

write.csv(species_citations, "objects/supp_table1.csv")

##### Supplementary Table 2: Environmental Variables and Units #####

# return columns of environmental occurrences as a list
env_vars = colnames(environment_occurrences)[c(4:47)]

# define which variables are categorized into which
moist = c(12:19, 28, 29, 31, 42, 44)
temp = c(1:11, 27, 30, 32:41, 43)
soil = c(20:26)

# variable name in the code/dataframe
code_name = env_vars[c(12:19, 28, 29, 31, 42, 44,
                      1:11, 27, 30, 32:41, 43,
                      20:26)]

# variable classification for visualization
var_type = c(rep("Moisture", 13),
             rep("Temperature", 24),
             rep("Soil", 7))

# units of environmental variables
units = c(rep("mm", 3), "Coefficient of Variation",
          rep("mm", 4), rep("unitless", 4), "kPa",
          rep("°C * 10", 2), "unitless", "unitless",
          rep("°C * 10", 7), "mm/year", "°C",
          "days", "days", rep("°C * 10", 2),
          rep("mm/month", 5), "°C", "unitless", 
          "kg/dm^3", "% weight",
          "% volume", "% weight", "-log([H+]) in water",
          "% weight", "% weight")

# names of environmental variables
var_name = c("Annual Precipitation",
             "Precipitation of Wettest Month",
             "Precipitation of Driest Month",
             "Precipitation Seasonality",
             "Precipitation of Wettest Quarter",
             "Precipitation of Driest Quarter",
             "Precipitation of Warmest Quarter",
             "Precipitation of Coldest Quarter",
             "Thornthwaite Aridity Index",
             "Climatic Moisture Index",
             "Emberger's Pluviothermic Quotient",
             "Drought Index",
             "Vapor Pressure Deficit",
             "Annual Mean Temperature",
             "Mean Diurnal Range",
             "Isothermality",
             "Temperature Seasonality",
             "Max Temperature of Warmest Month",
             "Min Temperature of Coldest Month",
             "Temperature Annual Range",
             "Mean Temperature of Wettest Quarter",
             "Mean Temperature of Driest Quarter",
             "Mean Temperature of Warmest Quarter",
             "Mean Temperature of Coldest Quarter",
             "Annual Potential Evapotranspiration (PET)",
             "Continentality",
             "Growing Degree Days (0°C)",
             "Growing Degree Days (5°C)",
             "Maximum Temperature of Coldest Month",
             "Minimum Temperature of Warmest Month",
             "PET of Coldest Quarter",
             "PET of Driest Quarter",
             "PET Seasonality",
             "PET of Warmest Quarter",
             "PET of Wettest Quarter",
             "Thermicity Index",
             "FT Index",
             "Topsoil Bulk Density",
             "Topsoil Clay Fraction",
             "Topsoil Gravel Content",
             "Topsoil Organic Carbon",
             "Topsoil pH",
             "Topsoil Sand Fraction",
             "Topsoil Silt Fraction")

# source of environmental variables
source = c(rep("WorldClim", 8),
           rep("Envirem", 3),
           "derived",
           "TerraClimate",
           rep("WorldClim", 11),
           rep("Envirem", 12),
           "derived",
           rep("FAO HWSD", 7))

# build a dataframe
supp_table2 = data.frame(var_name, code_name, var_type, source, units)

# convert dataframe into a table 
# Write this table to a comma separated .txt file:    
write.table(supp_table2, file = "objects/supp_table2.txt", sep = ",",
            quote = F, row.names = F)

##### Figure 1: PERMANOVAS and phylogeny #####

# Create dataset with only vittaria appalachiana and v. graminifolia
vit_gram = environment_occurrences |>
    dplyr::filter(species == "Vittaria graminifolia")
vit_gram = na.omit(vit_gram)

vit_appa = environment_occurrences |>
    dplyr::filter(species == "Vittaria appalachiana")
vit_appa = na.omit(vit_appa)

vit = rbind(vit_appa, vit_gram)

# library vegan package and geosphere packages
library(vegan)
library(geosphere)
library(ggfortify)
library(purrr)
library(PERMANOVA)
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html


# create a function the performs partwise PERMANOVAs and returns statistics

partwise_PERMANOVA = function(
        dataframe, # complete dataframe with all species
        selected_species, # character input lifestage pair for which to calculate dissimilarities between species
        permutations # number of permutations to calculate significance
        ){
    
    species_df = dataframe |>
        dplyr::filter(species2 == selected_species)
    
    species_standard = IniTransform(species_df[, c(4:47)])
    
    species = selected_species

    # moisture differences
    
    dist_moisture = DistContinuous(species_standard[, c(12:19, 28, 29, 31, 42, 44)])
    AOV_moisture = PERMANOVA(dist_moisture, as.factor(species_df$lifestage), nperm = permutations)
    
    # soil differences
    
    dist_soil = DistContinuous(species_standard[, c(20:26)])
    AOV_soil = PERMANOVA(dist_soil, as.factor(species_df$lifestage), nperm = permutations)
    
    # temperature differences
    
    dist_temp = DistContinuous(species_standard[, c(1:11, 27, 30, 32:41, 43)])
    AOV_temp = PERMANOVA(dist_temp, as.factor(species_df$lifestage), nperm = permutations)

    # extracting statistics
    
    initial = AOV_moisture$Initial
    moisture_explained = initial$Global[1]
    moisture_residual = initial$Global[2]
    
    initial = AOV_temp$Initial
    temp_explained = initial$Global[1]
    temp_residual = initial$Global[2]
    
    initial = AOV_soil$Initial
    soil_explained = initial$Global[1]
    soil_residual = initial$Global[2]
    
    # return statistics
    return(c(species, moisture_explained, moisture_residual, temp_explained, temp_residual, soil_explained, soil_residual))
    
}


### RUN functions ###
library(tidyr)

PERM1 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[1], permutations = 1)
PERM2 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[2], permutations = 1)
PERM3 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[3], permutations = 1)
PERM4 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[4], permutations = 1)
PERM5 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[5], permutations = 1)
PERM6 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[6], permutations = 1)
PERM7 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[7], permutations = 1)
PERM8 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[8], permutations = 1)
PERM9 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[9], permutations = 1)
PERM10 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[10], permutations = 1)

# compile into a dataframe
perm_df1 = data.frame(PERM1, PERM2, PERM3, PERM4, PERM5,
                     PERM6, PERM7, PERM8, PERM9, PERM10)
perm_df1 = as.data.frame(t(perm_df1))

readr::write_csv(perm_df1, "objects/perm_df1.csv")

PERM11 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[11], permutations = 1)
PERM12 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[12], permutations = 1)
PERM13 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[13], permutations = 1)
PERM14 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[14], permutations = 1)
PERM15 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[15], permutations = 1)
PERM16 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[16], permutations = 1)
PERM17 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[17], permutations = 1)
PERM18 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[18], permutations = 1)
PERM19 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[19], permutations = 1)
PERM20 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[20], permutations = 1)

perm_df2 = data.frame(PERM11, PERM12, PERM13, PERM14, PERM15,
                      PERM16, PERM17, PERM18, PERM19, PERM20)
perm_df2 = as.data.frame(t(perm_df2))

readr::write_csv(perm_df2, "objects/perm_df2.csv")

PERM21 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[21], permutations = 1)
PERM22 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[22], permutations = 1)
PERM23 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[23], permutations = 1)
PERM24 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[24], permutations = 1)
PERM25 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[25], permutations = 1)
PERM26 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[26], permutations = 1)
PERM27 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[27], permutations = 1)
PERM28 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[28], permutations = 1)
PERM29 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[29], permutations = 1)
PERM30 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[30], permutations = 1)

# compile into a dataframe
perm_df3 = data.frame(PERM21, PERM22, PERM23, PERM24, PERM25,
                      PERM26, PERM27, PERM28, PERM29, PERM30)
perm_df3 = as.data.frame(t(perm_df3))

readr::write_csv(perm_df3, "objects/perm_df3.csv")

PERM31 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[31], permutations = 1)
PERM32 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[32], permutations = 1)
PERM33 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[33], permutations = 1)
PERM34 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[34], permutations = 1)
PERM35 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[35], permutations = 1)
PERM36 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[36], permutations = 1)
PERM37 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[37], permutations = 1)
PERM38 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[38], permutations = 1)
PERM39 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[39], permutations = 1)
PERM40 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[40], permutations = 1)

# compile into a dataframe
perm_df4 = data.frame(PERM31, PERM32, PERM33, PERM34, PERM35,
                      PERM36, PERM37, PERM38, PERM39, PERM40)
perm_df4 = as.data.frame(t(perm_df4))

readr::write_csv(perm_df4, "objects/perm_df4.csv")

PERM41 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[41], permutations = 1)
PERM42 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[42], permutations = 1)
PERM43 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[43], permutations = 1)
PERM44 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[44], permutations = 1)
PERM45 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[45], permutations = 1)
PERM46 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[46], permutations = 1)
PERM47 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[47], permutations = 1)
PERM48 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[48], permutations = 1)
PERM49 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[49], permutations = 1)
PERM50 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[50], permutations = 1)

# compile into a dataframe
perm_df5 = data.frame(PERM41, PERM42, PERM43, PERM44, PERM45,
                      PERM46, PERM47, PERM48, PERM49, PERM50)
perm_df5 = as.data.frame(t(perm_df5))

readr::write_csv(perm_df5, "objects/perm_df5.csv")

PERM51 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[51], permutations = 1)
PERM52 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[52], permutations = 1)
PERM53 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[53], permutations = 1)
PERM54 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[54], permutations = 1)
PERM55 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[55], permutations = 1)
PERM56 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[56], permutations = 1)
PERM57 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[57], permutations = 1)
PERM58 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[58], permutations = 1)
PERM59 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[59], permutations = 1)
PERM60 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[60], permutations = 1)

# compile into a dataframe
perm_df6 = data.frame(PERM51, PERM52, PERM53, PERM54, PERM55,
                      PERM56, PERM57, PERM58, PERM59, PERM60)
perm_df6 = as.data.frame(t(perm_df6))

readr::write_csv(perm_df6, "objects/perm_df6.csv")

PERM61 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[61], permutations = 1)
PERM62 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[62], permutations = 1)
PERM63 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[63], permutations = 1)
PERM64 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[64], permutations = 1)
PERM65 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[65], permutations = 1)
PERM66 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[66], permutations = 1)
PERM67 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[67], permutations = 1)
PERM68 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[68], permutations = 1)
PERM69 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[69], permutations = 1)
PERM70 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[70], permutations = 1)

# compile into a dataframe
perm_df7 = data.frame(PERM61, PERM62, PERM63, PERM64, PERM65,
                      PERM66, PERM67, PERM68, PERM69, PERM70)
perm_df7 = as.data.frame(t(perm_df7))

readr::write_csv(perm_df7, "objects/perm_df7.csv")

PERM71 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[71], permutations = 1)
PERM72 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[72], permutations = 1)
PERM73 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[73], permutations = 1)
PERM74 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[74], permutations = 1)
PERM75 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[75], permutations = 1)
PERM76 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[76], permutations = 1)
PERM77 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[77], permutations = 1)
PERM78 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[78], permutations = 1)
PERM79 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[79], permutations = 1)
PERM80 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[80], permutations = 1)

# compile into a dataframe
perm_df8 = data.frame(PERM71, PERM72, PERM73, PERM74, PERM75,
                      PERM76, PERM77, PERM78, PERM79, PERM80)
perm_df8 = as.data.frame(t(perm_df8))

readr::write_csv(perm_df8, "objects/perm_df8.csv")


PERM81 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[81], permutations = 1)
PERM82 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[82], permutations = 1)
PERM83 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[83], permutations = 1)
PERM84 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[84], permutations = 1)
PERM85 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[85], permutations = 1)
PERM86 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[86], permutations = 1)
PERM87 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[87], permutations = 1)
PERM88 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[88], permutations = 1)
PERM89 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[89], permutations = 1)
PERM90 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[90], permutations = 1)

# compile into a dataframe
perm_df9 = data.frame(PERM81, PERM82, PERM83, PERM84, PERM85,
                      PERM86, PERM87, PERM88, PERM89, PERM90)
perm_df9 = as.data.frame(t(perm_df9))

readr::write_csv(perm_df9, "objects/perm_df9.csv")

PERM91 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[91], permutations = 1)
PERM92 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[92], permutations = 1)
PERM93 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[93], permutations = 1)
PERM94 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[94], permutations = 1)
PERM95 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[95], permutations = 1)
PERM96 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[96], permutations = 1)
PERM97 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[97], permutations = 1)
PERM98 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[98], permutations = 1)
PERM99 = partwise_PERMANOVA(environment_occurrences, selected_species = allsp[99], permutations = 1)


# compile into a dataframe
perm_df10 = data.frame(PERM91, PERM92, PERM93, PERM94, PERM95,
                      PERM96, PERM97, PERM98, PERM99)
perm_df10 = as.data.frame(t(perm_df10))

readr::write_csv(perm_df10, "objects/perm_df10.csv")


## compile datasets, DO Analysis ###
perm1 = readr::read_csv("objects/perm_df1.csv")
perm2 = readr::read_csv("objects/perm_df2.csv")
perm3 = readr::read_csv("objects/perm_df3.csv")
perm4 = readr::read_csv("objects/perm_df4.csv")
perm5 = readr::read_csv("objects/perm_df5.csv")
perm6 = readr::read_csv("objects/perm_df6.csv")
perm7 = readr::read_csv("objects/perm_df7.csv")
perm8 = readr::read_csv("objects/perm_df8.csv")
perm9 = readr::read_csv("objects/perm_df9.csv")
perm10 = readr::read_csv("objects/perm_df10.csv")

perms = rbind(perm1, perm2, perm3, perm4, perm5, perm6,
                   perm7, perm8, perm9, perm10)
perms[perms == "NaN"] <- NA
perms = perms |>
    dplyr::rename(
        species = V1,
        moisture_expl = V2,
        moisture_resid = V3,
        temp_expl = V4,
        temp_resid = V5,
        soil_expl = V6,
        soil_resid = V7
    ) 

perms = perms |>
    dplyr::mutate_at(c("moisture_expl", "moisture_resid",
                       "temp_expl", "temp_resid",
                       "soil_expl", "soil_resid"), as.numeric)

str(perms)

# add counts to the dataframe
counts = pivot_wider(counts, names_from = "lifestage", values_from = "n")
perms = merge(perms, counts, by = "species")
NAs = perms %>%
    filter(!complete.cases(.) | Gametophyte <3 | Sporophyte <3)
complete = perms %>%
    filter(complete.cases(.), Gametophyte >= 3, Sporophyte >= 3)

# calculate full model statistics
complete = complete |>
    dplyr::mutate(
        full_expl = moisture_expl + temp_expl + soil_expl,
        full_resid = moisture_resid + temp_resid + soil_resid
    )

complete = complete |>
    dplyr::mutate(
        proportion_expl = full_expl/full_resid,
        moist_prop_expl = moisture_expl/moisture_resid,
        temp_prop_expl = temp_expl/temp_resid,
        soil_prop_expl = soil_expl/soil_resid
    )
hist(complete$proportion_expl)
hist(complete$temp_prop_expl)
hist(complete$moist_prop_expl)
hist(complete$soil_prop_expl)
complete$species_2 = sub(" ", "_", complete$species)


complete2 = complete[-c(13, 32),]

# fern phylogeny
library(devtools)
#install_github("fernphy/ftolr")
library(ftolr)
fernphy = ft_tree()
library(ape)
library(phytools)

pruned.tree <- drop.tip(fernphy,
                      fernphy$tip.label[-match(complete2$species_2, fernphy$tip.label)])
par(mfrow = c(1,1), 
    mar = c(1,1,1,1))
plot(pruned.tree)

tree = bind.tip(pruned.tree, tip.label = "Callistopteris_baueriana", where = which(pruned.tree$tip.label=="Callistopteris_baldwinii"), edge.length = 1)
plot(tree)
tree = bind.tip(tree, tip.label = "Polyphlebium_borbonicum", where = which(tree$tip.label=="Polyphlebium_endlicherianum"), edge.length = 1)
plot(tree)

matrix = as.matrix(complete[, c(12:15)])

matrix[matrix > .5]<-.5
matrix

row.names(matrix) = complete$species_2
jpeg("objects/phylogeny_permanova.jpeg", quality = 100, width = 8.5,
     height = 5, units = "in", res = 600)

phylo.heatmap(tree, matrix, fsize=c(0.55,0.5,0.01), split = c(.75, .25),
              labels = F, )

dev.off()


##### Figure 2: Individual Variable Comparisons #####

# group by species
# standardize columns so sporophyte mean equals 0
# plot and sporophyte gametophyte values histogram

# standardize each species by sporophyte values
# subtract sporophyte mean from gametophyte mean
# divide by gametophyte standard deviation

lifestage_standard = environment_occurrences %>%
    dplyr::group_by(species2, lifestage) %>%
    summarise_all(.funs = c(mean, sd))

gameto_means = lifestage_standard %>%
    dplyr::filter(lifestage == "Gametophyte")
sporo_means = lifestage_standard %>%
    dplyr::filter(lifestage == "Sporophyte")

means = merge(gameto_means, sporo_means, by = "species2") |>
    rename(
        species = species2
    )
means = merge(means, complete,  by = "species")

# mean annual temp, temp seasonality, PET
# total annual precip, dry season precip, precip seasonality, drought_index
# bulk density, pH
differences = means %>%
    mutate(bio1 = (bio1_fn1.x - bio1_fn1.y) / bio1_fn2.y,
           bio4 = (bio4_fn1.x - bio4_fn1.y) / bio4_fn2.y,
           PET = (current_2.5arcmin_annualPET_fn1.x - current_2.5arcmin_annualPET_fn1.y) / current_2.5arcmin_annualPET_fn2.y,
           vpd = (vpd_fn1.x - vpd_fn1.y) / vpd_fn2.y,
           bio12 = (bio12_fn1.x - bio12_fn1.y) / bio12_fn2.y,
           bio17 = (bio17_fn1.x - bio17_fn1.y) / bio17_fn2.y,
           bio15 = (bio15_fn1.x - bio15_fn1.y) / bio15_fn2.y,
           drought_index = (drought_index_fn1.x - drought_index_fn1.y) / drought_index_fn2.y,
           bd = (topsoil.bulk.density_fn1.x - topsoil.bulk.density_fn1.y) / topsoil.bulk.density_fn2.y,
           pH = (topsoil.pH.in.water_fn1.x - topsoil.pH.in.water_fn1.y) / topsoil.pH.in.water_fn2.y,
           bio1sd = bio1_fn2.x / bio1_fn2.y,
           bio4sd = bio4_fn2.x / bio4_fn2.y,
           PETsd = current_2.5arcmin_annualPET_fn2.x / current_2.5arcmin_annualPET_fn2.y,
           vpdsd = vpd_fn2.x / vpd_fn2.y,
           bio12sd = bio12_fn2.x / bio12_fn2.y,
           bio17sd = bio17_fn2.x / bio17_fn2.y,
           bio15sd = bio15_fn2.x / bio15_fn2.y,
           drought_indexsd = drought_index_fn2.x / drought_index_fn2.y,
           bdsd = topsoil.bulk.density_fn2.x / topsoil.bulk.density_fn2.y,
           pHsd = topsoil.pH.in.water_fn2.x / topsoil.pH.in.water_fn2.y)

diffs = differences |>
    dplyr::select(bio1, bio4, PET, vpd, bio12, bio17, bio15, drought_index, bd, pH)

set.seed(1)
t.test(diffs$bio1, y = rnorm(n =10000))
t.test(diffs$bio4, y = rnorm(n =10000))
t.test(diffs$PET, y = rnorm(n =10000))
t.test(diffs$vpd, y = rnorm(n =10000))
t.test(diffs$bio12, y = rnorm(n =10000))
t.test(diffs$bio17, y = rnorm(n =10000))
t.test(diffs$bio15, y = rnorm(n =10000))
t.test(diffs$drought_index, y = rnorm(n =10000))
t.test(diffs$bd, y = rnorm(n = 10000))
t.test(diffs$pH, y = rnorm(n =10000))
# bio1, PET, bio12, bd are significant

diffs = stack(diffs)
diffs$species = differences$species
diffs$var_type = rep(c("temp", "temp", "temp", rep("precip", 5), "soil", "soil"), each = nrow(differences))

sds = differences |>
    dplyr::select(bio1sd, bio4sd, PETsd, vpdsd, bio12sd, bio17sd, bio15sd, drought_indexsd, bdsd, pHsd)
sds = stack(sds)
names(sds)[1] = "sd"
names(sds)[2] = "var"

diffs = cbind(diffs, sds)

# most difference species
topfive = diffs |>
    filter(species == "Crepidomanes schmidtianum" |
               species == "Vandenboschia speciosa" |
               species == "Vittaria graminifolia" |
               species == "Lomariopsis brackenridgei" |
               species == "Hymenophyllum wrightii")
topfive$values = ifelse(topfive$values > 5, 5, topfive$values)



library(ggplot2)

plot_diff = ggplot() +
    geom_boxplot(data = diffs, mapping = aes(x = ind, y = values), 
                 outlier.shape = NA, fill = "grey") +
    scale_y_continuous(limits = c(-5.1, 5.1)) +
    scale_x_discrete(limits=c("bd","pH",
                              "bio12", "bio17", "drought_index", "vpd", "bio15",
                              "PET", "bio4", "bio1"),
                     labels = c("bd" = "Topsoil\nBulk Density", "pH" = "Topsoil pH",
                                "bio12" = "Total Annual\nRainfall",
                                "bio17" = "Driest Qtr\nPrecipitation",
                                "drought_index" = "Drought\nIndex",
                                "vpd" = "VPD",
                                "bio15" = "Precipitation\nSeasonality",
                                "PET" = "Annual\nMean PET",
                                "bio4" = "Temperature\nSeasonality",
                                "bio1" = "Annual Mean\nTemperature")) +
    geom_hline(aes(yintercept = 0), linetype = "dashed") +
    #scale_color_manual(name = "Variable Type", 
     #                  labels = c("Precipitation", "Soil", "Temperature"), 
      #                 values = c("blue", "orange", "red")) +
    geom_pointrange(data = topfive, mapping = aes(x = ind, y = values, ymax = ifelse(values+sd > 5.09, 5.09, values+sd), 
                                                  ymin = ifelse(values-sd < -5.09, -5.09, values-sd), 
                                                  fill = species), 
               pch = 21, size = .5, position = position_jitter(w = 0.2)) +
    scale_fill_manual(name = "Species", 
                      values = c("#edbad1", "#687ace", "#df537e", "#bf9adf", "#43b6aa")) +
    annotate("text", x = 1, y = -5, label = "*", size = 10) +
    annotate("text", x = 3, y = -5, label = "*", size = 10) +
    annotate("text", x = 10, y = -5, label = "*", size = 10) +
    annotate("text", x = 8, y = -5, label = "*", size = 10) +
    coord_flip() +
    theme_bw() +
    labs(x = "Environmental Variable", 
         y = "Standardized Difference") +
    theme(axis.text.y = element_text(size=9),
          legend.position = "right",
          legend.text = element_text(face = "italic", size = 8),
          legend.margin=margin(0,4.5,0,0),
          legend.box.margin=margin(-10,-10,-10,-10))
    
plot_diff

ggsave(filename = "objects/plot_diff.jpg", plot = plot_diff, width = 7, height = 5, units = "in", dpi = 600)

# more seasonal, hotter, drier environments.

##### Figure 3: Facet PCA of Five Focal Species #####
top5 = environment_occurrences |>
    filter(species2 == "Crepidomanes schmidtianum" |
               species2 == "Vandenboschia speciosa" |
               species2 == "Vittaria graminifolia" |
               species2 == "Lomariopsis brackenridgei" |
               species2 == "Hymenophyllum wrightii") |>
    mutate(current_2.5arcmin_aridityIndexThornthwaite = current_2.5arcmin_aridityIndexThornthwaite * -1,
           drought_index = drought_index * -1,
           vpd = vpd * -1)

# run a PCA
library(cowplot)
top5_pca = prcomp(top5[, c(4:47)], center = T, scale = T)

pcs = as.data.frame(top5_pca[["x"]])
pcs$Lifestage = top5$lifestage
pcs$species = top5$species2

crep = ggplot(data = subset(pcs, species == "Crepidomanes schmidtianum"),
              aes(x = PC1, y = PC2, fill = Lifestage)) +
    geom_point(pch = 21, alpha = .7) +
    theme_bw() +
    ggtitle("Crepidomanes schmidtianum") +
    scale_fill_manual(values = c("black", "grey")) +
    scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9, 9)) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9.5, 7.5)) +
    coord_equal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust =.5, face = "italic"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
crep

vand = ggplot(data = subset(pcs, species == "Vandenboschia speciosa"),
              aes(x = PC1, y = PC2, fill = Lifestage)) +
    geom_point(pch = 21, alpha = .7) +
    theme_bw() +
    ggtitle("Vandenboschia speciosa") +
    scale_fill_manual(values = c("black", "grey")) +
    scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9, 9)) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9.5, 7.5)) +
    coord_equal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust =.5, face = "italic"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
vand

vitt = ggplot(data = subset(pcs, species == "Vittaria graminifolia"),
              aes(x = PC1, y = PC2, fill = Lifestage)) +
    geom_point(pch = 21, alpha = .7) +
    theme_bw() +
    ggtitle("Vittaria graminifolia") +
    scale_fill_manual(values = c("black", "grey")) +
    scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9, 9)) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9.5, 7.5)) +
    coord_equal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust =.5, face = "italic"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
vitt

loma = ggplot(data = subset(pcs, species == "Lomariopsis brackenridgei"),
              aes(x = PC1, y = PC2, fill = Lifestage)) +
    geom_point(pch = 21, alpha = .7) +
    theme_bw() +
    ggtitle("Lomariopsis brackenridgei") +
    scale_fill_manual(values = c("black", "grey")) +
    scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9, 9)) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9.5, 7.5)) +
    coord_equal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust =.5, face = "italic"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
loma

hyme = ggplot(data = subset(pcs, species == "Hymenophyllum wrightii"),
              aes(x = PC1, y = PC2, fill = Lifestage)) +
    geom_point(pch = 21, alpha = .7) +
    theme_bw() +
    ggtitle("Hymenophyllum wrightii") +
    scale_fill_manual(values = c("black", "grey")) +
    scale_x_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9, 9)) +
    scale_y_continuous(breaks = c(-6, -3, 0, 3, 6), limits = c(-9.5, 7.5)) +
    coord_equal() +
    theme(axis.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size = 12, hjust =.5, face = "italic"),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))
hyme

rot = top5_pca[["rotation"]]
moist = c(12:19, 28, 29, 31, 42, 44)
# flip aridity index (Thornwaite), drought_index, vpd
temp = c(1:11, 27, 30, 32:41, 43)
soil = c(20:26)

loadcol = c(rep("red", 11), rep("blue", 8), rep("orange", 7), "red",
            "blue", "blue", "red", "blue", rep("red", 10), "blue", "red", "blue")

loadings = autoplot(top5_pca, data = top5, 
                    loadings = T, 
                    loadings.label = F, 
                    shape = F, label = F,
                    loadings.colour = loadcol) +
    theme_bw() +
    coord_equal() +
    ggtitle("Loadings") +
    theme(axis.ticks = element_blank(),
          axis.text = element_blank(),
          #axis.title = element_blank(),
          plot.title = element_text(size = 12, hjust = .5),
          plot.margin = unit(c(0, 0, 0, 0), "cm"))


loadings

pca_plot = plot_grid(crep, vand, vitt, loma, hyme, loadings, 
                     labels = c("A", "B", "C", "D", "E", "F"), vjust = 1) +
    theme(plot.background = element_rect(fill = "white"))
pca_plot
ggsave(plot = pca_plot, filename ="objects/pca_plot.jpg", device = "jpg", width = 7, height = 5, units = "in",
       dpi = 600)
##### Figure 4: Quantifying Overlap and Mapping non-overlapping distributions #####

## letsR
library(letsR)

## with all species, filter out species 100% overlap
environment_occurrences$spec_life = paste(environment_occurrences$species2, environment_occurrences$lifestage)
mat = as.matrix(environment_occurrences[, c(48, 49)])

presab = lets.presab.points(mat, environment_occurrences$spec_life)
summary(presab)

# quantify overlap
overlap = lets.overlap(presab, method = "Chesser&Zink")

row.ind <- grep(" Gametophyte",rownames(overlap),value=TRUE)
col.ind <- match(sub(" Gametophyte"," Sporophyte",row.ind),colnames(overlap))
overlap_df <- data.frame(species=colnames(overlap)[col.ind],
                         overlap=diag(overlap[row.ind,col.ind])
)

# filter out overlap of 1
overlap_df$species = sub(" Sporophyte", "", overlap_df$species)
overlap_filtered = overlap_df |>
    filter(overlap != 1) |>
    rename(species2 = species)

# plot median longitude and latitude
eo_sp = environment_occurrences |>
    group_by(species2, lifestage) |>
    summarise(mlon = median(lon),
              mlat = median(lat))


eo_filtered = merge(overlap_filtered, eo_sp, by = "species2") |>
    rename(Lifestage = lifestage)

# change to spatial object for plotting
coordinates(eo_filtered) = ~ mlon + mlat

library(sfheaders)

pts <- as.matrix(eo_filtered@coords, ncol = 2 )
pts <- cbind(pts, eo_filtered@data)

# add lines
lines = sf_linestring( obj = pts, x = 1, y = 2, linestring_id = 3 )
data(land)
data(World)
tmap_mode("plot")
library(sf)

# plot
gametophytes_map = tm_shape(land) +
    tm_raster("elevation", palette = terrain.colors(10), legend.show = F) +
    tm_legend(show=T) +
    tm_shape(World) +
    tm_borders("white", lwd = .5) +
    tm_layout(bg.color = "#A7C7E7") +
    tm_shape(eo_filtered) +
    tm_dots(col = "Lifestage", palette=c(Gametophyte='black', Sporophyte = 'grey'), 
            stretch.palette = FALSE, 
            size = 0.5,
            shape =21) + 
    tm_shape(lines) +
    tm_lines() +
    #tm_text("id", along.lines = T, size = .5, auto.placement = F) +
    tm_layout(inner.margins=0, 
                    legend.text.size=1,
                    legend.title.size=1.2,
                    legend.position = c(0, 0), 
                    legend.bg.color = "white", legend.bg.alpha=.2, 
                    legend.frame="gray50", 
                    legend.width=1.5, legend.height=.35, 
                    legend.hist.bg.color="gray60", legend.hist.bg.alpha=.5)
gametophytes_map
tmap_save(tm = gametophytes_map, 
          filename = "objects/map_notext.png", 
          height = 4, 
          width = 7, 
          dpi = 300, 
          units = "in")

# put into paint and manually add labels

##### Figure 5: Historical Species Distribution Modelling #####
library(raster)
appa = c("Vittaria_appalachiana", "Vittaria_graminifolia")

appalachiana.tree <- drop.tip(fernphy,
                              fernphy$tip.label[-match(appa, fernphy$tip.label)])
par(mfrow = c(1,1), 
    mar = c(1,1,1,1))
plot(appalachiana.tree)
appalachiana.tree[["edge.length"]]
# edge length: 8.05 my

# present, Last Glacial Maximum, Last Interglacial, Mid-Pleiocene warm period

# present bioclim
present = raster::getData("worldclim", var="bio", res = 2.5)

# younger dryas stadial
ydsbios = list.files(path = "raw-data/YDS_v1_2_5m", pattern="\\.tif$")
ydsbios = paste("raw-data/YDS_v1_2_5m/", ydsbios, sep = "")
yds = raster::stack(ydsbios)

# Last Glacial Maximum
lgmbios = list.files(path = "raw-data/cclgmbi_2-5m", pattern="\\.tif$")
lgmbios = paste("raw-data/cclgmbi_2-5m/", lgmbios, sep = "")
lgm = raster::stack(lgmbios)

# Last interglacial
ligbios = list.files(path = "raw-data/LIG_v1_2_5m", pattern="\\.tif$")
ligbios = paste("raw-data/LIG_v1_2_5m/", ligbios, sep = "")
lig = raster::stack(ligbios)

# pleistocene
plebios = list.files(path = "raw-data/MIS19_v1_r2_5m/2_5min", pattern="\\.tif$")
plebios = paste("raw-data/MIS19_v1_r2_5m/2_5min/", plebios, sep = "")
ple = raster::stack(plebios)

# mid pleiocene warm period
mpwpbios = list.files(path = "raw-data/mPWP_v1_r2_5m/2_5min", pattern ="\\.tif$")
mpwpbios = paste("raw-data/mPWP_v1_r2_5m/2_5min/", mpwpbios, sep = "")
mpwp = raster::stack(mpwpbios)

# crop raster stack, rename layers to be all the same, 
# filter for matching layers, 

# 1, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19
present = present[[c(1, 4, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)]]
names = c("bio1", "bio4", "bio8", "bio9", "bio10",
          "bio11", "bio12", "bio13", "bio14", "bio15",
          "bio16", "bio17", "bio18", "bio19")

yds = yds[[c(1, 14, 18, 19, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]]
for (i in 1:14) {
    yds[[i]] = setNames(yds[[i]], names[i])
}

lgm = lgm[[c(1, 14, 18, 19, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]]
for (i in 1:14) {
    lgm[[i]] = setNames(lgm[[i]], names[i])
}

lig = lig[[c(1, 14, 18, 19, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]]
for (i in 1:14) {
    lig[[i]] = setNames(lig[[i]], names[i])
}

ple = ple[[c(1, 12, 13, 14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]]
for (i in 1:14) {
    ple[[i]] = setNames(ple[[i]], names[i])
}

mpwp = mpwp[[c(1, 12, 13, 14, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)]]
for (i in 1:14) {
    mpwp[[i]] = setNames(mpwp[[i]], names[i])
}

# crop rasters to north and south america...
e = raster::extent(-137.5, -31.5, -59.5, 59.5)

present = raster::crop(present, e)
yds = raster::crop(yds, present)
lgm = raster::crop(lgm, present)
lig = raster::crop(lig, present)
ple = raster::crop(ple, present)
mpwp = raster::crop(mpwp, present)

res(mpwp)
res(yds)
res(lig)
res(lgm)
res(ple)
res(present)
extent(mpwp)
extent(yds)
extent(lig)
extent(lgm)
extent(ple)
extent(present)



# run SDMtune, predict the past distribution, 
# load libraries
library(checkmate)
library(SDMtune)
library(dismo)

# define a function that get's pseudoabsence points
get_backgr = function(
        presence,
        samp_dist,
        samp_num,
        predictors
){
    
    #checks
    checkmate::assert_data_frame(presence)
    checkmate::assert_numeric(samp_dist)
    checkmate::assert_numeric(samp_num)
    
    # selecting background points
    # make into spatial
    coordinates(presence) = ~ lon + lat
    projection(presence) = CRS('+proj=longlat +datum=WGS84')
    
    # circles with a radius of d
    x = circles(presence, d = samp_dist, lonlat = TRUE)
    pol = polygons(x)
    
    # sample randomly from all circles
    samp1 = spsample(pol, samp_num, type = 'random', iter = 25)
    
    # make into dataframe
    background = as.data.frame(samp1)
    
    # return the result
    return(background)
    
}

# get background points
sporo_backgr = get_backgr(vit_gram[,c(48:49)], 200000,
                5000, present)

# build SWD
sporo_SWD = prepareSWD(species = "Vittaria graminifolia", 
                 p = vit_gram[, c(48:49)], 
                 a = sporo_backgr, 
                 env = present)

# get background points for gametophyte
gameto_backgr = get_backgr(vit_appa[,c(48:49)], 200000,
                           5000, present)

# build SWD
gameto_SWD = prepareSWD(species = "Vittaria appalachiana", 
                       p = vit_appa[, c(48:49)], 
                       a = gameto_backgr, 
                       env = present)

# model tuning function
# maxent only
model_tune_maxent = function(
        SWD_object,
        threshold,
        predictors
){
    
    # checks
    checkmate::assert_numeric(threshold)
    
    # split model into train and test
    list = trainValTest(SWD_object, test = 0.2, 
                        only_presence = TRUE, seed = 25)
    
    # define train and test
    train = list[[1]]
    test = list[[2]]
    
    # run default model
    default_model = SDMtune::train(method = "Maxent", data = train)
    
    # tune model by removing correlated variables using jacknife removal
    reduced_variables_model <- reduceVar(default_model, th = threshold, metric = "auc", 
                                         test = test, permut = 1, use_jk = T)
    
    # extract new SWD_object from model
    new_SWD = reduced_variables_model@data
    
    # recover testing data via a merge
    merged_SWD = SDMtune::mergeSWD(new_SWD, test, only_presence = TRUE)
    
    # use merged SWD to tune hyperparameters
    # split into training, validation, and testing sets
    reduced_list = trainValTest(merged_SWD, val = 0.2, test = 0.2, 
                                only_presence = TRUE, seed = 61516)
    
    # define train, validation, and test
    reduced_train = reduced_list[[1]]
    reduced_validation = reduced_list[[2]]
    reduced_test = reduced_list[[3]]
    
    # run a model
    maxent_model <- SDMtune::train("Maxent", data = reduced_train)
    
    # define hyperparameters by which to tune
    h <- list(fc = c("l", "lq", "lh", "lqp", "lqph", "lqpht"), 
              iter = seq(300, 1100, 200), 
              reg = seq(0.2, 2, 0.2))
    
    # optimize
    optimum_params = optimizeModel(maxent_model, hypers = h, metric = "auc", 
                                   test = reduced_validation, 
                                   pop = 15, gen = 2, seed = 798)
    
    # build new model, collapsing validation back into training
    index <- which.max(optimum_params@results$test_AUC)  # Index of the best model in the experiment
    train_val <- mergeSWD(reduced_train, reduced_validation, only_presence = TRUE)
    
    final_model <- SDMtune::train("Maxent", data = train_val, 
                                  fc = optimum_params@results[index, 1], 
                                  reg = optimum_params@results[index, 2],
                                  iter = optimum_params@results[index, 3])
    
    # return tuned maxent model
    final_list = list(train_val, reduced_test, final_model)
    return(final_list)
}

# don't remove predictor variables
model_tune_maxent_no_remove = function(
        SWD_object,
        predictors
){
  
    # split model into train and test
    list = trainValTest(SWD_object, test = 0.2, 
                        only_presence = TRUE, seed = 25)
    
    # define train and test
    train = list[[1]]
    test = list[[2]]
    
    # run default model
    default_model = SDMtune::train(method = "Maxent", data = train)
    
    # define hyperparameters by which to tune
    h <- list(fc = c("l", "lq", "lh", "lqp", "lqph", "lqpht"), 
              iter = seq(300, 1100, 200), 
              reg = seq(0.2, 2, 0.2))
    
    # optimize
    optimum_params = optimizeModel(default_model, hypers = h, metric = "auc", 
                                   test = test, 
                                   pop = 15, gen = 2, seed = 798)
    
    # build new model, collapsing validation back into training
    index <- which.max(optimum_params@results$test_AUC)  # Index of the best model in the experiment
    
    final_model <- SDMtune::train("Maxent", data = train, 
                                  fc = optimum_params@results[index, 1], 
                                  reg = optimum_params@results[index, 2],
                                  iter = optimum_params@results[index, 3])
    
    # return tuned maxent model
    final_list = list(train, test, final_model)
    return(final_list)
}


sporo_model = model_tune_maxent(sporo_SWD, 1, present)

gameto_model = model_tune_maxent_no_remove(gameto_SWD, present)

# write an rds object
readr::write_rds(sporo_model, "objects/sporo_model.rds")
readr::write_rds(gameto_model, "objects/gameto_model.rds")

# evaluate models
evaluate_model = function(
        model_tune_out
){
    
    # checks
    checkmate::assert_list(model_tune_out)
    
    # define
    test = model_tune_out[[2]]
    model = model_tune_out[[3]]
    species = model@data@species

    # calculate auc
    auc = SDMtune::auc(model, test = test)
    
    # calculate tss
    tss = SDMtune::tss(model, test = test)
    
    # return species and model evaluations
    # make a list
    list = list(species, auc, tss)
    
    return(list)
    
}

# reload in models
sporo_model = readr::read_rds("objects/sporo_model.rds")
gameto_model = readr::read_rds("objects/gameto_model.rds")

sporo_eval = evaluate_model(sporo_model)
gameto_eval = evaluate_model(gameto_model)

# retrain model with full dataset

retrain_model = function(
        model_tune_out
){
    
    # checks
    checkmate::assert_list(model_tune_out)
    
    # define
    train = model_tune_out[[1]]
    test = model_tune_out[[2]]
    model = model_tune_out[[3]]
    
    # merge
    all_occ = mergeSWD(train, test)
    
    # model class
    class = class(model@model)
    
    # if else statement to retrain model
    if (class == "Maxnet") {
        # retrain
        final_model <- SDMtune::train("Maxnet", data = all_occ, 
                                      fc = model@model@fc, 
                                      reg = model@model@reg)
        
        # return all_occ and final_model
        # make a list
        list = list(final_model, all_occ)
        
        return(list)
    } else {
        
        # retrain
        final_model <- SDMtune::train("Maxent", data = all_occ, 
                                      fc = model@model@fc, 
                                      reg = model@model@reg,
                                      iter = model@model@iter)
        
        # return all_occ and final_model
        # make a list
        list = list(final_model, all_occ)
        
        return(list)
    }
    
}

# a function that projects the model onto a certain climate

predict_model = function(
        retrained_out,
        predictors
){
    
    # checks
    checkmate::assert_list(retrained_out)
    
    # define
    model = retrained_out[[1]]
    env = get_env(model, predictors)
    species = model@data@species
    
    # predict
    # prediction
    pred = SDMtune::predict(model, data = env, type = "cloglog")
    
    ths = thresholds(model, type = "cloglog")
    ths_val = ths[2,2]
    
    reclass_df = c(0, ths_val, 0,
                   ths_val, 1, 1)
    reclass_m = matrix(reclass_df,
                       ncol = 3,
                       byrow = TRUE)
    
    # reclassify the raster using the reclass object - reclass_m
    presence_absence = reclassify(pred, reclass_m)
    
    # return binary and prediction
    # make a list
    list = list(species, pred, presence_absence)
    
    return(list)
    
}
# a function that get the environment for any model

get_env = function(
        model,
        predictors
){
    
    # get layers
    layers = names(model@data@data)
    
    # subset rasterStack
    env = raster::subset(predictors, subset = layers)
    
    # return
    return(env)
    
}


# retrain and predict models
sporo_retrain = retrain_model(sporo_model)
gameto_retrain = retrain_model(gameto_model)

# present
sporo_present = predict_model(sporo_retrain, present)
gameto_present = predict_model(gameto_retrain, present)

# younger dryas stadial
sporo_yds = predict_model(sporo_retrain, yds)
gameto_yds = predict_model(gameto_retrain, yds)

# lgm
sporo_lgm = predict_model(sporo_retrain, lgm)
gameto_lgm = predict_model(gameto_retrain, lgm)

# lig
sporo_lig = predict_model(sporo_retrain, lig)
gameto_lig = predict_model(gameto_retrain, lig)

# ple
sporo_ple = predict_model(sporo_retrain, ple)
gameto_ple = predict_model(gameto_retrain, ple)

# mpwp
sporo_mpwp = predict_model(sporo_retrain, mpwp)
gameto_mpwp = predict_model(gameto_retrain, mpwp)

# gameto_sporo rasters
gs_present = calc(stack(sporo_present[[3]], (gameto_present[[3]] * 2)), sum)

gs_yds = calc(stack(sporo_yds[[3]], (gameto_yds[[3]] * 2)), sum)

gs_lgm = calc(stack(sporo_lgm[[3]], (gameto_lgm[[3]] * 2)), sum)

gs_lig = calc(stack(sporo_lig[[3]], (gameto_lig[[3]] * 2)), sum)

gs_ple = calc(stack(sporo_ple[[3]], (gameto_ple[[3]] * 2)), sum)

gs_mpwp = calc(stack(sporo_mpwp[[3]], (gameto_mpwp[[3]] * 2)), sum)


# plot and quantify overlap
library(tmap)
library(tmaptools)
library(sf)
# plot graminifolia points
coordinates(vit_gram) = ~ lon + lat

# convex hull of gametophyte points
gameto.sf <- vit_appa %>%
    st_as_sf( coords = c( "lon", "lat" ))
gameto_convex = gameto.sf %>%
    summarise( geometry = st_combine( geometry ) ) %>%
    st_convex_hull()
mapview::mapview( list( gameto.sf, gameto_convex ) )

coordinates(vit_appa) = ~ lon + lat


pal_sporo = c( "#FFFAA0", "#C7C7C7")
present_map = tm_shape(sporo_present[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    tm_shape(vit_gram) +
    tm_symbols(col = "grey", shape = 21, size = .05, border.col = "black", alpha = .5) +
    tm_shape(vit_appa) +
    tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7") +
    tm_scale_bar(breaks = c(0, 1000, 2000), 
                 text.size = 22, 
                 position = c("left", "bottom"))
present_map

yds_map = tm_shape(sporo_yds[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    #tm_shape(vit_gram) +
   # tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
yds_map



# lgm
lgm_map = tm_shape(sporo_lgm[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
lgm_map

# lig
lig_map = tm_shape(sporo_lig[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
lig_map

# ple
ple_map = tm_shape(sporo_ple[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
ple_map

# mpwp
mpwp_map = tm_shape(sporo_mpwp[[3]], bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal_sporo) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
mpwp_map

all_climates_sporo = tmap_arrange(present_map, yds_map, lgm_map, lig_map, ple_map, mpwp_map, nrow = 2, outer.margins = c(0.05,.05,0.05,.05))

all_climates_sporo

tmap_save(tm = all_climates_sporo, 
          filename = "objects/all_climates_sporo.png", 
          height = 4, 
          width = 7, 
          dpi = 600, 
          units = "in")
# overlap with refugia sites and past predicted sites...

library(RColorBrewer)

pal = get_brewer_pal("Greys", n = 4)
pal = c( "#FFFAA0", pal[2], "black", pal[4])
pal[2]

gs_present_map = tm_shape(gs_present, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_present_map

# yds
gs_yds_map = tm_shape(gs_yds, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_yds_map

# lgm

gs_lgm_map = tm_shape(gs_lgm, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_lgm_map

# lig
gs_lig_map = tm_shape(gs_lig, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_lig_map

# ple
gs_ple_map = tm_shape(gs_ple, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_ple_map

# mpwp
gs_mpwp_map = tm_shape(gs_mpwp, bbox=tmaptools::bb(matrix(c(-110,-37, -31, 49),2,2))) +
    tm_raster(palette = pal) +
    #tm_shape(vit_gram) +
    #tm_dots(col = "grey", shape = 19) +
    #tm_shape(vit_appa) +
    #tm_symbols(col = "black", shape = 21, size = .1, border.col = "black", alpha = 0.5) +
    tm_shape(gameto_convex) +
    tm_polygons(col = "black", alpha = .5) +
    tm_layout(legend.show=FALSE, bg.color = "#A7C7E7")
gs_mpwp_map

all_climates_gs = tmap_arrange(gs_present_map, gs_yds_map, gs_lgm_map, gs_lig_map, gs_ple_map, gs_mpwp_map, nrow = 2, outer.margins = c(0.1,.1,0.1,.1))

all_climates_gs

tmap_save(tm = all_climates_gs, 
          filename = "objects/all_climates_gs.png", 
          height = 4, 
          width = 7, 
          dpi = 600, 
          units = "in")

sporo_gs_map = tmap_arrange(present_map, gs_present_map,
                            yds_map, gs_yds_map,
                            lgm_map, gs_lgm_map,
                            lig_map, gs_lig_map,
                            ple_map, gs_ple_map,
                            mpwp_map, gs_mpwp_map, nrow = 3,
                            outer.margins = c(0.08,0,0.08,0))
tmap_save(tm = sporo_gs_map, 
          filename = "objects/sporo_gs_map.png", 
          height = 8.5, 
          width = 7, 
          dpi = 600, 
          units = "in")


##### Table 1: Suitable Area and Overlap through Time #####
# quantify overlap, gametophyte only, sporophyte only
# calculate area at each time

# for loop
# set up vector to iterate through
gs_rasters = c(gs_present, gs_yds, gs_lgm, 
               gs_lig, gs_ple, gs_mpwp)



# calculate cell size in km^2
cell_size = raster::area(gs_present)
cell_size = raster::values(cell_size)

# set up storage
gs_data = matrix(nrow = 6, ncol = 3)

for ( i in 1:6 ) {
    
    # extract vals
    vals = raster::values(gs_rasters[[i]])
    
    area = ifelse(vals == 2, vals * cell_size, 0)
    
    # gametophyte only area
    gs_data[i, 1] = sum(area, na.rm = T)
    
    # sporophyte only area
    area = ifelse(vals == 1, vals * cell_size, 0)
    
    # put into matrix
    gs_data[i, 2] = sum(area, na.rm = T)
    
    # overlap
    area = ifelse(vals == 3, vals * cell_size, 0)
    
    # put into matrix
    gs_data[i, 3] = sum(area, na.rm = T)
}


gs_df = as.data.frame(gs_data) |>
    rename(
        gametophyte = V1,
        sporophyte = V2,
        overlap = V3
    ) |>
    dplyr::mutate(climate = c("Present", 
                              "Younger Dryas",
                              "Last Glacial Maximum",
                              "Last Interglacial",
                              "Pleistocene",
                              "mid-Pliocene warm period")) |>
    dplyr::select(climate, gametophyte, sporophyte, overlap)

# write into a table .txt file and import into Word
write.table(gs_df, file = "objects/table1.txt", sep = ",",
            quote = F, row.names = F)

