##############################################################################
##
## Get degree of urbanization of your study participants' place of residence
##
##############################################################################


# Load necessary packages -------------------------------------------------

if (!require("pacman")) install.packages("pacman")

pacman::p_load(rgdal, readxl, ggplot2, googleway, pbapply)



############################################################################
#
# Here, your input is necessary 
#
############################################################################



# Define your working directory (where you store your the file with the participants addresses)
# Be sure to use slashes (/) and not backslashes (\).

setwd("C:/Users/Elias/OneDrive/01_work/02_R/02_projects/git_insci_geo_location")


# Enter the name of your dataset 

my_addresses <- "example_data_DE.csv"


# Enter the separately provided key betweeen the parentheses

google_maps_API_key <- "API_key"


# Select your country from the list below and replace Germany --------

participants <- c(Germany = "DE")

# participants <- c(Spain = "ES",
#                   Portugal = "PT",
#                   Lithuania = "LI",
#                   Italy = "IT",
#                   Greece = "EL",
#                   Germany = "DE",
#                   France = "FR",
#                   Switzerland = "CH",
#                   Poland = "PL",
#                   Norway = "NO",
#                   Romania = "RO"
# )


############################################################################
#
# That is it. The rest should work without your assistance
#
############################################################################



# Reads address data of InSCI participants ---------------------------------


# Replace the example file with your csv file with the participants addresses.

addr_insci <- read.csv2(file.path(".", my_addresses), encoding = "UTF-8", stringsAsFactors = FALSE)


# make letters lowercase

addr_insci[] <- lapply(addr_insci, tolower)


# Remove "Byte order mark"

colnames(addr_insci) <- gsub("^X.U.FEFF.", "", colnames(addr_insci))




# Load the Degree of Urbanization data from the eurostat homepage ---------

# European Commission > Eurostat > GISCO > Geodata > Reference data > Population Distribution / Demography > DEGURBA
# https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/population-distribution-demography/degurba


# Creates a temporary file and a temporary path on the local computer

temp <- tempfile()
tempd <- tempdir()


# Downloads the degree of urbanization information/maps from the eurostat homepage as a zip file and safe it locally

download.file(
  url = "https://ec.europa.eu/eurostat/cache/GISCO/geodatafiles/DGURBA_2014_SH.zip", 
  destfile = temp, 
  mode = "wb")


# Unzips the file

unzip(temp, exdir = tempd)


# Loads the map file

degurba_shp <- readOGR(file.path(tempd, "DGURBA_2014_SH", "data", "DGURBA_RG_01M_2014.shp"), 
  "DGURBA_RG_01M_2014", stringsAsFactors = FALSE)


# Selects your country only

my_country <- degurba_shp[which(degurba_shp$CNTR_CODE %in% participants), ]


# Changes coordinates system to World Geodetic System  (WGS 84).
# This is the coordnates system that is used by google maps

# https://en.wikipedia.org/wiki/World_Geodetic_System

my_country <- sp::spTransform(my_country, CRS('+init=epsg:4326'))


# Gets municipality names from:
# https://ec.europa.eu/eurostat/web/nuts/local-administrative-units

download.file("https://ec.europa.eu/eurostat/documents/345175/501971/EU-28_2014.xlsx", temp, mode = "wb")

names_regions <- read_excel(temp, sheet = participants)


# Merges this information into

my_country <- sp::merge(my_country, names_regions, by.x = "NSI_CODE", by.y = "LAU2_NAT_CODE", all.x = TRUE)


# Add a google maps readable column to the address file of the participants

addr_insci$addr_google <- paste(addr_insci$zip, addr_insci$place, names(participants), sep = "+")



# Adds geo coordinates ----------------------------------------------------

geocode_results <- pblapply(addr_insci$addr_google, google_geocode, simplify = TRUE, key = google_maps_API_key)

my_coordinates <- sapply(geocode_results, function(x) x[["results"]][["geometry"]][["location"]])

my_coordinates[] <- sapply(my_coordinates , "[[", 1)

addr_insci <- data.frame(addr_insci, t(my_coordinates), stringsAsFactors = FALSE)

addr_insci[, c("lat", "lng")] <- sapply(addr_insci[, c("lat", "lng")], as.numeric)



# Plots InSCI participants addresses in country ---------------------------

# All green dots should be located within the country


ggplot() +
  coord_equal() + 
  geom_polygon(data = my_country, aes(x = long, y = lat, group = group), fill = NA, color = "black") + 
  geom_point(data = addr_insci, aes(x = lng, y = lat), color = "green", size = 4)


# Gets degree of urbanization of participants locations --------------------


# Convert participant's coordinates to spatial points

loc_participants <- SpatialPoints(
  
  cbind(addr_insci$lng,
        addr_insci$lat), 
  
  proj4string = CRS(proj4string(my_country))
  
)


# Finds the regions where the InSCI participants live in the map and extracts the information of these regions

loc_participants <- over(loc_participants, my_country)


# Merges the degubra information back into the address file

addr_insci_degurba <- data.frame(addr_insci, loc_participants, stringsAsFactors = FALSE)


# Reduce the address file to the columns I am interested in

my_vars <- intersect(
  
  names(addr_insci_degurba), 
  
  c("id", "street", "zip", "place", "lng", "lat", 
    "LAU_CODE", "CNTR_CODE", "DGURBA_CLA", 
    "NUTS_3", "NAME_1", "NAME_2_LAT")
)

addr_insci_degurba <- subset(addr_insci_degurba, select = my_vars)


# Recodes degurba from numbers to labels according to:

# https://ec.europa.eu/eurostat/ramon/miscellaneous/index.cfm?TargetUrl=DSP_DEGURBA

addr_insci_degurba$DGURBA_CLA <- ifelse(addr_insci_degurba$DGURBA_CLA == 1, "Cities", 
       ifelse(addr_insci_degurba$DGURBA_CLA == 2, "Towns and suburbs", 
              "Rural areas"))



# Saves the address file with the new information for your personal use-------------------------

write.csv2(addr_insci_degurba, file.path(".", "for_your_own_use.csv"), row.names = FALSE)



# Saves the address file with the new information for research purposes -------------

addr_insci_for_research <- subset(addr_insci_degurba, select = c("id", "CNTR_CODE", "DGURBA_CLA", "NUTS_3"))

write.csv2(addr_insci_for_research, file.path(".", "for_research.csv"), row.names = FALSE)



# Remove temporary files --------------------------------------------------

rm("addr_insci", "addr_insci_degurba", "addr_insci_for_research", 
  "degurba_shp", "geocode_results", "google_maps_API_key", "loc_participants", 
  "my_addresses", "my_coordinates", "my_country", "my_vars", "names_regions", 
  "participants", "temp", "tempd")

