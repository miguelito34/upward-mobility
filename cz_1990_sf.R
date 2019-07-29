# Create boundaries of US 1990 commuting zones in R sf format using US Census
# Bureau shapefiles

# Authors: Bill Behrman, Claudia Engel, and Tyler Walker
# Version: 2018-12-04

# Libraries
library(tidyverse)
library(readxl)
library(sf)
library(maptools)
library(stringr)

# Parameters
  # Boundary year
year <- "2017"
  # Boundary resolutions
resolutions <- c("500k", "5m", "20m")
  # FIPS codes for US states and District of Columbia
fips_states <- c(
  "01", "02", "04", "05", "06", "08", "09", "10", "11", "12", "13", "15", "16",
  "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29",
  "30", "31", "32", "33", "34", "35", "36", "37", "38", "39", "40", "41", "42",
  "44", "45", "46", "47", "48", "49", "50", "51", "53", "54", "55", "56"
)
  # FIPS codes for counties deleted since 1990 census
counties_deleted <- c(
  "02201", "02231", "02270", "02280", "12025", "30113", "46113", "51515",
  "51560", "51780"
)
  # FIPS codes for counties added since 1990 census with 1990 commuting zone
counties_added <- tribble(
  ~fips_county, ~cz_1990,
  "02068", "34115",
  "02105", "34109",
  "02158", "34112",
  "02195", "34110",
  "02198", "34111",
  "02230", "34109",
  "02275", "34110",
  "02282", "34109",
  "08014", "28900",
  "12086", "07000",
  "46102", "27704"
)
  # Rename commuting zones
rename_cz <- c(
  "34103" = "Valdez-Cordova"
)
  # EPSG code for WGS84 coordinate reference system
epsg_wgs84 <- 4326L
  # Temporary directory
tmp <- str_c("/tmp/", Sys.time() %>% as.integer(), "/")
  # URL for US Census Bureau shapefiles
url_cb <- str_c("http://www2.census.gov/geo/tiger/GENZ", year, "/shp/")
  # URL for commuting zone county partition using 1990 counties
url_cz <- "https://www.ers.usda.gov/webdocs/DataFiles/48457/czlma903.xls?v=40961"
  # Data directory
dir_data <- "data/"

#===============================================================================

# Ensure that rgeos is available
if (!rgeosStatus()) {
  stop("Error: rgeos not available")
}

# Create temporary directory
v <- tmp %>% str_sub(end = -2L)
if (!file.exists(v)) {
  dir.create(v, recursive = TRUE)
}

# Download commuting zones county partition
dest <- str_c(tmp, "czlma903.xls")
if (download.file(url = url_cz, destfile = dest, quiet = TRUE)) {
  stop("Error: Commuting zones download failed")
}

# Read commuting zone county partition, add place and state variables
cz <- 
  read_excel(dest, sheet = "CZLMA903", na = ".") %>% 
  select(
    fips_county = contains("FIPS"),
    cz_1990 = CZ90,
    place_state = contains("largest place")
  ) %>% 
  mutate(
    place =
      place_state %>% 
      str_replace(" borough.*| CDP.*| city.*| town.*| \\(rem.*|,.*", ""),
    state = place_state %>% str_sub(start = -2L)
  ) %>% 
  select(-place_state)

# Adjust county partition for counties added and deleted since 1990
v <- 
  counties_added %>% 
  left_join(cz %>% select(-fips_county) %>% distinct(), by = "cz_1990")
cz <- 
  bind_rows(cz, v) %>% 
  filter(!(fips_county %in% counties_deleted))

# Rename commuting zones
for (zone in names(rename_cz)) {
  cz$place[cz$cz_1990 == zone] <- rename_cz[zone]
}
  
# For resolution
for (resolution in resolutions) {
  
  # Boundary specification
  boundary <- str_c("cb_", year, "_us_county_", resolution)
  
  # Download and unzip US Census Bureau shapefile
  url <- str_c(url_cb, boundary, ".zip")
  dest <- str_c(tmp, boundary, ".zip")
  if (download.file(url = url, destfile = dest, quiet = TRUE)) {
    print(str_c("Error: Download for ", boundary, " failed"))
    next
  }
  unzip(zipfile = dest, exdir = str_c(tmp, boundary))
  
  # Read shapefile into sf object, subset to states and District of Columbia,
  # merge counties into communting zones, transform to WGS 1984 coordinate
  # reference system, and write out
  st_read(
    dsn = str_c(tmp, boundary, "/", boundary, ".shp"),
    stringsAsFactors = FALSE
  ) %>%
    filter(STATEFP %in% fips_states) %>% 
    left_join(cz, by = c("GEOID" = "fips_county")) %>%
    group_by(cz_1990, place) %>%
    summarize() %>%
    ungroup() %>% 
    st_cast("MULTIPOLYGON") %>% 
    st_transform(epsg_wgs84) %>%
    write_rds(str_c(dir_data, "cb_", year, "_us_cz_", resolution, "_sf.rds"))
}

# Remove temporary directory
if (unlink(tmp, recursive = TRUE, force = TRUE)) {
  print("Error: Remove temporary directory failed")
}
