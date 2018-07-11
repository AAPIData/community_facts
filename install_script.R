# This is a separate script to intall and import all packages.
# Run this while setting up your environment.

# Install libraries for census api
install.packages("censusapi")
install.packages("tidyverse")
install.packages("tidycensus")
install.packages("leaflet")
install.packages("sf")
install.packages("stringr")

# Install census_api_key
key <- '74108d0900e2a5203b62b06e67d4da51a66809c1'
Sys.setenv(CENSUS_KEY=key)
readRenviron("~/.Renviron")
Sys.getenv("CENSUS_KEY")
census_api_key(key, install=TRUE)

# Other notes: Make sure XQuartz is also installed if you are on a Mac.
