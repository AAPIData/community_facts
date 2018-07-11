# Import all packages.
# Make sure you run install_script.R before this script.
library(censusapi)
library(tidyverse)
library(tidycensus)
library(leaflet)
library(sf)
library(stringr)
library(rlist)

# Macros that might come be handy. Redefine as necessary.
ACS5 = "acs/acs5"

# Functions for searching.
# Use case 1: name -- If you have a table name but don't know its suffixes
# Use case 2: concept
# Use case 3: label
search_variable <- function(census, year, query, varType) {
  result <- makeVarlist(name = census, vintage = year, find = query, varsearch = varType)
  return(result)
}

# Get all matching results within a group (good for when there are many subgroups)
getGroupResults <- function(ID, census, year, locality){
  groupName <- paste('"',"group(",ID,')"',sep="")
  vars <- getMatchingTables(ID)
  results <- getCensus(name=census, vintage=year, var=vars, region=locality)
  return(results)
}

# Get just one matching result (good for when you want to be specific)
getSingleResults <- function(ID, census, year, locality){
  results <- getCensus(name=census, vintage= year, var=c("NAME",ID), region=locality)
  return(results)
}
