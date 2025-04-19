# HEADER========================================================================
# Title: histLdfPop
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: read in FAO Food Balance Sheets (FBS), population, and GDP
# to generate figures and tables for
# livestock-derived foods (LDF) trends, including association with human population and GDP
# http://www.fao.org/faostat/en/#faq writes
# "FBS measures a food availability instead of real consumption." (accessed October 1st 2019)
# the word "demand" is used to be internally consistent in the current study,
# although "demand" is "food availability" and not real consumption
# cty = country, reg = region
# only read in country data
# then add IMPACT region labels to do region aggregations
# ldf = livestock-derived food


# LIBRARIES=====================================================================
library("readxl")
library("data.table")

# disable scientific notation
options(scipen = 999)

# COUNTRY/REG NAMES=============================================================

faoCtyReg_dt <- readRDS(file = "procData_faoCtyReg_dt.Rda")

# READ GENUS PROTEIN CONTENT====================================================
# grams of protein per 100 g food for each ldf in a cty and reg

# cty
genusCtyProt_dt <- fread("rawData_genusCty_dt.csv")
genusCtyProt_dt$kcalPerKgGenus <- NULL

# reg
genusRegProt_dt <- fread("rawData_genusReg_dt.csv")
genusRegProt_dt$kcalPerKgGenus <- NULL


# READ DATA FOR LDF & POP CTY===================================================
# data downloaded as csv files from http://www.fao.org/faostat/en/#data Food Balance on Feb. 6, 2020
# use direct download of needed data using ISO3 codes for each country
# cty ldf demand and pop with ISO3
# ldf supply per person (kg and protein)
# and population for all cty

# 1961 to 2013
ldfPopCtyHist19612013_dt <- fread("rawData_FAOSTAT_data_2-6-2020 (1).csv")

# 2014 to 2017

# downloaded from FAOSAT on Feb. 2, 2020
ldfPopCtyHist20142017_old_dt <- fread("rawData_FAOSTAT_data_2-6-2020.csv")

# downloaded from FAOSTAT on Jun, 29. 2020
ldfPopCtyHist20142017_dt <- fread("rawData_FAOSTAT_data_6-29-2020.csv")
ldfPopCtyHist20142017_dt$Note <- NULL


# adjust China naming:
# ldfPopCtyHist20142017_dt CHN is all china and CPR is China, mainland
# ldfPopCtyHist19612013_dt CPR is all china and CHN is China, mainland
# must be consistent in both and CHN is mainland China and CPR is all China
# all China here means mainland China + Hong Kong SAR + Macao SAR + Taiwan 
ldfPopCtyHist20142017_dt[Area == "China, mainland", `Area Code` := "CHN"]
ldfPopCtyHist20142017_dt[Area == "China", `Area Code` := "CPR"]

# adjust GBR naming:
# ldfPopCtyHist20142017_dt GBR is United Kingdom of Great Britain and Northern Ireland
# ldfPopCtyHist19612013_dt GBR is United Kingdom
# must be consistent in both and should be United Kingdom of Great Britain and Northern Ireland
ldfPopCtyHist19612013_dt[Area == "United Kingdom", Area := "United Kingdom of Great Britain and Northern Ireland"]


# "rawData_FAOSTAT_data_6-29-2020.csv" doesn't include population data for LAO or UZB
# for 2014 to 2017
# but the older version rawData_FAOSTAT_data_2-6-2020 does.

# so append these data
laoUzbPop_dt <- ldfPopCtyHist20142017_old_dt[Item == "Population" &
  (`Area Code`== "LAO" | `Area Code`== "UZB")]

# stack laoUzbPop_dt ontop of ldfPopCtyHist20142017_dt so all data in one file 
ldfPopCtyHist20142017_dt <- rbind(ldfPopCtyHist20142017_dt, laoUzbPop_dt)

# bind ldf and pop data
ldfPopCtyHist_dt <- rbind(ldfPopCtyHist19612013_dt,
                          ldfPopCtyHist20142017_dt)

# change "Item" names for the ldf and human population
# 6 ldf: 
# IMPACT has six ldf commodities: here are the commodity names
# with the IMPACT commodity code in parentheses after the commodity name:
# 1) beef cattle meat (cbeef),
# 2) eggs (ceggs), 
# 3) sheep and goat meat (clamb)
# 4) milk from dairy cattle (cmilk), 
# 5) pig meat (cpork), 
# 6) poultry meat (cpoul).

# match these 6 IMPACT ldf to the FAO Items as follows:

# cbeef (in IMPACT) is Bovine Meat (in FAOSTAT FBS Item list)
# ceggs is Eggs
# clamb is Mutton & Goat Meat
# cmilk is Milk - Excluding Butter
# cpork is Pigmeat

# rename "Item" so match IMPACT names
ldfPopCtyHist_dt[Item == "Bovine Meat", Item := "Beef"]
ldfPopCtyHist_dt[Item == "Mutton & Goat Meat", Item := "Sheep"]
ldfPopCtyHist_dt[Item == "Pigmeat", Item := "Pork"]
ldfPopCtyHist_dt[Item == "Milk - Excluding Butter", Item := "Milk"]
ldfPopCtyHist_dt[Item == "Poultry Meat", Item := "Poultry"]

# rename Elements
ldfPopCtyHist_dt[Element == "Food supply quantity (kg/capita/yr)", Element := "kgPersYr"]
ldfPopCtyHist_dt[Element == "Protein supply quantity (g/capita/day)", Element := "protGPersDay"]
ldfPopCtyHist_dt[Element == "Total Population - Both sexes", Element := "pop000Peop"]

# keep relevant columns
ldfPopCtyHist_dt <- ldfPopCtyHist_dt[, c("Area Code",
                                         "Area",
                                         "Element",
                                         "Item",
                                         "Year",
                                         "Value")]


# "protGPersDay" into kg per year so in same units as food
ldfPopCtyHist_dt[Element == "protGPersDay", Value := (Value * 365) / 1000]

# rename Elements
ldfPopCtyHist_dt[Element == "kgPersYr", Element := "kgFoodPersYr"]
ldfPopCtyHist_dt[Element == "protGPersDay", Element := "kgProtPersYr"]


# rename columns
colnames(ldfPopCtyHist_dt) <- c("isoCty",
                                "faoCountry",
                                "element",
                                "item",
                                "year",
                                "value")

# year as character
ldfPopCtyHist_dt$year <- as.character(ldfPopCtyHist_dt$year)


# remove CPR as China is listed as an aggregate "CPR"
# and as individual iso3 units (CHN, HKG, MAC, TWN)
# keep the 4 individual units
ldfPopCtyHist_dt <- ldfPopCtyHist_dt[isoCty != "CPR", ]

# PER PERSON LDF-CTY============================================================
# data for per person ldf demand by cty

# drop population
ldfCtyPpsHist_dt <- ldfPopCtyHist_dt[item != "Population", ]


# rename columns
names(ldfCtyPpsHist_dt)[names(ldfCtyPpsHist_dt) == "item"] <- "ldf"
names(ldfCtyPpsHist_dt)[names(ldfCtyPpsHist_dt) == "element"] <- "ldfUnit"

# ldf unit named as food or protein 
ldfCtyPpsHist_dt[ldfUnit == "kgFoodPersYr", ldfUnit := "food"]
ldfCtyPpsHist_dt[ldfUnit == "kgProtPersYr", ldfUnit := "protein"]

# rename "value" to demand 
names(ldfCtyPpsHist_dt)[names(ldfCtyPpsHist_dt) == "value"] <- "demaPerPersKg"

# long to wide
# so can add protein from GENUS

ldfCtyPpsHist_dt <- dcast(ldfCtyPpsHist_dt,
                          isoCty + faoCountry + ldf + year ~
                            ldfUnit,
                          value.var = "demaPerPersKg")

# merge data so all cty/reg names together
ldfCtyPpsHist_dt <- merge(ldfCtyPpsHist_dt, faoCtyReg_dt,
                       by = c("isoCty", "faoCountry"),
                       all.x = TRUE)

# fill in missing data
# classify by hand the faoCountry that have missing region and WB class
ldfCtyPpsHist_dt[faoCountry == "Belgium-Luxembourg", wbCtyIncClass := "High income"]
ldfCtyPpsHist_dt[faoCountry == "Belgium-Luxembourg", reg := "EUR"]


ldfCtyPpsHist_dt[faoCountry == "Czechoslovakia", wbCtyIncClass := "High income"]
ldfCtyPpsHist_dt[faoCountry == "Czechoslovakia", reg := "EUR"]

ldfCtyPpsHist_dt[faoCountry == "USSR", wbCtyIncClass := "Upper middle income"]
ldfCtyPpsHist_dt[faoCountry == "USSR", reg := "FSU"]

ldfCtyPpsHist_dt[faoCountry == "Yugoslav SFR" |
                faoCountry ==  "Serbia and Montenegro",
              wbCtyIncClass := "Upper middle income"]

ldfCtyPpsHist_dt[faoCountry == "Yugoslav SFR" |
                faoCountry ==  "Serbia and Montenegro",
              reg := "EUR"]

ldfCtyPpsHist_dt[faoCountry == "Ethiopia PDR" |
                faoCountry == "Sudan (former)" |
                isoCty =="CIV",
              wbCtyIncClass := "Low income"]

ldfCtyPpsHist_dt[faoCountry == "Ethiopia PDR" |
                faoCountry == "Sudan (former)" |
                isoCty =="CIV",
              reg := "SSA"]

ldfCtyPpsHist_dt[faoCountry == "United Kingdom of Great Britain and Northern Ireland",
              reg := "EUR"]

ldfCtyPpsHist_dt[faoCountry == "United Kingdom of Great Britain and Northern Ireland",
              wbCtyIncClass := "High income"]

# NA values include TUN pork for protein (53 yr, 1961 to 2013) and 
# PAK pork for food (1 yr)
# these should be a zero

ldfCtyPpsHist_dt$food[is.na(ldfCtyPpsHist_dt$food)] <- 0
ldfCtyPpsHist_dt$protein[is.na(ldfCtyPpsHist_dt$protein)] <- 0

# add genus protein content of each ldf using cty
ldfCtyPpsHist_dt <- merge(ldfCtyPpsHist_dt,
                          genusCtyProt_dt,
                          by = c("ldf", "cty"),
                          all.x = TRUE)

# add genus protein content of each ldf using reg
ldfCtyPpsHist_dt <- merge(ldfCtyPpsHist_dt,
                          genusRegProt_dt,
                          by = c("ldf", "reg"),
                          all.x = TRUE)

ldfCtyPpsHist_dt[, percProtGenus := percProtCtyGenus]
ldfCtyPpsHist_dt[is.na(percProtGenus), percProtGenus := percProtRegGenus]

# per person demand for protein using GENUS data
ldfCtyPpsHist_dt[, proteinGenus := food * (percProtGenus / 100)]

# protein percent in food using FBS
ldfCtyPpsHist_dt[, percProt := 100 * protein / food]

# nan and infinite as NA so excluded from average protein]
ldfCtyPpsHist_dt$percProt[is.nan(ldfCtyPpsHist_dt$percProt)] = NA
ldfCtyPpsHist_dt$percProt[is.infinite(ldfCtyPpsHist_dt$percProt)] = NA

# * prot content ldf-cty====
# using FBS and GENuS
ldfProtPerc_dt <- ldfCtyPpsHist_dt[, c("year", "ldf", 
                                    "reg",
                                    "faoCountry", "isoCty",
                                    "cty",
                                    "percProt",
                                    "percProtGenus"
                                    )]

# average % protein in ldf-cty 
ldfProtPercCty_dt <- 
  ldfProtPerc_dt[, .(percProtCty =  mean(percProt),
                         percProtGenusCty =  mean(percProtGenus)),
                     by = .(ldf, cty, reg)]

saveRDS(ldfProtPercCty_dt, file = "procData_ldfProtPercCty_dt.Rda")

# * prot content ldf-reg====
# average % protein in ldf-reg using FBS and GENuS
ldfProtPercReg_dt <- 
  ldfProtPerc_dt[, .(percProtReg =  mean(percProt, na.rm = TRUE),
                     percProtGenusReg =  mean(percProtGenus)),
                 by = .(ldf, reg)]

saveRDS(ldfProtPercReg_dt, file = "procData_ldfProtPercReg_dt.Rda")


# wide to long
ldfCtyPpsHist_dt <- ldfCtyPpsHist_dt[, c("ldf",
                                         "faoCountry", "isoCty",
                                        "year", "food",
                                        "protein", "proteinGenus") ]

# wide to long so demand (food, protein, protein GENuS) in one column
ldfCtyPpsHist_dt <- melt(
  ldfCtyPpsHist_dt,
  id.vars = c("faoCountry",
              "isoCty",
              "year",
              "ldf"),
  variable.name = "ldfUnit",
  variable.factor = FALSE,
  value.name = "demaPerPersKg")

# POP CTY=======================================================================
# human population for each country

# keep population "item"
popCtyHist_dt <- ldfPopCtyHist_dt[item == "Population"]

# remove unneeded columns
popCtyHist_dt <- popCtyHist_dt[, c("item", "element") := NULL]

# rename  "value' as population
names(popCtyHist_dt)[names(popCtyHist_dt) == "value"] <- "pop000Peop"


# now map isoCty/faoCountry to regions & WB class

# load mapping data
faoCtyReg_dt <- readRDS(file = "procData_faoCtyReg_dt.Rda")

# merge data
popCtyHist_dt <- merge(popCtyHist_dt, faoCtyReg_dt,
                       by = c("isoCty", "faoCountry"),
                       all.x = TRUE)

# fill in missing data
# classify by hand the faoCountry that have missing region and WB class

popCtyHist_dt[faoCountry == "Belgium-Luxembourg", wbCtyIncClass := "High income"]
popCtyHist_dt[faoCountry == "Belgium-Luxembourg", reg := "EUR"]
popCtyHist_dt[faoCountry == "Belgium-Luxembourg", region := "Europe"]


popCtyHist_dt[faoCountry == "Czechoslovakia", wbCtyIncClass := "High income"]
popCtyHist_dt[faoCountry == "Czechoslovakia", reg := "EUR"]
popCtyHist_dt[faoCountry == "Czechoslovakia", region := "Europe"]

popCtyHist_dt[faoCountry == "USSR", wbCtyIncClass := "Upper middle income"]
popCtyHist_dt[faoCountry == "USSR", reg := "FSU"]
popCtyHist_dt[faoCountry == "USSR", region := "Former Soviet Union"]

popCtyHist_dt[faoCountry == "Yugoslav SFR" |
                faoCountry ==  "Serbia and Montenegro",
              wbCtyIncClass := "Upper middle income"]

popCtyHist_dt[faoCountry == "Yugoslav SFR" |
                faoCountry ==  "Serbia and Montenegro",
              reg := "EUR"]

popCtyHist_dt[faoCountry == "Yugoslav SFR" |
                faoCountry ==  "Serbia and Montenegro",
              region := "Europe"]

popCtyHist_dt[faoCountry == "Ethiopia PDR" |
                faoCountry == "Sudan (former)" |
                isoCty =="CIV",
              wbCtyIncClass := "Low income"]

popCtyHist_dt[faoCountry == "Ethiopia PDR" |
                faoCountry == "Sudan (former)" |
                isoCty =="CIV",
              reg := "SSA"]

popCtyHist_dt[faoCountry == "Ethiopia PDR" |
                faoCountry == "Sudan (former)" |
                isoCty =="CIV",
              region := "sub-Saharan Africa"]

popCtyHist_dt[faoCountry == "United Kingdom of Great Britain and Northern Ireland",
              wbCtyIncClass := "High income"]

popCtyHist_dt[faoCountry == "United Kingdom of Great Britain and Northern Ireland",
              reg := "EUR"]

popCtyHist_dt[faoCountry == "United Kingdom of Great Britain and Northern Ireland",
              region := "Europe"]


# remove columns
popCtyHist_dt <- popCtyHist_dt[, c("cty",
                                   "impCountry",
                                   "isoCountry") := NULL]

# save data
saveRDS(popCtyHist_dt, file = "procData_popCtyHist_dt.Rda")

# POP REG=======================================================================

# total population in each region
popRegHist_dt <- popCtyHist_dt[, .(
  pop000Peop =  sum(pop000Peop)),
  by = .(year, reg)]

saveRDS(popRegHist_dt, file = "procData_popRegHist_dt.Rda")

sum(popCtyHist_dt$pop000Peop)==sum(popRegHist_dt$pop000Peop)

# POP WLD=======================================================================
# total population of world
popWldHist_dt <- popCtyHist_dt[, .(
  pop000Peop =  sum(pop000Peop)),
  by = .(year)]

# check wld pop is the same as summing reg totals
popWldByRegHist_dt <- popRegHist_dt[, .(
  pop000Peop =  sum(pop000Peop)),
  by = .(year)]

sum(popWldByRegHist_dt$pop000Peop)==sum(popWldHist_dt$pop000Peop)
  
saveRDS(popWldHist_dt, file = "procData_popWldHist_dt.Rda")


# PER PERSON AND TOTAL LDF-CTY==================================================

# data for total demand by cty by
# merging population and per person demand
ldfCtyHist_dt <- merge(ldfCtyPpsHist_dt, popCtyHist_dt,
                          by = c("isoCty", "faoCountry", "year"),
                          all.x = TRUE)

# total demand is per person demand times number of people
ldfCtyHist_dt[, demaTot000Tons := (demaPerPersKg * (pop000Peop * 1000))/ 
                   (1000 * 1000)] # convert from kg to 000 tons

# remove pop column
ldfCtyHist_dt$pop000Peop <- NULL

# save cty demand
saveRDS(ldfCtyHist_dt, file = "procData_ldfCtyHist_dt.Rda")

# PER PERSON AND TOTAL LDF-REG==================================================

# remove per person cty demand 
ldfRegTotHist_dt <- ldfCtyHist_dt[, "demaPerPersKg" := NULL]

# * region total demand for each ldf====
ldfRegTotHist_dt <- ldfRegTotHist_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(year, ldfUnit, ldf, reg)]

# merge reg total for each ldf with population
ldfPopRegTotHist_dt <- merge(ldfRegTotHist_dt, popRegHist_dt,
                           by = c ("year", "reg"))

# * global total demand for each ldf====

# total world demand summed across all reg
ldfWldTotHist_dt <- ldfRegTotHist_dt[, .(
  reg = "WLD",
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(year, ldfUnit, ldf)]


# add world total to reg total
# first merge world total demand with world population
ldfPopWldTotHist_dt <- merge(ldfWldTotHist_dt, 
                          popWldHist_dt,
                          by = "year")

# second bind globe and reg total demand and population
ldfRegTotHist_dt <- rbind(ldfPopRegTotHist_dt,
                          ldfPopWldTotHist_dt)


# create a copy as now adding per person demand too
ldfRegHist_dt <- copy(ldfRegTotHist_dt)

# * per person demand====
ldfRegHist_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000)/ 
                (pop000Peop * 1000)] 

# remove pop column
ldfRegHist_dt$pop000Peop <- NULL

# label if reg or wld
ldfRegHist_dt[reg ==  "WLD", regType := "wld"]
ldfRegHist_dt[reg != "WLD", regType := "reg"]

saveRDS(ldfRegHist_dt, file = "procData_ldfRegHist_dt.Rda")