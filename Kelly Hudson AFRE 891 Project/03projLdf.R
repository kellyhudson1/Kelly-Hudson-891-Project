# HEADER========================================================================
# Title: projLdf
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: IMPACT simulations using excel interface for scenarios:
# 3 SSPs (SSP1, SSP2, SSP3) for income and population (not full SSP narrative)
# including per person ldf demand projected
# and aggregate cty to region for all data ldf total and population

# income elasticities: 1 default, 4 red meat lower
# 4 lower red meat income elasticities run with each SSP income & population
# 50% lower all cty
# 50% lower HIC (high-income cty)
# 100% lower all cty
# 100% lower HIC (high-income cty)

# unless otherwise stated,all scenarios run using HADGEM GCM 
# for RCP 6.0 and
# co2 fertilization of 379 ppm


# IMPACT v3 calibrated to SSP2 gdp and population
# no change in IPRs or elasticities between SSPs

# disable scientific notation
options(scipen = 999)

# LIBRARIES=====================================================================
library("data.table")

# ORGANIZE ALL LDF DEMAND RESULTS===============================================

# load data 
ldfCtyTotProj_dt <- fread("rawData_ldfCtyTotProj_dt.csv")


# add "scen" as scenario id
ldfCtyTotProj_dt$scen <- {stringr::str_sub(ldfCtyTotProj_dt$.id,
                                           start = 5, 
                                           end = -4)}
# drop .id
ldfCtyTotProj_dt$.id<- NULL


# year as character
ldfCtyTotProj_dt$YRS <- as.character(ldfCtyTotProj_dt$YRS)

# cbeef (in IMPACT) is Bovine Meat (in FAOSTAT FBS Item list)
# ceggs is Eggs
# clamb is Mutton & Goat Meat
# cmilk is Milk - Excluding Butter
# cpork is Pigmeat
# cpoult is Poultry Meat

# create ldf column
ldfCtyTotProj_dt[C == "cbeef", ldf := "Beef"]
ldfCtyTotProj_dt[C == "ceggs", ldf := "Eggs"]
ldfCtyTotProj_dt[C == "clamb", ldf := "Sheep"]
ldfCtyTotProj_dt[C == "cmilk", ldf := "Milk"]
ldfCtyTotProj_dt[C == "cpork", ldf := "Pork"]
ldfCtyTotProj_dt[C == "cpoul", ldf := "Poultry"]

# convert C into a character so can merge with protein
ldfCtyTotProj_dt$C <- as.character(ldfCtyTotProj_dt$C)

# column names
colnames(ldfCtyTotProj_dt) <- c("commodity",
                                "cty",
                                "year",
                                "foodDemaTot000Tons",
                                "scen",
                                "ldf")


# * assign region and country full name to cty====
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")


# harmonize cty name so can merge 
names(impCtyReg_dt)[names(impCtyReg_dt) == "impCty"] <- "cty"

# merge total cty demand for each ldf with reg/cty list
ldfCtyTotProj_dt <- merge(ldfCtyTotProj_dt, impCtyReg_dt, 
                     by = "cty", 
                     allow.cartesian = TRUE)

# * protein content====
# load ldf protein by cty/reg from FAO FBS + GENuS
ldfProtPercReg_dt <- readRDS(file = "procData_ldfProtPercReg_dt.Rda")
ldfProtPercCty_dt <- readRDS(file = "procData_ldfProtPercCty_dt.Rda")

# merge cty protein content of each ldf with cty total demand for each ldf
ldfCtyTotProj_dt <- merge(ldfCtyTotProj_dt, ldfProtPercCty_dt, 
                     by = c("ldf", "cty", "reg"), 
                     all.x = TRUE,
                     allow.cartesian = TRUE)

# merge reg protein content of each ldf with cty total demand for each ldf
ldfCtyTotProj_dt <- merge(ldfCtyTotProj_dt, ldfProtPercReg_dt, 
                       by = c("ldf", "reg"), 
                       all.x = TRUE,
                       allow.cartesian = TRUE)

# replace NA in "percProtCty" with "percProtReg"
# if cty level protein % missing give the region average,

# protein content of each LDF (g protein per 100 g food, so a %)

# using FBS
ldfCtyTotProj_dt[, percProt := percProtCty]
ldfCtyTotProj_dt[is.na(percProt), percProt := percProtReg]

# using GENuS
ldfCtyTotProj_dt[, percProtGenus := percProtGenusCty]
ldfCtyTotProj_dt[is.na(percProtGenus), percProtGenus := percProtGenusReg]

# calculate protein demand for each ldf in each cty

# using FBS ratio
ldfCtyTotProj_dt[, protDemaTot000Tons := foodDemaTot000Tons * percProt / 100]

# using GENuS
ldfCtyTotProj_dt[, protDemaGenusTot000Tons := foodDemaTot000Tons * percProtGenus / 100]

# remove unneeded columns
ldfCtyTotProj_dt[, c("percProt",
                     "percProtCty", 
                     "percProtReg",
                     "percProtGenus",
                     "percProtGenusCty", 
                     "percProtGenusReg",
                     "commodity") := NULL]

# wide to long so demand (food, protein, protein GENuS) in one column
ldfCtyTotProj_dt <- melt(
                      ldfCtyTotProj_dt, 
                      id.vars = c("reg",
                                  "region",
                                  "impCountry",
                                  "cty",
                                  "wbCtyIncClass",
                                  "year",
                                  "ldf",
                                  "scen"),
                      variable.name = "ldfUnit",
                      variable.factor = FALSE,
                      value.name = "demaTot000Tons")


# change ldfUnit names
ldfCtyTotProj_dt[ldfUnit == "foodDemaTot000Tons", ldfUnit := "food"]
ldfCtyTotProj_dt[ldfUnit == "protDemaTot000Tons", ldfUnit := "protein"]
ldfCtyTotProj_dt[ldfUnit == "protDemaGenusTot000Tons", ldfUnit := "proteinGenus"]

# save total demand in each cty for each ldf====
saveRDS(ldfCtyTotProj_dt, file = "procData_ldfCtyTotProj_dt.Rda")


# PER PERSON DEMAND=============================================================

# total cty demand for each ldf for all scenarios
if(!exists("ldfCtyTotProj_dt")) {
  ldfCtyTotProj_dt <-
    readRDS(file = "procData_ldfCtyTotProj_dt.Rda")
}
# country pop for all scenarios
popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")
popCtyProj_dt$year <- as.character(popCtyProj_dt$year)


# merge ldf and pop so can compute per person ldf demand
ldfCtyProj_dt <- merge(
  ldfCtyTotProj_dt,
  popCtyProj_dt,
  by = c("cty",
         "impCountry",
         "reg",
         "region",
         "wbCtyIncClass",
         "year",
         "scen")
)

# per person demand in kg
ldfCtyProj_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000) / (pop000Peop *
                                                                     1000)]

# remove population and drop pop and tot from data.table
ldfCtyProj_dt$pop000Peop <- NULL

# save demand for ldf at country scale, total and per person
saveRDS(ldfCtyProj_dt, 
        file = "procData_ldfCtyProj_dt.Rda")


# REGION DEMAND=================================================================
# total demand and per person per cty for each ldf

if(!exists("ldfCtyProj_dt")) {
  ldfCtyProj_dt <-
    readRDS(file = "procData_ldfCtyProj_dt.Rda")
}

# total population per cty
popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")
popCtyProj_dt$year <- as.character(popCtyProj_dt$year)

# REG TOTAL DEMAND FOR EACH LDF=================================================

# * reg total demand for each ldf====

# sum all cty demand by year, scen, ldf, and ldf unit
ldfRegTotProj_dt <- ldfCtyProj_dt[, .(demaTot000Tons = 
                                        sum(demaTot000Tons)),
                                  by = .(reg,
                                         region,
                                         year,
                                         scen,
                                         ldf, 
                                         ldfUnit)]

# not an even number because not all cty report demand for all ldf in
# every year

# count number obs per cty
ldfCtyProjNObs_dt <- ldfCtyProj_dt[, .N,
                                   by = .(cty,
                                          reg,
                                          region
                                   )]
# not all cty have each ldf in each year


# * world total demand for each ldf====
# sum cty demand for each ldf
ldfWldTotByCtyProj_dt <- ldfCtyProj_dt[, .(demaTot000Tons = 
                                             sum(demaTot000Tons),
                                           reg = "WLD",
                                           region = "World"),
                                       by = .(year,
                                              scen,
                                              ldf, 
                                              ldfUnit)]

# sum reg demand for each ldf
ldfWldTotByRegProj_dt <- ldfRegTotProj_dt[, .(demaTot000Tons = 
                                                sum(demaTot000Tons),
                                              reg = "WLD",
                                              region = "World"),
                                          by = .(year,
                                                 scen,
                                                 ldf, 
                                                 ldfUnit)]

# check sums
sum(ldfWldTotByRegProj_dt$demaTot000Tons) == sum(ldfWldTotByCtyProj_dt$demaTot000Tons)

# bind world total to region total
ldfRegTotProj_dt <- rbind(ldfRegTotProj_dt, 
                          ldfWldTotByCtyProj_dt)

# check row numbers
# for each region, ldf, ldfUnit, and scenario there should be 46 observations
# one for each year
nRow_ldfRegProj_dt <- ldfRegTotProj_dt[, .N,
                                       by = list(reg, ldf, ldfUnit, scen)]

min(nRow_ldfRegProj_dt$N) == max(nRow_ldfRegProj_dt$N)

# POPULATION====================================================================

# total population of each region
# sum across all cty in a reg
# * reg sum====
popRegProj_dt <- popCtyProj_dt[, .(pop000Peop =  sum(pop000Peop)),
                               by = .(reg, region, year, scen)]

# * pop world sum====
# sum population across all cty in world
popWldByCty_dt <- popCtyProj_dt[, .(pop000Peop =  sum(pop000Peop),
                                    reg = "WLD",
                                    region = "World"),
                                by = .(year, scen)]


# check if world pop is same if sum cty vs. sum reg
# take sum of pop across by reg
popWldByReg_dt <- popRegProj_dt[, .(pop000Peop =  sum(pop000Peop)),
                                by = .(year, scen)]

# check sums
sum(popWldByReg_dt$pop000Peop) == sum(popWldByCty_dt$pop000Peop)

# bind world total to region totals
popRegProj_dt <- rbind(popRegProj_dt, popWldByCty_dt)

# save projected pop for regions
saveRDS(popRegProj_dt, file = "procData_popRegProj_dt.Rda")


# MERGE DEMAND AND POP REG======================================================
# merge region total demand for each ldf with reg pop 
# so can compute per person demand for each ldf in each region
ldfRegProj_dt <- merge(ldfRegTotProj_dt,
                       popRegProj_dt,
                       by = c("reg",
                              "region",
                              "year",
                              "scen"))

# per person demand in kg
# change demand from thousands of tons to kg by multipling by one million
# change from thousand of people to people by multipling by 1000
ldfRegProj_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000) /
                (pop000Peop * 1000)]

# remove unneeded columns
ldfRegProj_dt$pop000Peop <- NULL

# label if reg or wld
ldfRegProj_dt[reg ==  "WLD", regType := "wld"]
ldfRegProj_dt[reg != "WLD", regType := "reg"]


# save demand for each ldf total and per person at region scale
saveRDS(ldfRegProj_dt, file = "procData_ldfRegProj_dt.Rda")