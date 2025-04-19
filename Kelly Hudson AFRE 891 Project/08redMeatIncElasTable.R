# HEADER========================================================================
# Title: redMeatElasTable
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: demand effect of changes in income elasticity for red meat 
# in (1) all world (2) high-income cty (HIC) 
# (3) low- and middle-income cty (LMIC)


# LIBRARIES=====================================================================
library("data.table")

# disable scientific notation
options(scipen = 999)

# LOAD DATA=====================================================================
if(!exists("ldfCtyProj_dt")) {
ldfCtyProj_dt <- readRDS(file = "procData_ldfCtyProj_dt.Rda")
}

# DEMAND BY INC GRP========================================================
# for each red meat ldf (beef, sheep, pork) and for aggregate red meat
# by year, scenario, income class, and protein-food
# calc as per person demand sum of demand in group / sum all people

ldfCtyProjIncE_dt <- ldfCtyProj_dt[
                                  (year == 2020 |
                                     year == 2050) 
                                 & (ldf == "Beef" |
                                      ldf == "Sheep" |
                                      ldf == "Pork"), ]

# keep needed columns for all dt
ldfCtyProjIncE_dt <-
  ldfCtyProjIncE_dt[, c("reg",
                     "impCountry",
                     "cty",
                     "wbCtyIncClass",
                     "year",
                     "ldf",
                     "ldfUnit",
                     "scen",
                     "demaTot000Tons",
                     "demaPerPersKg")]

# high income vs. not high income column
ldfCtyProjIncE_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "LMIC"]
ldfCtyProjIncE_dt[(wbCtyIncClass == "High income"), wbCtyIncClass := "HIC"]


# copy dt
ldfCtyProjIncECty_dt <- copy(ldfCtyProjIncE_dt)

# drop per person demand
# remove unneeded columns for 2020
ldfCtyProjIncE_dt <- ldfCtyProjIncE_dt[, demaPerPersKg := NULL]


# total demand for individual ldf by wb income class by year and scen
# sum ldf demand and population over WB classes
ldfIncEWbInc_dt <- ldfCtyProjIncE_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(wbCtyIncClass, year, ldf, ldfUnit, scen)]

# world total 
ldfIncEWld_dt <- ldfCtyProjIncE_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(year, ldf, ldfUnit, scen)]
ldfIncEWld_dt$wbCtyIncClass <- "ALL"

ldfIncEWbInc_dt <- rbind(ldfIncEWbInc_dt, ldfIncEWld_dt)

# add population of each cty
#popCtyProj_dt <- readRDS(file = "procData_popCtyProj_dt.Rda")
popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")
popCtyProj_dt$year <- as.character(popCtyProj_dt$year)


# keep relevant scenarios for all dt
popCtyProjIncE_dt <- popCtyProj_dt[
  (year == "2020" |
     year == "2050"), ]


# high income vs not high income column
popCtyProjIncE_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "LMIC"]
popCtyProjIncE_dt[wbCtyIncClass == "High income", wbCtyIncClass := "HIC"]

# total population in each WB income class by scenario and year
# sum ldf demand and population over WB classes
popWbIncClass_dt <- popCtyProjIncE_dt[, .(pop000Peop =  sum(pop000Peop)),
                                      by = .(wbCtyIncClass, year, scen)]
popWld_dt <- popCtyProjIncE_dt[, .(pop000Peop =  sum(pop000Peop)),
                                      by = .(year, scen)]
popWld_dt$wbCtyIncClass <- "ALL"

popWbIncClass_dt <- rbind(popWbIncClass_dt, popWld_dt)

# merge pop and demand 
ldfIncEWbInc_dt <- merge(popWbIncClass_dt, ldfIncEWbInc_dt,
                         by = c("year", "wbCtyIncClass", "scen"))


# per person demand
ldfIncEWbInc_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000) / (pop000Peop *
                                                                       1000)]
ldfIncEWbInc_dt$pop000Peop <- NULL


# demand in one column
# wide to long so demand in one column
ldfIncEWbInc_dt <-
  melt(
    ldfIncEWbInc_dt,
    id.vars = c("year",
                "wbCtyIncClass",
                "ldfUnit",
                "ldf",
                "scen"
    ),
    value.name = "dem",
    variable.name = "demUnit"
  )

# rename demUnit
ldfIncEWbInc_dt[demUnit == "demaPerPersKg", demUnit := "perPersKg"]
ldfIncEWbInc_dt[demUnit == "demaTot000Tons", demUnit := "Tot000Tons"]



# long to wide so years as column and can calc % change
ldfIncEWbIncWide_dt <- dcast(ldfIncEWbInc_dt,
                             wbCtyIncClass +
                             ldfUnit +
                             ldf + demUnit + scen ~ year, 
                           value.var = "dem")


# * food ====
# % change from in ldf food demand from 2020 to 2050 for each incE in a new dt
setkey(ldfIncEWbIncWide_dt, ldfUnit)
ldfIncEWbIncFood_dt <- ldfIncEWbIncWide_dt["food"]


ldfIncEWbIncFood_dt[, percChange := 100 * (`2050`  - `2020`)/ `2020`]


# SSP and scen col
ldfIncEWbIncFood_dt$ssp <- sub("_.*", "", ldfIncEWbIncFood_dt$scen)
ldfIncEWbIncFood_dt$incElas <- sub(".*_", "", ldfIncEWbIncFood_dt$scen)

ldfIncEWbIncFood_dt$scen <- NULL

# long to wide so all scen as a column
ldfIncEWbIncFood_dt <- dcast(ldfIncEWbIncFood_dt,
                             ssp + wbCtyIncClass +
                               ldfUnit +
                               ldf + demUnit ~ incElas, 
                             value.var = c("2020", "2050", "percChange"))



# keep needed cols for table
ldfIncEWbIncFood_dt <- ldfIncEWbIncFood_dt[, c("ssp",
                                               "wbCtyIncClass",
                                               "ldfUnit",
                                               "ldf",
                                               "demUnit",
                                               "2020_ref",
                                               "percChange_ref",
                                               "percChange_rm50LAllCty",
                                               "percChange_rm100LAllCty",
                                               "percChange_rm50LHiCty",
                                               "percChange_rm100LHiCty",
                                               "percChange_rmNegPt02HiCty"
                                               )]

# * protein ====
# aggregated over the 3 red meats

setkey(ldfIncEWbInc_dt, ldfUnit)
ldfIncEWbIncProt_dt <- ldfIncEWbInc_dt["proteinGenus"]

# sum over all ldf so have demand for red meat
rmIncEWbIncProt_dt <- ldfIncEWbIncProt_dt[, .(
  dem =  sum(dem)),
  by = .(wbCtyIncClass, year, demUnit, scen)]


# long to wide so year in col to calc % change
rmIncEWbIncProtWide_dt <- dcast(rmIncEWbIncProt_dt,
                             wbCtyIncClass +
                             demUnit + scen ~ year, 
                             value.var = "dem")


rmIncEWbIncProtWide_dt[, percChange := 100 * (`2050`  - `2020`)/ `2020`]

# SSP and scen col
rmIncEWbIncProtWide_dt$ssp <- sub("_.*", "", rmIncEWbIncProtWide_dt$scen)
rmIncEWbIncProtWide_dt$incElas <- sub(".*_", "", rmIncEWbIncProtWide_dt$scen)

rmIncEWbIncProtWide_dt$scen <- NULL

# long to wide so all incE as a column
rmIncEWbIncProtWide_dt <- dcast(rmIncEWbIncProtWide_dt,
                             ssp + wbCtyIncClass +
                             demUnit ~ incElas, 
                             value.var = c("2020", "2050", "percChange"))

# keep needed cols for table
rmIncEWbIncProtWide_dt <- rmIncEWbIncProtWide_dt[, c("ssp",
                                                     "wbCtyIncClass",
                                                     "demUnit",
                                                     "2020_ref",
                                                     "percChange_ref",
                                                     "percChange_rm50LAllCty",
                                                     "percChange_rm100LAllCty",
                                                     "percChange_rm50LHiCty",
                                                     "percChange_rm100LHiCty",
                                                     "percChange_rmNegPt02HiCty"
                                                     )]

rmIncEWbIncProtWide_dt$ldf <- "red meat"
rmIncEWbIncProtWide_dt$ldfUnit <- "proteinGenus"


# row bind data
redMeatAv_dt <- rbind(rmIncEWbIncProtWide_dt, ldfIncEWbIncFood_dt) 

# column order
redMeatAv_dt <- setcolorder(
  redMeatAv_dt,
  c("ssp",
    "wbCtyIncClass",
    "ldf",
    "ldfUnit",
    "demUnit",
    "2020_ref",
    "percChange_ref",
    "percChange_rm50LAllCty",
    "percChange_rm100LAllCty",
    "percChange_rm50LHiCty",
    "percChange_rm100LHiCty",
    "percChange_rmNegPt02HiCty"
  )
)

# round all numeric columns to 1 dp
redMeatAv_dt <- data.frame(lapply(redMeatAv_dt, function(y) if(is.numeric(y)) round(y, 1) else y))
redMeatAv_dt <- as.data.table(redMeatAv_dt)

# reorder red meat ldf as ordered factor
redMeatAv_dt$ldf <-
  factor(
    redMeatAv_dt$ldf,
    ordered = TRUE,
    levels = c(
      "Beef",
      "Sheep",
      "Pork",
      "red meat"
    )
  )

# order data
redMeatAv_dt <- setorder(redMeatAv_dt, ssp, demUnit, ldf, wbCtyIncClass)


# Table for per person demand 
# for red meat in 3 groups
setkey(redMeatAv_dt, demUnit, ldf)
redMeatSSPElast_dt <- redMeatAv_dt[.("perPersKg", "red meat")]
redMeatSSPElast_dt <- setorder(redMeatSSPElast_dt, wbCtyIncClass)
fwrite(redMeatSSPElast_dt, 
       "output_table3_redMeatScenDemaSSP.csv",
       row.names = FALSE)


# Table for demand for SSP into SI by all individual red meat LDF
setkey(redMeatAv_dt, ssp)
fwrite(redMeatAv_dt["SSP2"],
       "output_tableSI4_redMeatScen.csv",
       row.names = FALSE)


# MIN AND MAX BY CTY============================================================
# within a WB income group

# SSP and scen col
ldfCtyProjIncECty_dt$ssp <- sub("_.*", "", ldfCtyProjIncECty_dt$scen)
ldfCtyProjIncECty_dt$incElas <- sub(".*_", "", ldfCtyProjIncECty_dt$scen)

ldfCtyProjIncECty_dt$scen <- NULL

# wide to long so demand in one column
ldfCtyProjIncECty_dt <-
  melt(
    ldfCtyProjIncECty_dt,
    id.vars = c(
      "cty",
      "impCountry",
      "reg",
      "wbCtyIncClass",
      "year",
      "ldfUnit",
      "ldf",
      "ssp",
      "incElas"
    ),
    value.name = "dem",
    variable.name = "demUnit"
  )

# rename demUnit
ldfCtyProjIncECty_dt[demUnit == "demaPerPersKg", demUnit := "perPersKg"]
ldfCtyProjIncECty_dt[demUnit == "demaTot000Tons", demUnit := "Tot000Tons"]


# long to wide so year as a separate columns
ldfCtyProjIncECty_dt <- dcast(
  ldfCtyProjIncECty_dt,
  cty +
    impCountry +
    reg +
    wbCtyIncClass +
    ldfUnit +
    ldf + demUnit + ssp + incElas ~  year,
  value.var = "dem"
)


# % changes
ldfCtyProjIncECty_dt[, percChange := 100 * (`2050`  - `2020`) / `2020`]

# long to wide so all incE as a column
ldfCtyProjIncECty_dt <- dcast(
  ldfCtyProjIncECty_dt,
  cty +
    impCountry +
    reg +
    wbCtyIncClass +
    ldfUnit +
    ldf + demUnit + ssp ~ incElas,
  value.var = c("2020", "2050", "percChange")
)

# keep needed columns
ldfCtyProjIncECty_dt <- ldfCtyProjIncECty_dt[, c(
  "cty",
  "impCountry",
  "reg",
  "wbCtyIncClass",
  "ldfUnit",
  "ldf",
  "demUnit",
  "ssp",
  "2020_ref",
  "percChange_ref",
  "percChange_rm50LAllCty",
  "percChange_rm100LAllCty",
  "percChange_rm50LHiCty",
  "percChange_rm100LHiCty"
)]

# wide to long, so all scenarios in one column
ldfCtyProjIncECty_dt <-
  melt(
    ldfCtyProjIncECty_dt,
    id.vars = c("wbCtyIncClass",
                "cty",
                "impCountry",
                "reg",
                "ldfUnit",
                "ldf",
                "demUnit",
                "ssp"
    ),
    value.name = "val",
    variable.name = "scenVar"
  )

# min and max by wb group for each ldf in food
setkey(ldfCtyProjIncECty_dt, ldfUnit)

# data for cty ldf in separate table for food, absolute value in 2020
ldfCtyProjIncECty_dt <- ldfCtyProjIncECty_dt["food"][, .(
  min =  round(min(val), 0),
  max =  round(max(val), 0)),
  by = .(wbCtyIncClass, ldf, demUnit, scenVar, ssp)]


ldfCtyProjIncECty_dt$range <- paste(ldfCtyProjIncECty_dt$min, 
                                    ldfCtyProjIncECty_dt$max, sep ="\226")
ldfCtyProjIncECty_dt[ , c("min", "max") := NULL]

# long to wide, so scen as colums
ldfCtyProjIncECty_dt <- dcast(ldfCtyProjIncECty_dt,
                               wbCtyIncClass +
                               ldf + 
                               demUnit + ssp ~ scenVar, 
                             value.var = "range")

# red meat ldf as ordered factor
ldfCtyProjIncECty_dt$ldf <-
  factor(
    ldfCtyProjIncECty_dt$ldf,
    ordered = TRUE,
    levels = c(
      "Beef",
      "Sheep",
      "Pork"
    )
  )

ldfCtyProjIncECty_dt <- setorder(ldfCtyProjIncECty_dt, demUnit, wbCtyIncClass, ldf, ssp)
ldfCtyProjIncECty_dt

# save
setkey(ldfCtyProjIncECty_dt, ssp)
fwrite(ldfCtyProjIncECty_dt["SSP2"],
       "output_tableSI5_elastScenMinMax.csv",
       row.names = FALSE)


# HIC vs. LMIC contribution===

incGrpCont_dt <- ldfCtyProj_dt[(scen == "SSP1_ref" |
                                  scen == "SSP2_ref" |
                                  scen == "SSP3_ref"
) &
  (year == 2020 |
     year == 2050) 
& (ldf == "Beef" |
     ldf == "Sheep" |
     ldf == "Pork") &  
  ldfUnit == "proteinGenus", ]


# high income vs. not high income column
incGrpCont_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "LMIC"]
incGrpCont_dt[wbCtyIncClass == "High income", wbCtyIncClass := "HIC"]

# total demand for individual ldf by wb income class by year and scen
incGrpCont_dt <- incGrpCont_dt[, .(
  demaTot000Tons =  sum(demaTot000Tons)),
  by = .(wbCtyIncClass, year, scen)]

incGrpCont_dt <- dcast(incGrpCont_dt,
                       scen + year ~ wbCtyIncClass)


incGrpCont_dt[, percLMIC := round(100 * LMIC / (HIC + LMIC), 0)]
incGrpCont_dt[, percHICPerc := round(100 * HIC / (HIC + LMIC), 0)]