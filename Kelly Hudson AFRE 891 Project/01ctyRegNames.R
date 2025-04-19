# HEADER========================================================================
# Title: ctyRegNames
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"


# 1) assign all impact cty (3 letter cty code) to a Standard-IMPACT region
# and a WB cty class

# 2) assign all fao cty (3 letter cty code) to a Standard-IMPACT region,
# impact cty, and WB cty class

# use the 8 region aggregation in IMPACT (Standard-IMPACT):
# SAS	South Asia	
# SSA	sub-Saharan Africa	
# EUR	Europe	
# LAC	Latin America and Caribbean	
# FSU	Former Soviet Union	
# EAP	East Asia and Pacific	
# NAM	North America	
# MEN	Middle East and North Africa	

# LIBRARIES=====================================================================
library("readxl")
library("data.table")
library("zoo")


# IMPACT MAPPING================================================================

# assign FAO region and IMPACT region to IMPACT cty code
impCtyReg_dt <- data.table(read_xlsx(path = "rawData_ctyRegMapping.xlsx",
                                  sheet = "impactCtyReg"))

# keep IMPACT cty, IMPACT 8 regions
colnames(impCtyReg_dt)

# select col to keep
impCtyReg_dt <- impCtyReg_dt[, c("Cty",       # impact country 3 letter code 
                           "Standard-IMPACT...15", # impact 8 regions
                           "LongName")]
# rename cols
colnames(impCtyReg_dt) <- c("cty",
                         "reg",
                         "impCountryAbb")

# WB classification of impact cty (cty)
# load world bank country classification
wbCtyClassRaw_dt <- data.table(read_xlsx(path = "rawData_ctyRegMapping.xlsx",
                                     sheet = "WB",
                                     skip = 5))

# keep columns and complete cases
wbCtyClassImp_dt <- wbCtyClassRaw_dt[, c("x...3",
                                         "x...4",
                                   "x...7")]
wbCtyClassImp_dt <- wbCtyClassImp_dt[complete.cases(wbCtyClassImp_dt), ]

# rename so can merge with impact cty
colnames(wbCtyClassImp_dt) <- c("wbCountry",
                                "cty",
                                "wbCtyIncClass")


# merge impact and WB dcty lists
impCtyReg_dt <- merge(wbCtyClassImp_dt, impCtyReg_dt, 
                        by = "cty",
                        all.y = TRUE)

# some impact cty has different names to WB cty names, 
# because of different naming conventions
# and aggregations of cty in IMPACT

# so manually add cty income class where is NA
impCtyReg_dt[(impCountryAbb == "Morocco"), 
             wbCtyIncClass := "Lower middle income"] 

impCtyReg_dt[(impCountryAbb == "China" |
                impCountryAbb == "Guyanas" |
                impCountryAbb == "Other Balkans" |
                impCountryAbb == "Other Indian Ocean" |
                impCountryAbb == "Other Pacific Ocean" |
                impCountryAbb == "Other Caribbean"), 
             wbCtyIncClass := "Upper middle income"] 

impCtyReg_dt[(impCountryAbb == "Baltic States" |
                impCountryAbb == "Belgium-Luxembourg" |
                impCountryAbb == "Switzerland" |
                impCountryAbb == "Finland" |
                impCountryAbb == "France" |
                impCountryAbb == "Italy" |
                impCountryAbb == "UK" |
                impCountryAbb == "Spain" | 
                impCountryAbb == "Rest of Arabia" |
                impCountryAbb == "Other Southeast Asia" |
                impCountryAbb == "Other Atlantic"),
             wbCtyIncClass := "High income"] 


# add impact full cty name
impactISO_dt <- data.table(read_xlsx(path = "rawData_ctyRegMapping.xlsx",
                                         sheet = "impactISO"))

# keep columns and complete cases
colnames(impactISO_dt) <- c("cty",
                                "impCountry",
                                "isoCountry",
                                "isoCty")
impactISOImp_dt <- impactISO_dt[, c("cty", "impCountry")]


# merge impact and WB cty lists
impCtyReg_dt <- merge(impactISOImp_dt,
                          impCtyReg_dt, 
                      by = "cty",
                      all.x = FALSE)


# remove Jersey isoCTy as it is part of UKP/GBR
# and is not sperate in FAOSTAT or IMPACT results
impCtyReg_dt$dupImpCty <- duplicated(impCtyReg_dt$cty)
impCtyReg_dt <- impCtyReg_dt[!duplicated(impCtyReg_dt$cty), ] 

# remove unneeded columns
impCtyReg_dt <- impCtyReg_dt[, c("wbCountry", 
                                 "impCountryAbb",
                                 "dupImpCty") := NULL]


# region long names
impCtyReg_dt[reg == "EAP", region := "East Asia & Pacific"]
impCtyReg_dt[reg == "EUR", region := "Europe"]
impCtyReg_dt[reg == "FSU", region := "Former Soviet Union"]
impCtyReg_dt[reg == "LAC", region := "Latin America & Caribbean"]
impCtyReg_dt[reg == "MEN", region := "Middle East & North Africa"]
impCtyReg_dt[reg == "NAM", region := "North America"]
impCtyReg_dt[reg == "SAS", region := "South Asia"]
impCtyReg_dt[reg == "SSA", region := "sub-Saharan Africa"]

# save list of IMPACT cty with their 
# fao and impact regions associations and WB income class
saveRDS(impCtyReg_dt, file = "procData_impCtyReg_dt.Rda")

# FAO MAPPING====

# For each cty in FAOSTAT build a data.table that contains 
# 1) fao cty (3 letter cty code) 2) Standard-IMPACT regions, 3) a fao region,
# 4) full cty name, 5) impact cty, 6) WB cty class


# start with impactISO
faoCtyReg_dt <- copy(impactISO_dt)


# fill the NA with previous rows value, so each isoCty 
# has the associated impact code
# use "zoo" package
faoCtyReg_dt <- na.locf(impactISO_dt)

# add FAO region, IMPACT region, wb income class, to isocty
faoCtyReg_dt <- merge(faoCtyReg_dt, impCtyReg_dt,
                      by = c("cty", "impCountry"))



# add fao country name so can later merge with fao fbs data
faoCtyRegFBS_dt <- data.table(read_xlsx(path = "rawData_ctyRegMapping.xlsx",
                                     sheet = "faoCtyReg"))

# keep columns
faoCtyRegFBS_dt <- faoCtyRegFBS_dt[, c("Country", "ISO3 Code")]

# rename columns
colnames(faoCtyRegFBS_dt) <- c("faoCountry", "isoCty")

# remove duplicates
faoCtyRegFBS_dt <- unique(faoCtyRegFBS_dt[, c("faoCountry", "isoCty")])


# merge 
faoCtyReg_dt <- merge(faoCtyReg_dt, faoCtyRegFBS_dt,
                      by = "isoCty")

# save cty from faostat with their 
# impact regions, impact cty, associations and WB income class
saveRDS(faoCtyReg_dt, file = "procData_faoCtyReg_dt.Rda")