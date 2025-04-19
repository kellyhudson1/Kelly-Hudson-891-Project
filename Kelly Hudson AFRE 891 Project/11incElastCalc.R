# HEADER========================================================================
# Title: incElastCalc
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: compute income elasticities of demand using hist and proj data
# compare to exogenous trend, plot all

# LIBRARIES=====================================================================
library("data.table")
library("dplyr")
library("ggplot2")
library("RColorBrewer")

# disable scientific notation
options(scipen = 999)


# LOAD DATA=====================================================================

# one cty per region based on largest total demand for red meat (in protein)
ctyListRedMeat2050_dt <- readRDS(file = "procData_ctyListRedMeat2050_dt.Rda")

# load historical ldf demand
if(!exists("ldfCtyHist_dt")) {
ldfCtyHist_dt <- readRDS(file = "procData_ldfCtyHist_dt.Rda")
}


# keep food demand
setkey(ldfCtyHist_dt, ldfUnit)
ldfFoodCtyHist_dt <- ldfCtyHist_dt["food"]

names(ldfFoodCtyHist_dt)[names(ldfFoodCtyHist_dt) == "cty"] <- "isoCty"
names(ldfFoodCtyHist_dt)[names(ldfFoodCtyHist_dt) == "country"] <- "faoCountry"


# keep columns needed to calculate elasticity
ldfFoodCtyHist_dt <- ldfFoodCtyHist_dt[, c("isoCty", "faoCountry", "year", "ldf", "demaPerPersKg")]


# historical GDP================================================================
gdpCtyHist_dt <- fread("rawData_FAOSTAT_data_2-12-2020.csv")


# data is total GDP in million US $ 2005 prices
# keep relevant columns
gdpCtyHist_dt <- gdpCtyHist_dt[, c("Area Code",
                                         "Area",
                                         "Year",
                                         "Value")]

# rename columns
colnames(gdpCtyHist_dt) <- c("isoCty",
                                "faoCountry",
                                "year",
                                "value")

# year as numeric 
gdpCtyHist_dt$year <- as.character(gdpCtyHist_dt$year)


# rename as GDP
names(gdpCtyHist_dt)[names(gdpCtyHist_dt) == "value"] <- "gdpTotMill"

# merge with population
popCtyHist_dt <- readRDS(file = "procData_popCtyHist_dt.Rda")

gdpCtyHist_dt <- merge(gdpCtyHist_dt, popCtyHist_dt,
      by = c("year", "isoCty", "faoCountry"))


# gdp per person
gdpCtyHist_dt[, gdpPerPers := (gdpTotMill * 1000000)/
                (pop000Peop * 1000)]


# keep columns needed to calculate elasticity
gdpCtyHist_dt <- gdpCtyHist_dt[, c("isoCty", "faoCountry", "year", "gdpPerPers")]


# MERGE GDP WITH LDF DEMAND=====================================================
gdpAsfCty_dt <- merge(gdpCtyHist_dt, ldfFoodCtyHist_dt,
      by = c("isoCty", "faoCountry", "year"),
      all = FALSE)

gdpAsfCty_dt$dataType <- "Historical"

# CALC ELASTICITY===============================================================
# compute the point elasticity
# so know the elasticity each year
# elasticity = (delta Q / delta income) * (income / Q )

gdpAsfCty_dt <- data.table(
  gdpAsfCty_dt %>%
  group_by(isoCty, faoCountry, ldf, dataType) %>%
  mutate(
    incElas = ((demaPerPersKg - lag(demaPerPersKg, 1)) /
               (gdpPerPers - lag(gdpPerPers, 1))) * 
      (gdpPerPers / demaPerPersKg)
  )
)

# remove missing data
gdpAsfCty_dt <- gdpAsfCty_dt[complete.cases(gdpAsfCty_dt), ]

# keep needed columns
gdpAsfCty_dt <- gdpAsfCty_dt[, c("isoCty", "year", "ldf", "dataType", "incElas")]

names(gdpAsfCty_dt)[names(gdpAsfCty_dt) == "isoCty"] <- "cty"


# order data by cty, ldf, year
gdpAsfCty_dt <- setorder(gdpAsfCty_dt, cty, ldf)

# IMPACT MODEL ELASTICITY=======================================================
# load exogenous trend used in model in reference case

#incElas_dt <- readRDS(file = "procData_incElas_dt.Rda")
incElas_dt <-fread("rawData_incEScen_dt.csv")

incElas_dt$year <- as.numeric(as.character(incElas_dt$year))

# keep ldf
incElas_dt <- incElas_dt[c == "cbeef" |
                                  c == "ceggs" |
                                  c == "clamb"  |
                                  c == "cmilk"  |
                                  c == "cpork"  |
                                  c == "cpoul", ]

# harmonize names
incElas_dt[c == "cbeef", c := 'Beef']
incElas_dt[c == "ceggs", c := 'Eggs']
incElas_dt[c == "clamb", c := 'Sheep']
incElas_dt[c == "cmilk", c := 'Milk']
incElas_dt[c == "cpork", c := 'Pork']
incElas_dt[c == "cpoul", c := 'Poultry']

names(incElas_dt)[names(incElas_dt) == "c"] <- "ldf"
incElas_dt$dataType <- "Exogenous trend"

# bind hist and proj
incElas_dt <- rbind(incElas_dt, gdpAsfCty_dt)

# keep for 8 cty

# check names of cty are the same for proj and hist
incElas_dt[cty == "CHN", cty := 'CHM']
incElas_dt[cty == "FRA", cty := 'FRP']



# merge
incElas8Cty_dt <- merge(ctyListRedMeat2050_dt, incElas_dt,
                        by = "cty")

# keep needed cols
incElas8Cty_dt <- incElas8Cty_dt[, c("cty", "ldf", 
                                     "year", "dataType", 
                                     "incElas")]

# rename cty so use full country name
incElas8Cty_dt[cty == "BRA", country := "Brazil"]
incElas8Cty_dt[cty == "CHM", country := "China"]
incElas8Cty_dt[cty == "EGY", country := "Egypt"]
incElas8Cty_dt[cty == "DEU", country := "Germany"]
incElas8Cty_dt[cty == "IND", country := "India"]
incElas8Cty_dt[cty == "NGA", country := "Nigeria"]
incElas8Cty_dt[cty == "RUS", country := "Russia"]
incElas8Cty_dt[cty == "USA", country := "USA"]

# cty as ordered  factor to match region order
incElas8Cty_dt$country <-
  factor(
    incElas8Cty_dt$country,
    ordered = TRUE,
    levels = c("China",
               "Germany",
               "Russia",
               "Brazil",
               "Egypt",
               "USA",
               "India",
               "Nigeria"
    )
  )

# data type as ordered factor
incElas8Cty_dt$dataType <-
  factor(
    incElas8Cty_dt$dataType,
    ordered = TRUE,
    levels = c(
      "Historical",
      "Exogenous trend"                   
    )
  )

# * ldf as ordered factor====
incElas8Cty_dt$ldf <-
  factor(
    incElas8Cty_dt$ldf,
    ordered = TRUE,
    levels = c(
      "Beef",
      "Sheep",
      "Pork",
      "Poultry",
      "Eggs",
      "Milk"
    )
  )

incElas8Cty_dt <- setorder(incElas8Cty_dt, cty, ldf, dataType, year)


# PLOT DATA=====================================================================

# * plot options====
# shape list
# common region marker shape
regShape <- c(15, 16, 17,
              18, 6, 4,
              5, 0)

# common region color
# http://colorbrewer2.org/#type=sequential&scheme=BuGn&n=3
brewColor <- brewer.pal(8, name = 'Set1')
regColor <- c(brewColor[[1]], 
              brewColor[[2]],
              brewColor[[3]],
              brewColor[[4]], 
              brewColor[[5]],
              "#000000",
              brewColor[[7]], 
              brewColor[[8]])

# plot options
theme_opts <- list(
  theme(
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "white"),
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 16),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 16,
                               angle = 90),
    axis.text.y = element_text(size = 16),
    axis.title = element_text(size = 16),
    aspect.ratio = 1,
    panel.spacing = unit(0, "lines")
    
  )
)

# list of years to report, take every 5th one
fifthYr <- c(
  "2020", "2025",
  "2030", "2035",
  "2040", "2045",
  "2050")


# * plot trend in future=========================================================
# * income elasticity over time for each cty and each ldf using impact ref values

# set key to filter item so only plot food demand for every 5th year
setkey(incElas8Cty_dt, year)

# generate plot
incEProjTrendPlot <-
  ggplot(incElas8Cty_dt[.(fifthYr)], 
         aes(y = incElas,
             x = as.numeric(year),
             shape = country, 
             color = country)) +
  geom_point(size = 2, stroke = 1.5) +
  geom_line() + 
  facet_wrap(~ ldf, 
             nrow = 2) +
  theme_bw() + theme_opts +
  labs(shape = "Country",
       color = "Country",
       x = "Year",
       y = "Income elasticity of demand") + 
  scale_shape_manual(values = regShape) +
  scale_color_manual(values = regColor) +
  guides(shape = guide_legend(nrow = 1,
                              override.aes = list(size = 6)),
         color = guide_legend(override.aes = list(linetype = 0)))


# # save plot
# ggsave(
#   file = "unrepFig_incElas8Cty.tiff",
#   width = 12,
#   height = 9,
#   incEProjTrendPlot,
#   compression = "lzw"
# )

# * plot historical and future elasticities====
# only plot very fifth year for trend
incElas8Cty_dt <-
  incElas8Cty_dt[(incElas8Cty_dt$dataType == "Historical") | 
                   (incElas8Cty_dt$dataType == "Exogenous trend" &
                      incElas8Cty_dt$year ==  "2020" | 
                      incElas8Cty_dt$year ==  "2025" |
                      incElas8Cty_dt$year ==  "2030" |
                      incElas8Cty_dt$year ==  "2035" |
                      incElas8Cty_dt$year ==  "2040" |
                      incElas8Cty_dt$year ==  "2045" |
                      incElas8Cty_dt$year ==  "2050" 
                      ), ]

incElas8Cty_dt[dataType =="Exogenous trend", dataTypeL := "Reference case elasticities"]
incElas8Cty_dt[dataType =="Historical", dataTypeL := "Estimated"]


# plot limits for y axis
incElastYMin <- -10
incElastYMax <-  10


{
# plot
incEProjHistBW_plot <-
  ggplot(incElas8Cty_dt,
         aes(
           y = incElas,
           x = as.numeric(year),
           shape = dataTypeL)) +
  geom_point(size = 0.3) +
  geom_smooth(data =  subset(incElas8Cty_dt, 
                             dataTypeL == "Estimated"),
               method = 'lm',
               formula = y ~ x,
               aes(color = " "),
               lwd = 0.5) +  
  facet_wrap(country ~  ldf,
             nrow = 4) +
  theme_bw() +   theme(
    strip.text.x = element_text(size = 6,
                                margin = margin(0.05, 0, 0.05, 0, "cm")),
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 5, angle = 90, vjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 12),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm")) +
  ylim(incElastYMin, incElastYMax) +
  scale_shape_manual(values = c(16, 2)) +
  scale_color_manual(name = " Linear regression",
                     values = c(
                       " " = "black")) +
  labs(shape = "Data type",
       color = "Data type",
       x = "Year",
       y = "Income elasticity of demand"
  ) +
  guides(shape = guide_legend(nrow = 1,
                              override.aes = list(size = 4)),
         color = guide_legend(override.aes = list(linetype = 1, 
                                                  fill = NA))) +
  theme(legend.margin = margin(t = -0.2, 
                               r = 0, 
                               b = -0.1, 
                               l = 0, unit = "cm"),
        legend.key.height = unit(0, "cm"))
  
}


{ 
  incEProjHistCo_plot <-
  ggplot(incElas8Cty_dt,
         aes(
           y = incElas,
           x = as.numeric(year),
           shape = dataTypeL,
           color = dataTypeL)) +
  geom_point(size = 0.3) +
    geom_smooth(data =  subset(incElas8Cty_dt,
                            dataTypeL == "Estimated"),
                fill = "#abd9e9",
               method = 'lm',
              formula = y ~ x,
               lwd = 0.5) +
  facet_wrap(country ~  ldf,
             nrow = 4) +
  theme_bw() +
    ylim(incElastYMin, incElastYMax) +
    labs(shape = "Data type",
         color = "Data type",
         x = "Year",
         y = "Income elasticity of demand"
    ) +
    theme(
    strip.text.x = element_text(size = 6,
                                margin = margin(0.05, 0, 0.05, 0, "cm")),
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 5, angle = 90, vjust = 0),
    axis.text.y = element_text(size = 5),
    axis.title = element_text(size = 12),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm")) +
    scale_shape_manual(values = c(16, 2)) +
    scale_color_manual(values = c("#2c7bb6", "#fdae61")) +
    guides(shape = guide_legend(nrow = 1,
                                override.aes = list(size = 4)),
           color = guide_legend(override.aes = list(linetype = 0,
                                                    fill = NA))) +
    theme(legend.margin = margin(t = -0.2,
                                 r = 0,
                                 b = -0.1,
                                 l = 0, unit = "cm"),
          legend.key.height = unit(0, "cm"))
}

# save plot
ggsave(
  file = "output_figSI1_incElas8Cty.tiff",
  width = 10,
  height = 5,
  incEProjHistCo_plot,
  compression = "lzw"
)


# * income elasticity table=====================================================
{
# data
impCtyReg_dt <- readRDS(file = "procData_impCtyReg_dt.Rda")

incElasCtyReg_dt <- merge(incElas_dt, impCtyReg_dt,
      by = "cty")


# ldf as ordered factor
incElasCtyReg_dt$ldf <-
  factor(
    incElasCtyReg_dt$ldf,
    ordered = TRUE,
    levels = c(
      "Beef",
      "Sheep",
      "Pork",
      "Poultry",
      "Eggs",
      "Milk"
    )
  )


# average elasticity by LDF for world
tapply(incElasCtyReg_dt$incElas, incElasCtyReg_dt$ldf, mean)

# * plot elasticity by reg/ldf/time====

setkey(incElasCtyReg_dt, dataType , year)
incElasCtyReg3Yr_dt <- incElasCtyReg_dt[CJ("Exogenous trend",
                                        c("2020", "2035", "2050"))]

# income elasticity by reg-ldf-year plot
incElasRegAsfYr_plot <- ggplot(data = incElasCtyReg3Yr_dt,
                      aes(x = ldf,
                          y = incElas,
                          fill = year)) + 
  geom_boxplot(outlier.shape = NA, 
               position = position_dodge(width = 0.7),
               color = "black",
               size = 0.3) + 
  facet_wrap(~ region, nrow = 2) +
  labs(fill = "Year",
       x     = "Livestock-derived food",
       y     = "Income elasticity of demand") +
  theme_bw() +
  theme_opts + 
  theme(strip.text = element_text(size = 10),
        axis.text  = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
  scale_fill_brewer(palette = "Greys")


# save plot
ggsave(
  file = "output_fig1_incElasReg.tiff",
  incElasRegAsfYr_plot,
  width  = 9,
  height = 6,
  compression = "lzw"
)

# income elasticity by WB-ldf-year plot
# high income vs not high income column
incElasCtyReg3Yr_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "Low- and middle-income countries"]
incElasCtyReg3Yr_dt[(wbCtyIncClass == "High income"), wbCtyIncClass := "High-income countries"]

incElasWBAsfYr_plot <- ggplot(data = incElasCtyReg3Yr_dt,
                               aes(x = ldf,
                                   y = incElas,
                                   fill = year)) + 
  geom_boxplot(outlier.shape = NA) + 
  facet_wrap(~ wbCtyIncClass) +
  labs(fill = "Year",
       x     = "Livestock-derived food",
       y     = "Income elasticity of demand") +
  theme_bw() +
  theme_opts + 
  theme(strip.text = element_text(size = 10),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 12)) + 
scale_fill_brewer(palette = "Greys")


# save plot
ggsave(
  file = "output_figSI2_incElastIncGrp.tiff",
  incElasWBAsfYr_plot,
  width  = 9,
  height = 6,
  compression = "lzw"
)

# table by reg
incElastReg_Tab <- incElasCtyReg3Yr_dt[,
                                  .(N = .N,
                                    mean = round(mean(incElas), 2),
                                    min = round(min(incElas), 2),
                                    max = round(max(incElas), 2)),
                                  by = .(ldf, year, reg)]

names(incElastReg_Tab)[names(incElastReg_Tab) == "reg"] <- "location"
incElastReg_Tab$dataType <- "reg"

# table by World Bank income group
incElastIncGroup_Tab <- incElasCtyReg3Yr_dt[,
                                 .(N = .N,
                                   mean = round(mean(incElas), 2),
                                   min = round(min(incElas), 2),
                                   max = round(max(incElas), 2)),
                                 by = .(ldf, year, wbCtyIncClass)]

names(incElastIncGroup_Tab)[names(incElastIncGroup_Tab) == "wbCtyIncClass"] <- "location"
incElastIncGroup_Tab$dataType <- "incGroup"

# bind income and reg groups
incElast_Tab <- rbind(incElastIncGroup_Tab, incElastReg_Tab)


# range
incElast_Tab$range <- paste(incElast_Tab$min, incElast_Tab$max, sep ="\226")

incElast_Tab$range <- paste0("(", incElast_Tab$range,")")

incElast_Tab$value <- paste(incElast_Tab$mean, incElast_Tab$range, sep = " ")

incElast_Tab <- incElast_Tab[, c("ldf", "year", "location", "dataType", "value")]

# long to wide

incElast_Tab <- dcast(incElast_Tab,
                      location + dataType + year ~ ldf,
                      value.var = "value"
                      )


# save income elasticity range
setkey(incElast_Tab, dataType)

# # by region
# fwrite(incElast_Tab["reg"],
#        "unrepTable_incElastReg.csv",
#        row.names = FALSE)


# by WB income group
incElastIncGroupTable_Tab <- incElast_Tab["incGroup"]

unique(incElastIncGroupTable_Tab$location)

# income group as ordered factor
incElastIncGroupTable_Tab$location <-
  factor(
    incElastIncGroupTable_Tab$location,
    ordered = TRUE,
    levels = c(
      "Low- and middle-income countries",
      "High-income countries"
    )
  )

incElastIncGroupTable_Tab <- setorder(incElastIncGroupTable_Tab, location)
incElastIncGroupTable_Tab$dataType <- NULL

# fwrite(incElastIncGroupTable_Tab,
#        "unrepTable_incElastIncGrp.csv",
#        row.names = FALSE)

# countries per grouping
impCtyReg_dt[,.N,by = wbCtyIncClass]

}

# * plot all cty elasticity by ldf-reg====
# plot all countries by reg-ldf

{
setkey(incElasCtyReg_dt, year, dataType)

  
incElasCtyReg_dt$cty
ctyIncEProjTrendAsfReg_plot <-
  ggplot(incElasCtyReg_dt[CJ((fifthYr),
                              ("Exogenous trend"))], 
         aes(y = incElas,
             x = as.numeric(year),
             shape = region, 
             color = region)) +
  geom_point(size = 0.5) +
  facet_wrap(~ ldf, 
             nrow = 2) +
  theme_bw() + theme_opts +
  labs(shape = "Region",
       color = "Region",
       x = "Year",
       y = "Income elasticity of demand") + 
  scale_shape_manual(values = regShape) +
  scale_color_manual(values = regColor) +
  guides(shape = guide_legend(nrow = 1,
                              override.aes = list(size = 6)),
         color = guide_legend(override.aes = list(linetype = 0)))

# # save plot
# ggsave(
#   file = "unrepFig_incEProjTrendCty.tiff",
#   width = 12,
#   height = 9,
#   ctyIncEProjTrendAsfReg_plot,
#   compression = "lzw"
# )

}