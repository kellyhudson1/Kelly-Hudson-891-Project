# HEADER========================================================================
# Title: maps
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: map of projected % change in per person demand for ldf 

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("sf")
library("rnaturalearth")
library("scales")
library("viridis")
library("RColorBrewer")

# disable scientific notation
options(scipen = 999)

# LOAD AND ORG DATA=============================================================

# * load cty====
if(!exists("ldfCtyProj_dt")) {
  ldfCtyProj_dt <- readRDS(file = "procData_ldfCtyProj_dt.Rda")
}

# * organize data====

# keep SSP2 with ref income elasticities
# for each SSP in 2020 and 2050 for "food"

mapLdfCty_dt <- ldfCtyProj_dt[scen == "SSP2_ref" &
                                    (year == "2020" |
                                       year == "2050") &
                                    ldfUnit == "food", ]


colnames(mapLdfCty_dt)
names(mapLdfCty_dt)[names(mapLdfCty_dt) == "impCountry"] <- "country"

# keep relevant columns
mapLdfCty_dt <- mapLdfCty_dt[, c("cty",
                                 "country",
                                 "reg",
                                 "year",
                                 "ldf",
                                 "demaPerPersKg",
                                 "demaTot000Tons")]

# long to wide
mapLdfCty_dt <- dcast(mapLdfCty_dt,
                        ldf + cty + country + reg ~ year,
                        value.var = c("demaPerPersKg",
                                      "demaTot000Tons"))

# % change
# 2020 to 2050
nYrs <- 2050 - 2020

# annual percent change, 2020 to 2050
mapLdfCty_dt[, annPCPP20502020 := round(100 * ((demaPerPersKg_2050 / demaPerPersKg_2020) ^
                                                      (1 / nYrs)- 1), 2)]

mapLdfCty_dt[, annPCTot20502020 := round(100 * ((demaTot000Tons_2050 / demaTot000Tons_2020) ^
                                                      (1 / nYrs)- 1), 2)]


# convert long to wide so can merge all cty with map and no NA is given to
# cty without data

mapLdfCty_dt <- dcast(mapLdfCty_dt,
                      country + cty ~ ldf,
                      value.var = "annPCPP20502020")


# rename column so can merge
# add demand to iso cty list
faoCtyReg_dt <- readRDS(file = "procData_faoCtyReg_dt.Rda")
names(faoCtyReg_dt)[names(faoCtyReg_dt) == "isoCty"] <- "iso_a3"
names(faoCtyReg_dt)[names(faoCtyReg_dt) == "impCountry"] <- "country"


# each isoCty need a demand so merge on cty
mapLdfCty_dt <- merge(faoCtyReg_dt, mapLdfCty_dt,
      by = c("cty", "country"),
      all= TRUE, 
      allow.cartesian = TRUE)

# MAP DATA TABLE================================================================

# load raw data for map
worldData_dt <- ne_countries(scale = "medium", returnclass = "sf")


# merge map data with ldf data
worldDataLdf_dt <- merge(mapLdfCty_dt,
                         worldData_dt,
                         by = "iso_a3",
                         all.y  = TRUE)


# keep needed columns
worldDataLdf_dt <- worldDataLdf_dt[, c("geounit",
                                       "Beef",
                                       "Sheep",
                                       "Pork",
                                       "Poultry",
                                       "Milk",
                                       "Eggs",
                                       "geometry")]

# now wide to long so can use facet to plot
worldDataLdf_dt <- melt(
  worldDataLdf_dt,
  id.vars = c("geounit",
              "geometry"),
  variable.name = "ldf",
  value.name = "annPC20502020"
)

# order ldf
worldDataLdf_dt$ldf <- ordered(worldDataLdf_dt$ldf,
                               levels = c("Beef",
                                          'Sheep',
                                          "Pork",
                                          "Poultry",
                                          "Milk",
                                          "Eggs"))

# CREATE MAP====

# * theme options====
theme_opts <- theme(
  plot.title = element_text(hjust = 0.5, size = 20),
  panel.background = element_rect(fill = "white"),
  strip.background = element_rect(fill = "white", color = "black", size = 0.5),
  strip.text = element_text(size = 18),
  panel.border = element_rect(color = "black", fill = NA, size = 0.5), 
  legend.position = "bottom",
  legend.text = element_text(size = 12),
  legend.title = element_text(size = 16),
  axis.text = element_blank(),
  axis.title = element_text(size = 12),
  axis.ticks = element_blank(),
  panel.spacing = unit(0, "cm")
  
  
)

# * color scheme====
# map is for discrete data, so need a discrete scale

# range in whole numbers is -1 to 6
min(worldDataLdf_dt$annPC20502020, na.rm = TRUE)

max(worldDataLdf_dt$annPC20502020, na.rm = TRUE)

{
# list colors to use manually
colorList7 <- c(
  "#f0f0f0",
  "#bdbdbd",
  "#969696",
  "#737373",
  "#525252",
  "#252525",
  "#000000"
)


# * color bins====
# discrete bins for data
# range of values per color
discScale1 <- "-1–0"
discScale2 <- " 0–1"
discScale3 <- " 1–2"
discScale4 <- " 2–3"
discScale5 <- " 3–4"
discScale6 <- " 4–5"
discScale7 <- " 5–6"


worldDataLdf_dt[annPC20502020 <  0, annPC20502020F := discScale1]
worldDataLdf_dt[annPC20502020 >=  0 &
                 annPC20502020 < 1, annPC20502020F := discScale2]
worldDataLdf_dt[annPC20502020 >=  1 &
                 annPC20502020 < 2, annPC20502020F := discScale3]
worldDataLdf_dt[annPC20502020 >=  2 &
                 annPC20502020 < 3, annPC20502020F := discScale4]
worldDataLdf_dt[annPC20502020 >=  3 &
                 annPC20502020 < 4, annPC20502020F := discScale5]
worldDataLdf_dt[annPC20502020 >=  4 &
                 annPC20502020 < 5, annPC20502020F := discScale6]
worldDataLdf_dt[annPC20502020 >=  5, annPC20502020F  := discScale7]


# percent change as ordered factor
worldDataLdf_dt$annPC20502020F <- ordered(worldDataLdf_dt$annPC20502020F,
                               levels = c(discScale1,
                                          discScale2,
                                          discScale3,
                                          discScale4,
                                          discScale5,
                                          discScale6,
                                          discScale7
                                          ))

levels(worldDataLdf_dt$annPC20502020F)

# map=====
mapLdfGlo <- ggplot(worldDataLdf_dt) +
  geom_sf(aes(fill     = annPC20502020F,
              geometry = geometry),
          colour   = "black") +
  scale_fill_manual(
    values  = colorList7,
    na.value = "#ffffff",
    guide = guide_legend(
      direction = "horizontal",
      nrow = 1,
      byrow = TRUE,
      label.position = "top",
      keyheight = 2,
      keywidth = 2,
      title.vjust = 0.3
    )
  ) +
  facet_wrap(~ ldf, ncol = 2) +
  theme_opts +
  theme(legend.text = element_text(margin = margin(r = 30, unit = "pt"))) +
  labs(title = "",
       x     = "",
       y     = "",
       fill  = "Annual percent change (2020 to 2050)") +
  coord_sf(ylim = c(-60, 90),
           expand = FALSE)

# save map
ggsave(
    file = "output_figSI5_ldfPerChangPpsCty.tiff",
    mapLdfGlo,
    width  = 12,
    height = 10,
    compression = "lzw"
    )


}

# summarize data
# positive skew as mean is larger than median
sumStat_dt <- data.table(worldDataLdf_dt)[, as.list(summary(annPC20502020, na.rm = TRUE)), 
                            by = "ldf"]
sumStat_dt <- data.frame(lapply(sumStat_dt, function(y) if(is.numeric(y)) round(y, 2) else y)) 

fwrite(sumStat_dt, "output_tableSI2_ctySumStatPercChange.csv")
