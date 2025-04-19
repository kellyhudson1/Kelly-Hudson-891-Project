# Title: demaFactIncGrp 
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: factors of demand by WB income group

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("RColorBrewer")
library("scales")

# LOAD DATA====
# * total demand====
if(!exists("ldfCtyProj_dt")) {
  ldfCtyProj_dt <- readRDS("procData_ldfCtyProj_dt.Rda")
}

setkey(ldfCtyProj_dt, year, scen, ldfUnit, ldf)
rmCtyProj_dt <- ldfCtyProj_dt[CJ(
  c("2020", "2050"),
  c("SSP1_ref",
    "SSP2_ref",
    "SSP3_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty"),
  "food",
  c("Beef", "Sheep", "Pork"))]

# * population====
#popCtyProj_dt <- readRDS(file = "procData_popCtyProj_dt.Rda")
popCtyProj_dt <- fread("rawData_popCtyProj_dt.csv")
popCtyProj_dt$year <- as.character(popCtyProj_dt$year)

setkey(popCtyProj_dt, year, scen)
popCtyProj_dt <- popCtyProj_dt[CJ(
  c("2020", "2050"),
  c("SSP1_ref",
    "SSP2_ref",
    "SSP3_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty")
)]

# * income====
income_dt <- readRDS(file = "procData_income_dt.Rda")

# keep 2020 and 2050
setkey(income_dt, year, scen)
incCty_dt <- income_dt[CJ(
  c("2020", "2050"),
  c("SSP2_ref",
    "SSP2_rm100LAllCty",
    "SSP2_rm100LHiCty",
    "SSP1_ref",
    "SSP3_ref")
)]

# * price====

prices_dt <- fread("rawData_prices_dt.csv")
prices_dt$year <- as.character(prices_dt$year)
colnames(prices_dt)

# keep 2020 and 2050 for red meat
setkey(prices_dt, year, c)
rmPriceCty_dt <- prices_dt[CJ(
  c("2020", "2050"),
  c("cbeef", "clamb", "cpork"))]

unique(rmPriceCty_dt$scen)

# rename scenarios
rmPriceCty_dt[scen == "PCX_SSP1_ref_dt", scen := "SSP1_ref"]
rmPriceCty_dt[scen == "PCX_SSP2_ref_dt", scen := "SSP2_ref"]
rmPriceCty_dt[scen == "PCX_SSP3_ref_dt", scen := "SSP3_ref"]
rmPriceCty_dt[scen == "PCX_SSP2_rm100LAllCty_dt", scen := "SSP2_rm100LAllCty"]
rmPriceCty_dt[scen == "PCX_SSP2_rm100LHiCty_dt", scen := "SSP2_rm100LHiCty"]

names(rmPriceCty_dt)[names(rmPriceCty_dt) == "c"] <- "ldf"
rmPriceCty_dt[ldf =="cbeef", ldf := "Beef"]
rmPriceCty_dt[ldf =="clamb", ldf := "Sheep"]
rmPriceCty_dt[ldf =="cpork", ldf := "Pork"]

# * merge====
# merge demand and price as both endogenous
rmDemaPriceCty_dt <- merge(rmCtyProj_dt, rmPriceCty_dt,
                        by = c("scen", "ldf", "cty", "year"),
                        all = TRUE)

# complete cases
rmDemaPriceCty_dt <- rmDemaPriceCty_dt[complete.cases(rmDemaPriceCty_dt), ]

# high income vs not high income column
rmDemaPriceCty_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "low- & middle-income countries"]
rmDemaPriceCty_dt[(wbCtyIncClass == "High income"), wbCtyIncClass := "high-income countries"]

rmDemaPriceCty_dt[, valueDema := demaTot000Tons * price]


# by income group
rmDemaPriceIncGrp_dt <-
  rmDemaPriceCty_dt[, .(demaTot000Tons =  sum(demaTot000Tons),
                        valueDema =  sum(valueDema)),
                    by = .(scen, ldf, year, wbCtyIncClass)]
rmDemaPriceIncGrp_dt[, price :=  valueDema / demaTot000Tons]

# all world
rmDemaPriceWld_dt <-
  rmDemaPriceCty_dt[, .(demaTot000Tons =  sum(demaTot000Tons),
                        valueDema =  sum(valueDema)),
                    by = .(scen, ldf, year)]
rmDemaPriceWld_dt[, price :=  valueDema / demaTot000Tons]

# population by income group
# high income vs not high income column
popCtyProj_dt[!(wbCtyIncClass == "High income"), wbCtyIncClass := "low- & middle-income countries"]
popCtyProj_dt[wbCtyIncClass == "High income", wbCtyIncClass := "high-income countries"]

# by income group
popIncGrp_dt <-
  popCtyProj_dt[, .(pop000Peop =  sum(pop000Peop)),
                    by = .(scen, year, wbCtyIncClass)]
popWld_dt <-
  popCtyProj_dt[, .(pop000Peop =  sum(pop000Peop)),
                by = .(scen, year)]

# add pop to total demand so can compute per person demand

# by income group
rmDemaPriceIncGrp_dt <- merge(rmDemaPriceIncGrp_dt, popIncGrp_dt,
                              by = c("scen", "year", "wbCtyIncClass"))
rmDemaPriceIncGrp_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000) / (pop000Peop *
                                                                       1000)]
rmDemaPriceIncGrp_dt$valueDema <- NULL
rmDemaPriceIncGrp_dt$pop000Peop <- NULL
rmDemaPriceIncGrp_dt <- dcast(rmDemaPriceIncGrp_dt,
                              scen + year + wbCtyIncClass ~ ldf,
                              value.var = c("demaTot000Tons", "demaPerPersKg", "price"))
rmDemaPriceIncGrp_dt <- melt(rmDemaPriceIncGrp_dt,
                             id.vars = c("scen", "year", "wbCtyIncClass"))

# for world
rmDemaPriceWld_dt <- merge(rmDemaPriceWld_dt, popWld_dt,
                              by = c("scen", "year"))
rmDemaPriceWld_dt[, demaPerPersKg := (demaTot000Tons * 1000 * 1000) / (pop000Peop *
                                                                            1000)]
rmDemaPriceWld_dt$valueDema <- NULL
rmDemaPriceWld_dt$pop000Peop <- NULL
rmDemaPriceWld_dt <- dcast(rmDemaPriceWld_dt,
                              scen + year ~ ldf,
                              value.var = c("demaTot000Tons", "demaPerPersKg", "price"))
rmDemaPriceWld_dt <- melt(rmDemaPriceWld_dt,
                             id.vars = c("scen", "year"))

# merge population and income by income group
incPopCty_dt <- merge(incCty_dt, popCtyProj_dt,
                       by = c("scen", "cty", "year"),
                       all = TRUE)
incPopIncGrp_dt <-
  incPopCty_dt[, .(pop000Peop =  sum(pop000Peop),
                   incTot = sum(incTot)),
                by = .(scen, year, wbCtyIncClass)]
incPopIncGrp_dt[ , perPersInc := (incTot * 1000 * 1000000)/
                (pop000Peop * 1000) ]
incPopIncGrp_dt$incTot <- NULL
incPopIncGrp_dt <- melt(incPopIncGrp_dt,
                        id.vars = c("scen", "year", "wbCtyIncClass"))

# merge population and income all World
incPopWld_dt <-
  incPopCty_dt[, .(pop000Peop =  sum(pop000Peop),
                   incTot = sum(incTot)),
               by = .(scen, year)]
incPopWld_dt[ , perPersInc := (incTot * 1000 * 1000000)/
                   (pop000Peop * 1000) ]
incPopWld_dt$incTot <- NULL
incPopWld_dt <- melt(incPopWld_dt,
                        id.vars = c("scen", "year"))



rmDemaFactIncGrp_dt <- rbind(incPopIncGrp_dt, rmDemaPriceIncGrp_dt)
rmDemaFactWld_dt <- rbind(incPopWld_dt, rmDemaPriceWld_dt)
rmDemaFactWld_dt$wbCtyIncClass <- "all countries"

rmDemaFact_dt <- rbind(rmDemaFactIncGrp_dt, rmDemaFactWld_dt)
rmDemaFact_dt <- dcast(rmDemaFact_dt,
                       scen + wbCtyIncClass + variable ~ year)

rmDemaFact_dt[, percChange := 100 * (`2050` - `2020`) / `2020`]



# * plot demand factors====
# theme
theme_opts <- list(
  theme(
    strip.background = element_rect(fill = "white"),
    strip.text.x = element_text(size = 10),
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.key = element_rect(color = NA, fill = NA),
    legend.key.size = unit(0.5, "cm"),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 8, angle = 90, vjust = 0),
    axis.text.y = element_text(size = 8),
    axis.title = element_text(size = 12),
    aspect.ratio = 1,
    panel.spacing = unit(0, "lines")
  ))

# rename all factors
levels(rmDemaFact_dt$variable) <-
  c(
    "population",
    "per person income",
    "total demand beef",
    "total demand pork",
    "total demand sheep",
    "per person demand beef",
    "per person demand pork",
    "per person demand sheep",
    "price beef",
    "price pork",
    "price sheep"
  )

# variable as ordered factor
rmDemaFact_dt$variable <-
  factor(
    rmDemaFact_dt$variable,
    ordered = TRUE,
    levels = c(
      "total demand beef",
      "total demand sheep",
      "total demand pork",
      "per person demand beef",
      "per person demand sheep",
      "per person demand pork",
      "price beef",
      "price sheep",
      "price pork",
      "per person income",
      "population"
      )
  )


# rename with scenario long names
rmDemaFact_dt[scen == "SSP1_ref", 
              scenL := "SSP1 & reference values"]
rmDemaFact_dt[scen == "SSP2_ref",
              scenL := "SSP2 & reference values"]
rmDemaFact_dt[scen == "SSP3_ref",
              scenL := "SSP3 & reference values"]

rmDemaFact_dt[scen == "SSP2_rm100LAllCty",
              scenL := "SSP2 & 100% lower all cty"]
rmDemaFact_dt[scen == "SSP2_rm100LHiCty",
              scenL := "SSP2 & 100% lower HIC"]

# scenario long names as ordered factor
rmDemaFact_dt$scenL <-
  factor(
    rmDemaFact_dt$scenL,
    ordered = TRUE,
    levels = c(
      "SSP1 & reference values",
      "SSP2 & reference values",
      "SSP2 & 100% lower HIC",
      "SSP2 & 100% lower all cty",
      "SSP3 & reference values"
    )
  )

# scenario colors

# for SSPs, match to previous figs
brewColorSsp <- brewer.pal(11, name = 'BrBG')

# for SSP2 variants use shades of grey
scenColor <- c(brewColorSsp[[8]],  # SSP1
               "#ffffff",          # SSP2
               "#bdbdbd",          # SSP2 100% lower HIC
               "#525252",          # SSP2 100% lower all cty
               brewColorSsp[[3]])  # SSP3


{
dodge <- position_dodge(width = 0.7)
  
  demaFact_plot <- ggplot(data = rmDemaFact_dt, 
                              aes(x = variable)) +
    geom_bar(
      aes(y = percChange,
          fill = scenL),
      stat = "identity",
      width = 0.5, 
      position = dodge,
      color = "black",
      size = 0.3) +
    coord_flip() +
    scale_x_discrete(labels = wrap_format(15)) +
    facet_wrap(~ wbCtyIncClass, 
               scales = "free_x",
               nrow = 1) + 
    labs(y = "Percent change between 2020 and 2050",
         x     = " ",
         fill = "Income and its elasticty") +
    scale_fill_manual(values = scenColor) +
    theme_bw() + theme_opts + 
    guides(fill = guide_legend(direction = "vertical", 
                               nrow = 2)) 
  # save plot
  ggsave(
    file = "output_fig5_demaFact.tiff",
    width = 10,
    height = 5,
    demaFact_plot,
    compression = "lzw"
  )
  
  }

# * plot prices====

setkey(rmDemaFact_dt, variable)
rmPrice_dt <- rmDemaFact_dt[ c("price beef",
                              "price pork",
                              "price sheep")]
rmPrice_dt$percChange <- NULL
names(rmPrice_dt)[names(rmPrice_dt) == "variable"] <- "price"

rmPrice_dt <- melt(rmPrice_dt,
                   id.vars = c("wbCtyIncClass",
                               "scen",
                               'scenL',
                               "price"),
                   variable.factor = FALSE,
                   variable.name = "year")

# rename with scenario long names
rmPrice_dt[scen == "SSP1_ref", scen := "SSP1"]
rmPrice_dt[scen == "SSP2_ref", scen := "SSP2"]
rmPrice_dt[scen == "SSP3_ref", scen := "SSP3"]

rmPrice_dt[scen == "SSP2_rm100LAllCty", scen := "SSP2 100% lower all cty"]
rmPrice_dt[scen == "SSP2_rm100LHiCty", scen := "SSP2 100% lower HIC"]

# scenario long names as ordered factor
rmPrice_dt$scen <-
  factor(
    rmPrice_dt$scen,
    ordered = TRUE,
    levels = c(
      "SSP3",
      "SSP2 100% lower HIC",
      "SSP2 100% lower all cty",
      "SSP2",
      "SSP1"
    )
  )

rmPrice_dt[price == "price beef", price := "beef"]
rmPrice_dt[price == "price sheep", price := "sheep"]
rmPrice_dt[price == "price pork", price := "pork"]

rmPrice_dt$price <-
  factor(
    rmPrice_dt$price,
    ordered = TRUE,
    levels = c(
      "beef",
      "sheep",
      "pork"
    )
  )

  # plot
  redMeatPrices_plot <- ggplot(data = rmPrice_dt, 
                              aes(x = value / 1000,
                                  y = scen,
                                  shape = year,
                                  color = year)) +
    geom_point(size = 2, stroke = 1) +
    geom_hline(aes(yintercept = as.integer(rmPrice_dt$scen)),
               color = "gray", size = 0.3,
               linetype = "dashed") +
    facet_wrap(price ~ wbCtyIncClass) + 
    scale_y_discrete(labels = wrap_format(15)) +
    scale_x_continuous(labels = scales::number_format(accuracy = 1)) +
    labs(shape = "Year",
         color = "Year",
         x = expression(
      atop(textstyle("Price ($ kg"^-1*")"))),
      y     = "Scenario") +
    theme_bw() + theme_opts +
    guides(shape = guide_legend(nrow = 1, 
                                override.aes = list(size = 6))) +
    theme(legend.margin = margin(t = -0.5, r = 0, b = -0.1, l = 0, unit = "cm"),
          legend.key.height = unit(0, "cm")) +
    scale_color_manual(values = c("#2c7bb6", "#fdae61")) +
    scale_shape_manual(values = c(16, 2))
  
    # save plot
  ggsave(
    file = "output_figSI9_redMeatPrices.tiff",
    width = 9,
    height = 9,
    redMeatPrices_plot,
    compression = "lzw"
  )
  