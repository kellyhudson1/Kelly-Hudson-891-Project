# HEADER========================================================================
# Title: ldfRegYrFigTab
# Author: Adam Komarek
# Date last edited: 08 August 2021
# Script run in R version 4.0.5
# Open project 'project_ldfDemand.Rproj' before running any scripts
# Study title: "Income, consumer preferences, and the future of livestock-derived food demand"
# Description: plot demand for ldf over time by region, tables for % of total

# SSP2_ref is the main reference case scenario

# LIBRARIES=====================================================================
library("data.table")
library("ggplot2")
library("ggpubr")
library("RColorBrewer")

# disable scientific notation
options(scipen = 999)


# LOAD DATA=====================================================================
# region demand for each ldf
ldfRegAll_dt <- readRDS(file = "procData_ldfRegAll_dt.Rda")

# country demand for each ldf
if(!exists("ldfCtyAll_dt")) {
  ldfCtyAll_dt <- readRDS(file = "procData_ldfCtyAll_dt.Rda")
}

# aggregated data for protein summed across all 6 ldf
apdRegAll_dt <- readRDS(file = "procData_apdRegAll_dt.Rda")

# PLOTS=========================================================================
# * plot options====
# ** theme options====
theme_opts <- list(
  theme(
    strip.text = element_text(size = 16),
    strip.background = element_rect(fill = "white"),
    legend.position = "bottom",
    legend.text  = element_text(size = 12),
    legend.title = element_text(size = 16),
    panel.grid = element_blank(),
    axis.text.x = element_text(size = 12,
                               angle = 0),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14,
                                margin = margin(t = 0,
                                         r = -3,
                                         b = 0,
                                         l = 0,
                                         unit = "mm")),
    aspect.ratio = 1,
    panel.spacing = unit(0, "cm")
    
  )
)

# ** guide options====

guide_opts <- list(
  guides(shape = guide_legend(nrow = 3,
                              override.aes = list(size = 4, stroke = 1.5)),
         color = guide_legend(override.aes = list(linetype = 0))))

# ** region shape====
regShape <- c(15, 16, 17,
               18, 6, 4,
               5, 0, 1)

# ** region color====
brewColor <- brewer.pal(9, name = 'Set1')
regColor <- c(brewColor[[1]], 
              brewColor[[2]],
              brewColor[[3]],
              brewColor[[4]], 
              brewColor[[5]],
              "#000000",
              brewColor[[7]], 
              brewColor[[8]],
              "#000000")



# report every 5th year
fifthYr <- c(
  "1961",
  "1965",
  "1970",
  "1975",
  "1980",
  "1985",
  "1990",
  "1995",
  "2000",
  "2005",
  "2010",
  "2015",
  "2020",
  "2025",
  "2030",
  "2035",
  "2040",
  "2045",
  "2050"
)

# table LDF protein content=====================================================
ldfProtPercCty_dt <- readRDS(file = "procData_ldfProtPercCty_dt.Rda")


# ldf as ordered factor
ldfProtPercCty_dt$ldf <-
  factor(
    ldfProtPercCty_dt$ldf,
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

protPerc_Tab <- ldfProtPercCty_dt[,      .(
  mean = round(mean(percProtGenusCty), 1),
  min = round(min(percProtGenusCty), 1),
  max = round(max(percProtGenusCty), 1)
),
by = .(ldf, reg)]

# range
protPerc_Tab$range <- paste(protPerc_Tab$min, protPerc_Tab$max, sep ="\226")
protPerc_Tab$range <- paste0("(", protPerc_Tab$range,")")
protPerc_Tab$value <- paste(protPerc_Tab$mean, protPerc_Tab$range, sep = " ")

protPerc_Tab <- protPerc_Tab[, c("ldf", "reg", "value")]

# long to wide
protPerc_Tab <- dcast(protPerc_Tab,
                      reg ~ ldf,
                      value.var = "value"
)


fwrite(protPerc_Tab,
       "output_tableSI1_protPerc.csv",
       row.names = FALSE)


# protein demand figures========================================================
apdRegAll_dt
# keep each fifth year and GENUS protein
setkey(apdRegAll_dt, year, ldfUnit)
apdRegAllPlot_dt <- apdRegAll_dt[CJ(
  fifthYr,
  "proteinGenus"
)]

# keep projected data only after 2019 for SSP2_ref
# and all historical data
apdRegAllPlot_dt <- apdRegAllPlot_dt[
  (year > 2019 & scen == "SSP2_ref") |
    scen == "hist", ]


# * Fig. 2: per person protein demand===========================================

# gen plot
protDemaPersRegYr <-
  ggplot(apdRegAllPlot_dt,
         aes(
           y = 1000 * (apdPerPersKg / 365),
           x = as.numeric(year),
           shape = region,
           color = region)
         ) +
  geom_point(size = 2, 
             stroke = 1, 
             position = position_jitter(w = 0.3, h = 0)) +
  geom_line() + 
  facet_grid(~ dataType, scales = "free") +
  theme_bw() + theme_opts + guide_opts +
  scale_y_continuous(breaks = seq(0, 80, 10)) + 
  labs(shape = "Region",
       color = "Region",
       x = "Year",
       y = expression(
                      atop(textstyle("Per person demand for livestock-derived food"),
                      atop(textstyle("(grams protein day"^-1*")"))))) +
 scale_shape_manual(values = regShape) +
 scale_color_manual(values = regColor)

# save plot
ggsave(
  file = "output_fig2_protDemaPersRegYr.tiff",
  width = 10,
  height = 6,
  protDemaPersRegYr,
  compression = "lzw"
)

# * Fig. 4: total protein demand================================================

setkey(apdRegAllPlot_dt, reg)

# generate total protein demand plot
protDemaTotRegYr <-
  ggplot(apdRegAllPlot_dt[!("WLD")],
         aes(
           y = apdTot000Tons / 1000,
           x = as.numeric(year),
           shape = region,
           color = region
         )) +
  geom_point(size = 2, 
             stroke = 1, 
             position = position_jitter(w = 0.3, h = 0)) +
  geom_line() + 
  facet_grid( ~ dataType, scales = "free") +
  theme_bw() + theme_opts + guide_opts +
  scale_y_continuous(breaks = seq(0, 35, 5)) + 
  labs(shape = "Region",
       color = "Region",
       x = "Year",
       y = expression(
         atop(textstyle("Total demand for livestock-derived food"),
                   atop(textstyle("(million metric tons protein year"^-1*")"))))) +
  scale_shape_manual(values = regShape) +
  scale_color_manual(values = regColor)

# save plot
ggsave(
  file = "output_fig4_protDemaTotRegYr.tiff",
  width = 10,
  height = 6,
  protDemaTotRegYr,
  compression = "lzw"
)

# * PLOT LDF-REG FAO FBS VS. SIMULATED====
{
# keep food 
setkey(ldfRegAll_dt, ldfUnit, year)
ldfRegObsSim_dt <- ldfRegAll_dt[CJ(
  "food",
  fifthYr
)]

# For for 2005 to 2013
# keep projected data in 
# SSP2_ref and historical data

ldfRegObsSim_dt <- ldfRegObsSim_dt[
  ((scen == "hist" & year < 2014) |
     (scen == "SSP2_ref")), ]

# ldf as ordered factor
ldfRegObsSim_dt$ldf <-
  factor(
    ldfRegObsSim_dt$ldf,
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

# change scenario name and reorder
ldfRegObsSim_dt[scen == "hist", scen := "Historical data"]
ldfRegObsSim_dt[scen == "SSP2_ref", 
                scen := "Projected data"]


# ldf as ordered factor
ldfRegObsSim_dt$scen <-
  factor(
    ldfRegObsSim_dt$scen,
    ordered = TRUE,
    levels = c(
      "Historical data",
      "Projected data")
  )

}

# generate plot
{
setkey(ldfRegObsSim_dt, reg)
ldfObsSimPerPersRegYr <-
  ggplot(ldfRegObsSim_dt[!("WLD")],
         aes(
           y = demaPerPersKg,
           x = as.numeric(year),
           shape = scen,
           color = scen
         )) +
  geom_point(size = 0.5, stroke = 0.5) +
  geom_line(size = 0.1) + 
  facet_wrap(ldf ~ reg, nrow = 6,
             scales = "free_y") +
  theme_bw() + theme_opts +
  labs(shape = "",
       color = "",
       x = "Year",
  y = expression(
         textstyle("Per person demand (Kg year"^-1*")"))) + 
  guides(shape = guide_legend(nrow = 1,
                              override.aes = list(size = 6)),
         color = guide_legend(nrow = 1,
                              override.aes = list(linetype = 0))) +
  theme(strip.text = element_text(size = 10),
        axis.text.x = element_text(size = 10,
                                   angle = 90),
        axis.text.y = element_text(size = 10)) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA)) +
  scale_color_manual(values = c("#2c7bb6", "#fdae61")) +
  scale_shape_manual(values = c(16, 2))

                                    
# save plot
ggsave(
  file = "output_figSI7_ldfObsSimPerPersRegYr.tiff",
  width = 10,
  height = 10,
  ldfObsSimPerPersRegYr,
  compression = "lzw"
)

}

# TABLES=========================================================================

# * organize data====

# columns as region (8 + wld = 9) columns as year 
# rows as year (1980, 2010, 2020, 2050) with % change 1980 to 2010 and 2020 to 2050 
# for each ldf (6)

# keep data
ldfRegAllTab_dt <- ldfRegAll_dt[(year == 1980 & dataType == "Historical"|
                                   year == 2010 & dataType == "Historical"|
                                   year == 2020 & dataType == "Projected" & scen == "SSP2_ref" |
                                   year == 2050 & dataType == "Projected" & scen == "SSP2_ref") &
                                        ldfUnit == "food", ]

# region as ordered factor with world listed 1st
ldfRegAllTab_dt$reg <-
  factor(
    ldfRegAllTab_dt$reg,
    ordered = TRUE,
    levels = 
      c("WLD",
        "EAP",
        "EUR",
        "FSU",
        "LAC",
        "MEN",
        "NAM",
        "SAS",
        "SSA")
  )

# ldf as ordered factor
ldfRegAllTab_dt$ldf <-
  factor(
    ldfRegAllTab_dt$ldf,
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

# * demand per person====
# long to wide (reshape)
ldfRegPpsTab_dt <- dcast(ldfRegAllTab_dt,
                            reg + ldf ~ dataType + year,
                            value.var = "demaPerPersKg")


# add percent change
# 1980 to 2010 is same number years as 2020 to 2050
nYrs <- 30

ldfRegPpsTab_dt[, annPC20101980 := round(100 * ((Historical_2010 / Historical_1980) ^
                                                         (1 / nYrs)- 1), 2)]

ldfRegPpsTab_dt[, annPC20502020 := round(100 * ((Projected_2050 / Projected_2020)  ^
                                                   (1 / nYrs)- 1), 2)]


# round all numeric values
ldfRegPpsTab_dt <- cbind(ldfRegPpsTab_dt[, c("reg", "ldf"), 
                                     with = FALSE],
                      ldfRegPpsTab_dt[, c("Historical_1980",
                                        "Historical_2010",
                                        "Projected_2020",
                                        "Projected_2050",
                                        "annPC20101980",
                                        "annPC20502020"), 
                                    with = FALSE][, round(.SD, 1), ])

# wide to long so all numbers in one column
ldfRegPpsLongTab_dt <-
  melt(
    ldfRegPpsTab_dt,
    id.vars = c("reg",
                "ldf"),
    value.name = "demand",
    variable.name = "time"
  )

# long to wide (reshape) so region as individual columns
ldfRegPpsWideTab_dt <- dcast(ldfRegPpsLongTab_dt,
                       ldf + time ~ reg,
                       value.var = "demand")

# save per person demand
fwrite(ldfRegPpsWideTab_dt,
       "output_table2_ldfRegPps.csv",
       row.names = FALSE)


# * total demand====

# demand in millions
ldfRegAllTab_dt[, demaTotMillTons := demaTot000Tons / 1000]

# long to wide (reshape)
ldfRegAllTab_dt$region <- NULL
ldfRegTotTab_dt <- dcast(ldfRegAllTab_dt,
                           reg + ldf ~ dataType + year,
                           value.var = "demaTotMillTons")


# add percent change

ldfRegTotTab_dt[, annPC20101980 := round(100 * ((Historical_2010 / Historical_1980) ^
                                                  (1 / nYrs)- 1), 2)]

ldfRegTotTab_dt[, annPC20502020 := round(100 * ((Projected_2050 / Projected_2020)  ^
                                                  (1 / nYrs)- 1), 2)]


ldfRegTotTab_dt <- cbind(ldfRegTotTab_dt[, c("reg", "ldf"), 
                                     with = FALSE],
                         ldfRegTotTab_dt[, c("Historical_1980",
                                        "Historical_2010",
                                        "Projected_2020",
                                        "Projected_2050",
                                        "annPC20101980",
                                        "annPC20502020"), 
                                     with = FALSE][, round(.SD, 1), ])

# wide to long so all numbers in one column
ldfRegTotTab_dt <-
  melt(
    ldfRegTotTab_dt,
    id.vars = c("reg",
                "ldf"),
    value.name = "demand",
    variable.name = "time"
  )

# long to wide (reshape) so region as individual columns
ldfRegTotTab_dt <- dcast(ldfRegTotTab_dt,
                       ldf + time ~ reg,
                       value.var = "demand")

# save total demand
fwrite(ldfRegTotTab_dt,
       "output_tableSI3_ldfRegTot.csv",
       row.names = FALSE)


# * % cont of each region to ldf demand by ldf==========================

# remove % change rows
ldfRegTotCont_dt <- ldfRegTotTab_dt[!(time == "annPC20101980" |
                                    time == "annPC20502020"), ]


# wide to long 
ldfRegTotCont_dt <-
  melt(
    ldfRegTotCont_dt,
    id.vars = c("ldf",
                "time",
                "WLD"),
    value.name = "demand",
    variable.name = "reg"
  )

# % of total
ldfRegTotCont_dt[, percCont := round(100 *(demand / WLD), 1) ]

# long to wide
ldfRegTotCont_dt <- dcast(ldfRegTotCont_dt,
                       ldf + time + WLD ~ reg,
                       value.var = "percCont")

# * % cont of each ldf to demand by region==============================
# keep data
ldfRegContTot_dt <- ldfRegAll_dt[(year == 1980 & dataType == "Historical" |
                 year == 2010 & dataType == "Historical" |
                 year == 2020 & dataType == "Projected" & scen == "SSP2_ref" |
                 year == 2050 & dataType == "Projected" & scen == "SSP2_ref") &
                ldfUnit == "proteinGenus", ]


# remove regType
ldfRegContTot_dt[, c("ldfUnit", "regType", "scen") := NULL]


# wide to long
ldfRegContTot_dt <- melt(
  ldfRegContTot_dt,
  id.vars = c("reg",
              "region",
              "year",
              "ldf",
              "dataType"),
  variable.name = "demandType",
  value.name =    "demand"
)

# long to wide
ldfRegContTot_dt <- dcast(ldfRegContTot_dt,
                     region + reg + year + dataType + demandType ~ ldf,
                                 value.var = "demand")
# sum
ldfRegContTot_dt[, totDemProt := Beef + Sheep + Pork + Poultry + Eggs + Milk]

# percent
ldfRegContTot_dt[, perBeef    := round(100 * (Beef  / totDemProt), 1)]
ldfRegContTot_dt[, perSheep   := round(100 * (Sheep / totDemProt), 1)]
ldfRegContTot_dt[, perPork    := round(100 * (Pork / totDemProt), 1)]
ldfRegContTot_dt[, perPoultry := round(100 * (Poultry / totDemProt), 1)]
ldfRegContTot_dt[, perEggs    := round(100 * (Eggs / totDemProt), 1)]
ldfRegContTot_dt[, perMilk    := round(100 * (Milk / totDemProt), 1)]

ldfRegContTot_dt[, totPerc  := perBeef + perSheep + perPork +
                perPoultry + perEggs + perMilk]

# wide to long

ldfRegContTot_dt <- melt(
  ldfRegContTot_dt,
  id.vars = c("reg", "region", "year", "dataType", "demandType"),
  variable.name = "item",
  value.name    = "value"
)

# id if total or %
ldfRegContTot_dt[ , percOrTot  :=  "perc"]
ldfRegContTot_dt[item == "Beef" | 
              item == "Sheep" | 
              item == "Pork" |
              item == "Poultry" | 
              item == "Eggs" | 
              item == "Milk"|
              item == "totDemProt"
                , percOrTot :=  "tot"]



ldfRegContTot_dt <- ldfRegContTot_dt[!(item == "totDemProt" |
                                 item == "totPerc")]
                                     
# * stacked bar chart for ldf contribution====

# year as factor
ldfRegContTot_dt$year <- as.factor(ldfRegContTot_dt$year)
ldfRegContTot_dt$year <- factor(ldfRegContTot_dt$year,
                             levels = rev(levels(ldfRegContTot_dt$year)))


brewerColorAsf <- brewer.pal(9, name = 'Greys')
brewerColorAsf
ldfColors <- c(brewerColorAsf[[1]],
               brewerColorAsf[[9]],
               brewerColorAsf[[2]],
               brewerColorAsf[[8]],
               brewerColorAsf[[3]],
               brewerColorAsf[[7]]
)

# region as ordered factor
ldfRegContTot_dt$region <-
  factor(
    ldfRegContTot_dt$region,
    ordered = TRUE,
    levels = c(
      "World",
      "East Asia & Pacific",
      "Europe",
      "Former Soviet Union",
      "Latin America & Caribbean",
      "Middle East & North Africa",
      "North America",
      "South Asia",
      "sub-Saharan Africa"
    )
  )

# plot
# setkey to plot each ldf % of total protein
setkey(ldfRegContTot_dt, demandType, percOrTot)
ldfRegContTot_plot <-
  ggplot(ldfRegContTot_dt[.("demaPerPersKg", "perc")],
         aes(x = year,
             y = value,
             fill = item)) +
  geom_bar(position = "stack", 
           stat = "identity",
           width = 0.5,
           colour = "black") +
  coord_flip() +
  facet_wrap(~ region, 
             nrow = 3) + 
  theme_bw() + theme_opts +
  scale_y_continuous(breaks = seq(0, 100, 25),
                     position = "left") +
  scale_fill_manual(values = ldfColors,
                    labels = c("Beef", "Sheep",
                               "Pork", "Poultry",
                               "Eggs", "Milk")) +
  labs(x = "Year",
       y = "Percent of total demand",
       fill = "Livestock-derived food") + 
  guides(fill  = guide_legend(nrow = 1,
                              reverse = TRUE)) + 
  theme(strip.text = element_text(size = 12),
        axis.text.x = element_text(size = 12,
                                   angle = 0),
        axis.title.y = element_text(size = 16,
                                    margin = margin(r = 1,
                                                    unit = "mm")))

# save plot
ggsave(
  file = "output_figSI6_ldfContTotReg.tiff",
  width  = 9,
  height = 9,
  ldfRegContTot_plot,
  compression = "lzw"
)
