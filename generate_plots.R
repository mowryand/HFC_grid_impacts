# Script to generate the plots for paper

##### PACKAGES -----

library(data.table)
library(XML)
library(ggplot2)
library(xtable)
library(cowplot)
library(gridExtra)

set.seed(123456)

##### CONSTANTS -----

#_#_#_# CHANGE THIS ACCORDING TO DIRECTORY SYSTEM PATH #_#_#_#
CHR.USER <- Sys.info()["user"]
PATH.BASE <- paste0("C:/Users/", CHR.USER, "/Dropbox (MIT)/EV Charging Infra/GITHUB_UPLOAD") 
#_#_#_# CHANGE THIS ACCORDING TO DIRECTORY SYSTEM PATH #_#_#_#

# Other paths
PATH.SCRIPTS <- paste0(PATH.BASE, "/0_Other_Scripts/")
PATH.INPUTS <- paste0(PATH.BASE, "/1_Inputs/")
PATH.TOPOLOGY <- paste0(PATH.INPUTS, "Network1/")
# PATH.IO <- paste0(PATH.BASE, "/2_Model_IO/")
PATH.IO <- paste0("C:/Users/", CHR.USER, "/Dropbox (MIT)/EV Charging Infra/Data/Network1/PSO/")
PATH.IMAGES <- paste0(PATH.BASE, "/3_Plots/")

# Other
topology.name <- "2018.JUN.Monthly.Auction.NetworkModel_PeakWD.raw"
dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))

##### VARIABLES -----

# Arranging scenarios for presentation
scn.to.load <- list(
  "cap_mod 000%" = list( # Batteries but no EVs
    base = paste0("Scn_", c("000990", "000991")),
    conc_0 = paste0("Scn_", c("000990", "000991")), # 0% battery
    conc_05 = paste0("Scn_", c("000900", "000901")), # 5% battery
    conc_10 = paste0("Scn_", c("000902", "000903")), # 10% battery 
    conc_15 = paste0("Scn_", c("000904", "000905")) # 15% battery 
    # conc_20 = paste0("Scn_", c("000906", "000907")) # 20% battery
  ),
  "cap_mod 050%" = list(
    base = paste0("Scn_", c("000990", "000991")),
    disp_0 = paste0("Scn_", c("000992", "000993")),
    conc_0 = paste0("Scn_", c("000994", "000995"))
  ),
  "cap_mod 075%" = list(
    base = paste0("Scn_", c("000990", "000991")),
    disp_0 = paste0("Scn_", c("000977", "000978")),
    conc_0 = paste0("Scn_", c("000987", "000988")), # 0% battery 
    conc_05 = paste0("Scn_", c("000920", "000921")), # 5% battery 
    conc_10 = paste0("Scn_", c("000922", "000923")), # 10% battery 
    conc_15 = paste0("Scn_", c("000924", "000925")), # 15% battery 
    conc_20 = paste0("Scn_", c("000926", "000927")), # 20% battery
    dr_05 = paste0("Scn_", c("000952", "000953")), # 5% Demand Response
    dr_10 = paste0("Scn_", c("000954", "000955")), # 10% Demand Response
    dr_15 = paste0("Scn_", c("000956", "000957")), # 15% Demand Response
    dr_20 = paste0("Scn_", c("000958", "000959")) # 20% Demand Response
  ),
  "cap_mod 100%" = list(
    base = paste0("Scn_", c("000990", "000991")),
    disp_0 = paste0("Scn_", c("000970", "000971")), 
    conc_0 = paste0("Scn_", c("000980", "000981")), # 0% battery 
    conc_05 = paste0("Scn_", c("000930", "000931")), # 5% battery 
    conc_10 = paste0("Scn_", c("000932", "000933")), # 10% battery 
    conc_15 = paste0("Scn_", c("000934", "000935")), # 15% battery 
    conc_20 = paste0("Scn_", c("000936", "000937")), # 20% battery
    dr_05 = paste0("Scn_", c("000942", "000943")), # 5% Demand Response
    dr_10 = paste0("Scn_", c("000944", "000945")), # 10% Demand Response
    dr_15 = paste0("Scn_", c("000946", "000947")), # 15% Demand Response
    dr_20 = paste0("Scn_", c("000948", "000949")) # 20% Demand Response
  ),
  "cap_mod 125%" = list(
    base = paste0("Scn_", c("000990", "000991")),
    disp_0 = paste0("Scn_", c("000975", "000976")),
    conc_0 = paste0("Scn_", c("000985", "000986")), # 0% battery 
    conc_05 = paste0("Scn_", c("000960", "000961")), # 5% battery 
    conc_10 = paste0("Scn_", c("000962", "000963")), # 10% battery 
    conc_15 = paste0("Scn_", c("000964", "000965")), # 15% battery 
    conc_20 = paste0("Scn_", c("000966", "000967")), # 20% battery
    dr_05 = paste0("Scn_", c("000912", "000913")), # 5% Demand Response
    dr_10 = paste0("Scn_", c("000914", "000915")), # 10% Demand Response
    dr_15 = paste0("Scn_", c("000916", "000917")), # 15% Demand Response
    dr_20 = paste0("Scn_", c("000918", "000919")) # 20% Demand Response
  ),
  "cap_mod 150%" = list(
    base = paste0("Scn_", c("000990", "000991")),
    disp_0 = paste0("Scn_", c("000996", "000997")),
    conc_0 = paste0("Scn_", c("000998", "000999"))
  ) 
)

# Which injectors (HFCs) to highlight in the plots
vec.inj <- c("A" = "38131_chg", "B" = "7315_chg", "C" = "13436_chg", "D" = "38360_chg", "E" = "3471_chg", "F" = "8367_chg")

# Common size for all plots. This variable is accessed through the environment by sourced scripts.
GGPLOT_SIZE = 11

# Pull in miscellaneous functions
source(paste0(PATH.SCRIPTS, "library.R"))


##### GENERATE FIGURES -----

# Fig 2: Maps of EVSE and Power networks -----
chr.filename <- paste0("2_facet_map.png")
source(paste0(PATH.SCRIPTS, "plot_tmap_all.R"))
# circle.nodes = as.integer(gsub("_chg", "", vec.inj))
plts <- plotStandardTopologies(base.scenario = "Scn_000980", circle.nodes = vec.inj)

plt <- tmap_arrange(plts[["gen+xm"]], plts[["evse"]], nrow = 1)
tmap::tmap_save(tm = plt, filename = paste0(PATH.IMAGES, chr.filename),
                width = 8, height = 4, units = "in")


# Fig 3: EVSE charging distributions -----
lst.plt <- getChargerUseProfile(return.plt = T, show.segments = c("DC Fast", "Residential"))
plt.save <- gridExtra::arrangeGrob(
  lst.plt[[1]] + theme_bw(GGPLOT_SIZE) + theme(legend.position = "bottom"), 
  lst.plt[[4]] + theme_bw(GGPLOT_SIZE), 
  lst.plt[[5]] + theme_bw(GGPLOT_SIZE), 
  layout_matrix = rbind(c(1,2),                         c(1,3)))
ggsave(filename = "evse_inl_dist.png", path = PATH.IMAGES, plot = plt.save,
       device = "png", width = 8.5, height = 4, units = "in")


# Fig 4: Base case gen/load/price comparison to ERCOT 2019 actuals -----
chr.filename <- "validate_base.png"
vec.scn <- paste0("Scn_", c("000990", "000991"))
source(paste0(PATH.SCRIPTS, "plot_validate_base.R"))
plt.return <- gridExtra::arrangeGrob(
  plt.load + guides(fill = F), 
  plt.price + guides(color = F, linetype = F), 
  plt.gen + guides(fill = F), 
  plt.cap, 
  get_legend({plt.load + theme(legend.position = "bottom")}),
  get_legend({plt.price + guides(color = F) + theme(legend.position = "bottom")}),
  layout_matrix = rbind(c(1,1,1,1,1,1,3,3,3,3,3,3,4,4,4,4), 
                        c(1,1,1,1,1,1,3,3,3,3,3,3,4,4,4,4), 
                        c(2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4),
                        c(2,2,2,2,2,2,3,3,3,3,3,3,4,4,4,4),
                        c(7,5,7,7,6,7,3,3,3,3,3,3,4,4,4,4)))
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt.return, 
       device = "png", width = 8.5, height = 3.5, units = "in")


# Fig 5, 7: Results -----
source(paste0(PATH.SCRIPTS, "plot_results_yearstitch.R"))

chr.filename = "results_main.png"
plt1.4 <- plotChargerImpacts(scn.list = scn.to.load[2:6], metric = "cost")
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt1.4 + theme(legend.position = "bottom"),
       device = "png", width = 8.5, height = 3, units = "in")

chr.filename = "results_mitigation.png"
plt2.4 <- plotBatteryMitigation(scn.list = scn.to.load[3:5], metric = "cost")
plt4.4 <- plotFlexibilityMitigation(scn.list = scn.to.load[3:5])
plt <- gridExtra::arrangeGrob({plt2.4 + guides(fill=F) + theme(axis.text.x = element_text(size = rel(0.85)))},
                              {plt4.4 + guides(fill=F) + theme(axis.text.x = element_text(size = rel(0.85))) + labs(y=NULL) },
                              get_legend({plt2.4 + theme(legend.position = "bottom")}),
                              layout_matrix = rbind(c(1, 1, 2, 2), c(1, 1, 2, 2), c(1, 1, 2, 2), c(1, 1, 2, 2), c(1, 1, 2, 2), c(3, 3, 3, 3)))
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt, 
       device = "png", width = 8.5, height = 3, units = "in")


# Fig 6: Boxplot lineups -----
source(paste0(PATH.SCRIPTS, "plot_results_chg_boxplots_yearstitch.R"))

chr.filename <- paste0("violins_local.png")
scn.to.load2 <- list(
  from = paste0("Scn_", c("000970", "000971")), # Disp
  to = paste0("Scn_", c("000980", "000981")) # Conc
)
dt.lmps <- boxplots.getData(scn.to.load2)
dt.ordered.impacts <- dt.lmps[variable == "lmp_dif", mean(abs(value)), by = inj][order(V1, decreasing = T), list(inj, value = V1)] # SAVE FOR LATER PLOT
plt1 <- boxplots.plotQuantiles(scn.to.load2, dt.lmps, vec.inj)
plt2 <- boxplots.plotSpecViolins(scn.to.load2, dt.lmps, vec.inj)
pltLeg <- get_legend({plt1 + theme(legend.position = "bottom")})
plt <- gridExtra::arrangeGrob(
  {plt1 + theme_bw(base_size = GGPLOT_SIZE) +
      scale_y_log10(name = "|Δ(HFC LMP)| ($/MWh)", limits = c(0.0005, 18677.22), labels = function(x) format(x, scientific = F, drop0trailing = T)) +
      scale_x_continuous(name = "Individual HFCs", breaks = seq_along(dt.lmps[, unique(inj)])) + 
      theme(axis.text.x = element_text(color = "white", angle = 90, vjust = .5)) +
      labs(title = "Local Effects Distributions by HFCs") + guides(color = F, fill = F)}, 
  {plt2 + theme_bw(base_size = GGPLOT_SIZE) +
      scale_y_log10(name = NULL, limits = c(0.0005, 18677.22), labels = function(x) format(x, scientific = F, drop0trailing = T)) +
      scale_x_discrete(name = "Maximum Demand (MW)") + 
      theme(axis.text.x = element_text(angle = 90, vjust = .5)) + 
      labs(title = "Selected HFCs")},
  pltLeg,
  layout_matrix = rbind(c(1,1,2), c(1,1,2), c(1,1,2), c(1,1,2), c(1,1,2), c(1,1,2), c(1,1,2), c(1,1,2), c(3,3,3)))
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt, 
       device = "png", width = 8.5, height = 3, units = "in")


# Fig 8: Value of Storage -----
source(paste0(PATH.SCRIPTS, "plot_results_yearstitch.R"))
chr.filename <- paste0("res_value_of_storage.png")
plt5.3 <- plotStorageCostBenefit(scn.list = scn.to.load[c(1,3,4,5)])[[3]]
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt5.3 + theme(legend.position = "bottom"), 
       device = "png", width = 4, height = 3, units = "in")


# Fig 9: Network profile 1 -----
source(paste0(PATH.SCRIPTS, "plot_tmap_all.R"))
chr.filename <- paste0("9_profile_1.png")
plts2 <- plotStandardTopologies(base.scenario = "Scn_000980", circle.nodes = vec.inj, dt.evse.colors = dt.ordered.impacts)
tmap::tmap_save(tm = plts2[["evse_col"]], filename = paste0(PATH.IMAGES, chr.filename),
                width = 4, height = 4, units = "in")


# Fig 9: Network profile 2 -----
chr.filename <- paste0("9_profile_2.png")
scenario <- c("Scn_000981") 
node.of.interest <- 38131
num.order <- 6
time.interval <- as.POSIXct(c("2013-08-21 17:00:00", "2013-08-21 18:00:00"), tz = "ETC/GMT+6")
graph.network.layout <- c("geo", "auto")[2]
graph.subset.type <- c("ego", "bbox")[1]
bbox <- c(-98-25/60, 29+9/60, -95-30/60, 30+10/60)
source(paste0(PATH.SCRIPTS, "plot_pf_net.R"))
pltss <- plotPfNet(dt.edges.pro, dt.nodes.pro)
SCALESSS <- 1.4
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = pltss[[1]], 
       device = "png", width = 4*SCALESSS, height = 4*SCALESSS, units = "in")


# Fig A3, demand flexibility -----
source(paste0(PATH.SCRIPTS, "plot_results_yearstitch.R"))
chr.filename <- "demand_flexibility.png"
scn.list.flex <- list(
  dr_0 = paste0("Scn_", c("000980", "000981")), # 0% Flexibility
  dr_05 = paste0("Scn_", c("000942", "000943")), # 5% Flexibility
  dr_10 = paste0("Scn_", c("000944", "000945")), # 10% Flexibility
  dr_15 = paste0("Scn_", c("000946", "000947")), # 15% Flexibility
  dr_20 = paste0("Scn_", c("000948", "000949")) # 20% Flexibility
)
plt1 <- plotFlexibilityResults(scn.list = scn.list.flex)
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt1, 
       device = "png", width = 6, height = 3, units = "in")


# Fig A4, durations -----
source(paste0(PATH.SCRIPTS, "plot_charger_heatmaps_and_congestion_durations.R"))
chr.filename <- paste0("congestion_durations.png")
lst.inj <- scenarioYearStitch.getInjectorInfo(vector.scenarios = c("Scn_000980", "Scn_000981")) # 100% concentrated no mitigation
dt.inj <- merge(lst.inj[[1]], lst.inj[[2]], by = "inj")
plt <- plotDurations2(dt.inj, these.inj = vec.inj)
plt <- plt + theme_bw(base_size = GGPLOT_SIZE)
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt, 
       device = "png", width = 8.5, height = 3, units = "in")


# Fig A5, impact rankings -----
source(paste0(PATH.SCRIPTS, "plot_results_yearstitch.R"))
chr.filename <- "impact_rankings.png"
plt1 <- plotInjectorRankings(scn.list = scn.to.load[2:6], vec.inj)
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt1, 
       device = "png", width = 6, height = 3, units = "in")

# Fig A6, feeder line distribution -----
source(paste0(PATH.SCRIPTS, "process_get_charger_network.R"))
chr.filename <- "feeder_length_dist.png"
plt <- plotFeederLineDist()[[1]]
ggsave(filename = chr.filename, path = PATH.IMAGES, plot = plt, 
       device = "png", width = 8, height = 3, units = "in")
