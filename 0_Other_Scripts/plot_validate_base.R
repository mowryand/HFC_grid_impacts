# Produce three plots, comparing PSO runs to ERCOT historicals: aggregate load, gen energy by type, footprint prices

##### LIBRARIES #####
library(data.table)
library(ggplot2)
library(gridExtra)

##### INPUTS #####

if(F) {
  GGPLOT_SIZE = 11
  vec.scn <- paste0("Scn_", c("000990", "000991"))
}

# Pull in miscellaneous functions
source(paste0(PATH.SCRIPTS, "library.R"))


### Pull in sim results
dt.ed <- scenarioYearStitch.getAreaDispatch(vec.scn)

dt.ed[, month := format(as.Date(stop, tz = "ETC/GMT+6"), format = "%Y-%m-01", tz = "ETC/GMT+6")]
dt.ed[, peak := ifelse(wkdy %in% c(6,7), 
                       ifelse(he %in% 7:22, "2x16", "2x8"),
                       ifelse(he %in% 7:22, "5x16", "5x8"))]

dt.load.sim <- dt.ed[ara == 0, list(mwh = sum(load)), by = list(month)]
# dt.price.sim <- dt.ed[ara == 0, list(price = mean(loadprice)), by = list(month, peak)]
dt.price.sim <- dt.ed[ara != 0, list(lmp = weighted.mean(loadprice, load)), by = list(month, peak)]

lst.inj <- scenarioYearStitch.getInjectorInfo(vec.scn)
dt.gen.sim <- merge(lst.inj[[1]][, list(inj, stop, p)], lst.inj[[2]][, list(inj, gen_type = type)], by = "inj")
dt.gen.sim <- dt.gen.sim[, list(mwh = sum(p)), by = list(stop, gen_type)]
dt.gen.sim[, month := format(as.Date(stop, tz = "ETC/GMT+6"), "%Y-%m-01")]
dt.gen.sim <- dt.gen.sim[, list(mwh = sum(mwh)), by = list(month, gen_type)]


### Pull in actuals
dt.load.act <- fread(paste0(PATH.INPUTS, "misc sources/DemandandEnergy2019_highlights.csv"))
dt.load.act <- dt.load.act[, list(month = format(as.Date(month, format = "%m/%d/%Y")), mwh)]

dt.gen.act <- fread(paste0(PATH.INPUTS, "misc sources/DemandandEnergy2019_highlights.csv"))
dt.gen.act[, month := format(as.Date(month, format = "%m/%d/%Y"))]
dt.gen.act <- melt(dt.gen.act, id.vars = c("month"), value.name = "mwh", variable.name = "gen_type")
dt.gen.act <- dt.gen.act[!gen_type %in% c("mwh", "mw_max_hourly")]

# dt.price.act <- fread(paste0(PATH.INPUTS, "misc sources/ERCOT_2019_spp_highlights.csv"))
dt.price.act <- fread(paste0(PATH.INPUTS, "misc sources/ev_ercot_lmp.csv"))
setnames(dt.price.act, tolower(gsub(" ", "_", names(dt.price.act))))
dt.price.act <- dt.price.act[grepl("LZ", price_node_name), list(month = format(as.Date(`local_datetime_(hour_ending)`, format = "%m/%d/%Y %H:%M"), "%Y-%m-01"), 
                                                                loc = price_node_name,
                                                                wkdy = as.integer(format(as.Date(`local_datetime_(hour_ending)`, format = "%m/%d/%Y %H:%M"), "%u")),
                                                                # spp = as.numeric(spp), 
                                                                lmp = as.numeric(`lmp_price_$/mwh`), 
                                                                he = as.integer(tstrsplit(tstrsplit(`local_datetime_(hour_ending)`, split = " ")[[2]], ":")[[1]])+1)]
# approximate peak definitions
dt.price.act[, peak := ifelse(wkdy %in% c(6,7), 
                              ifelse(he %in% 7:22, "2x16", "2x8"),
                              ifelse(he %in% 7:22, "5x16", "5x8"))]
# dt.price.act <- dt.price.act[, list(spp = mean(spp)), by = list(month, peak, loc)]
dt.price.act <- dt.price.act[, list(lmp = mean(lmp)), by = list(month, peak, loc)]

dt.loads <- fread(paste0(PATH.INPUTS, "misc sources/DemandandEnergy2019_highlights2.csv"))
dt.loads[, month := format(as.Date(month, format = "%m/%d/%Y"))]
dt.loads <- melt(dt.loads, id.vars = c("month"), variable.name = "loc", value.name = "mwh")
dt.price.act <- merge(dt.price.act, dt.loads, by = c("month", "loc"))
# dt.price.act <- dt.price.act[, list(spp = weighted.mean(spp, mwh)), by = list(month, peak)]
dt.price.act <- dt.price.act[, list(lmp = weighted.mean(lmp, mwh)), by = list(month, peak)]


### Plot: Load
dt.load <- rbind(cbind(dt.load.act, type = "act"),
                 cbind(dt.load.sim, type = "sim"))
dt.load[, month := factor(x = format(as.Date(month), "%b"), 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))]
dt.load[, mwh_norm := mwh/max(mwh), by = type]
catn("Mean monthly abs error of (normalized load): ", round(dcast(dt.load, month ~ type, value.var = "mwh_norm")[, mean(abs(sim - act))]*100, 1), "%")

plt.load <- ggplot(dt.load) + theme_bw(GGPLOT_SIZE) + theme(panel.border = element_rect(color = "white")) +
  geom_bar(mapping = aes(x = month, y = mwh_norm, fill = type), stat = "identity", position = position_dodge()) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1, color = "grey50") + 
  labs(x = NULL, y = NULL, title = "Normalized Energy Demand") +
  # scale_fill_brewer("", type = "qual", palette = 6, breaks = c("act", "sim"), labels = c("2019\nActual", "2033\nSimulated"))
  scale_x_discrete(breaks = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_fill_manual("", breaks = c("act", "sim"), 
                    labels = c("2019\nActual", "2033\nSim."),
                    values = c("darkgrey", "brown"))


### Plot: Generation
dt.gen <- rbind(cbind(dt.gen.act, type = "Actual"),
                 cbind(dt.gen.sim, type = "Sim."))
dt.gen[, month := factor(x = format(as.Date(month), "%b"), 
                         levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))]
dt.gen[, gen_type := factor(x = gen_type, levels = rev(c("Coal", "Natural Gas", "Water", "Uranium", "Solar", "Wind", "Other Fuel")))]
dt.gen <- dt.gen[gen_type != "Other Fuel"]
dt.gen[, share := mwh/sum(mwh), by = list(month, type)]
catn("Mean monthly abs error of (normalized gen-type): ", round(dcast(dt.gen, month + gen_type ~ type, value.var = "share")[, mean(abs(`Sim.` - Actual))]*100, 1), "%")


month_labs = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")
names(month_labs) = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
plt.gen <- ggplot(dt.gen) + 
  theme_bw(GGPLOT_SIZE) + theme(panel.border = element_rect(color = "white")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5, size = rel(.8))) +
  theme(strip.background = element_rect(color = "white", fill = "grey90")) +
  geom_bar(mapping = aes(x = type, y = share, fill = gen_type), stat = "identity") +
  labs(x = NULL, y = NULL, title = "Normalized Energy Generation") +
  scale_fill_manual("", 
                    breaks = rev(c("Coal", "Natural Gas", "Water", "Uranium", "Solar", "Wind")),
                    values = rev(c("darkgrey", "brown", "lightblue", "darkorange", "yellow", "darkgreen"))) +
  facet_wrap(~month, nrow = 1, labeller = labeller(month = month_labs)) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1, color = "grey50") 


### Plot: Capacity Stack
dt.cap.sim <- lst.inj[[2]][, list(cap = sum(max)), by = list(gen_type = type)][gen_type != "Charger"]
dt.cap.act <- fread(paste0(PATH.INPUTS, "misc sources/CDR_capacity.csv"), header = T)
dt.cap.act <- dt.cap.act[, list(cap = sum(`2019`)), by = list(gen_type = type_new)]
dt.cap <- rbind(cbind(dt.cap.act, type = "Actual"),
                cbind(dt.cap.sim, type = "Sim."))
# dt.cap <- rbind(cbind(dt.cap.act, type = "2019\nActual"),
#                 cbind(dt.cap.sim, type = "2033\nSim."))
dt.cap[, gen_type := factor(x = gen_type, levels = rev(c("Coal", "Natural Gas", "Water", "Uranium", "Solar", "Wind", "Other Fuel")))]
dt.cap <- dt.cap[gen_type != "Other Fuel"]

plt.cap <- ggplot(dt.cap) + 
  theme_bw(GGPLOT_SIZE) + theme(panel.border = element_rect(color = "white")) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  theme(strip.background = element_rect(color = "white", fill = "grey90")) +
  # geom_bar(mapping = aes(x = type, y = cap, fill = gen_type), stat = "identity", position = position_fill()) +
  geom_bar(mapping = aes(x = type, y = cap/1e3, fill = gen_type), stat = "identity", position = position_stack()) +
  labs(x = NULL, y = NULL, title = "Capacity Mix (GW)") +
  scale_fill_manual("", 
                    breaks = rev(c("Coal", "Natural Gas", "Water", "Uranium", "Solar", "Wind")),
                    values = rev(c("darkgrey", "brown", "lightblue", "darkorange", "yellow", "darkgreen"))) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1, color = "grey50") 


### Plot: Price
dt.price <- rbind(cbind(dt.price.act[, list(month, peak, price = lmp)], type = "act"),
                 cbind(dt.price.sim[, list(month, peak, price = lmp)], type = "sim"))
dt.price[, month := factor(x = format(as.Date(month), "%b"), 
                          levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))]
dt.price <- dt.price[peak %in% c("2x8", "5x16")]

plt.price <- ggplot(dt.price) + theme_bw(GGPLOT_SIZE) + theme(panel.border = element_rect(color = "white")) +
  geom_line(mapping = aes(x = month, y = price, color = type, linetype = peak, group = interaction(type, peak)), 
            stat = "identity", size = 2) +
  geom_hline(yintercept = 0) + geom_hline(yintercept = 1, color = "grey50") + 
  labs(x = NULL, y = NULL, title= "System Average Prices ($/MWh)") +
  scale_linetype_manual("", breaks = c("2x8", "5x16"), 
                        labels = c("Wkend\nOff-Pk", "Wkday\nOn-Pk"),
                        values = c("dotted", "solid")) +
  # scale_color_brewer("", type = "qual", palette = 6, breaks = c("act", "sim"), labels = c("2019\nActual", "2033\nSimulated"))
  scale_x_discrete(breaks = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"), 
                   labels = c("J", "F", "M", "A", "M", "J", "J", "A", "S", "O", "N", "D")) +
  scale_color_manual("", breaks = c("act", "sim"), 
                    labels = c("2019\nActual", "2033\nSim."),
                    values = c("darkgrey", "brown"))

# grid.arrange(plt.load, {plt.price+guides(color = F)}, {plt.gen+guides(fill = F)}, plt.cap, 
#              layout_matrix = rbind(c(1,1,1,3,3,3,4,4), 
#                                    c(2,2,2,3,3,3,4,4)))
plt.return <- gridExtra::arrangeGrob(
  plt.load, 
  {plt.price+guides(color = F)}, 
  {plt.gen+guides(fill = F)}, 
  plt.cap, 
 layout_matrix = rbind(c(1,1,1,3,3,3,4,4), 
                       c(2,2,2,3,3,3,4,4)))
