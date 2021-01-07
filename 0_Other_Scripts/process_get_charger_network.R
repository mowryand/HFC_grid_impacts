# Adds chargers and their batteries to the network files
 
##### INPUTS #####

if(F) {
  extractScenarioVariables(getSettingsFromScn("Scn_000980"))
  future.year <- 2033
  bln.write.to.file <- F
  mapping.file.name <- "parsed_bus_info"
  path.write <- paste0(PATH.DATA, "Network1/PSO/", "Scn_000980", "/")
}

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### MAP CHARGER LOCATIONS TO NODES #####

dt.chargers <- getChargingNetwork("tesla", "TX", cap.mod = cap.mod, future.year = future.year)
dt.nodes <- getNodes(scenario, mapping.file.name)
dt.nodes <- dt.nodes[bus_type == "Load"] # only allow connections to load nodes

# Find closest node to each charger
# https://en.wikipedia.org/wiki/K-d_tree
lst.results <- RANN::nn2(data = as.matrix(dt.nodes[, list(lat, long)]),
                         query = as.matrix(dt.chargers[, list(lat, long)]),
                         k = 1)
dt.map <- cbind(dt.chargers, dt.nodes[lst.results[[1]], list(node, x = long, y = lat, area, volt)])
# dt.map[, list(stations = .N, chargers = sum(stalls)), by = area][order(stations, decreasing = T)]
dt.nodal.agg <- dt.map[, list(mw = sum(mw), stalls = sum(stalls)), by = list(node, area)]


if(rpt.all.node.lmp == "chg") {
  tbl.name <- "_NDE_ID"
  dt.write <- fread(paste0(path.write, scenario, tbl.name, ".csv"))
  dt.write[`//Enode` %in% dt.nodal.agg$node, ReportNode := 1]  
  if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
} 

##### Translate chargers to INJECTOR tables #####

tbl.name <- "_INJ_ID"
catn(tbl.name)
chr.names = c("//Injector","Name","Area","LoadFlag","Link","MaxMw","MinMw",
              "RaiseRR","LowerRR","RampCapOnly","EnergyCost","CostAdder",
              "RampUpCost","RampDnCost")

dt.write <- dt.nodal.agg[, list(paste0(node, "_chg"), paste0(node, "_chg"), area, 1, NA, mw, NA,
                          NA, NA, NA, NA, NA,
                          NA, NA)]

setnames(dt.write, chr.names)
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)

# 
tbl.name <- "_INJ_NET"
catn(tbl.name)
chr.names <- c("//Injector","Node","PhysicalArea","LossFactor","IgnoreLoss")

dt.write <- dt.nodal.agg[, list(paste0(node, "_chg"), node, NA, NA, NA)]
setnames(dt.write, chr.names)
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)


##### Now attach batteries at the nodes #####

if(bln.add.batteries) {
  tbl.name <- "_INJ_ID"
  catn(tbl.name)
  chr.names = c("//Injector","Name","Area","LoadFlag","Link","MaxMw","MinMw",
                "RaiseRR","LowerRR","RampCapOnly","EnergyCost","CostAdder",
                "RampUpCost","RampDnCost")
  
  if(grepl("COPY", bat.add.subset.RAW[1]) | grepl("NA", bat.add.subset.RAW[1])) {
    # Copy the battery topology from reference scenario
    ref.inj <- fread(paste0(PATH.IO, "Scn_", bat.add.subset.RAW[2], "/Scn_", bat.add.subset.RAW[2], tbl.name, ".csv"))
    dt.write <- ref.inj[grepl("_bat", `//Injector`)]
  } else {
    ### Size the batteries
    # Mental model: this is the base battery pack
    bat.atts <- getTeslaPowerpackAtts() 
    pe.ratio <- bat.atts$power/bat.atts$energy
    # Calc batts per charger to match charger nameplate
    bats.per.charger <- getTeslaSuperchargerAtts()$power/getTeslaPowerpackAtts()$power 
    # Resize according to mods
    min.bats.per.charger <- bat.pow.min/bat.atts$power
    bats.per.charger <- bats.per.charger*bat.pow.mod 
    bats.per.charger <- max(c(bats.per.charger, min.bats.per.charger))
    # Reconsitute bat.atts so that it now represents one resized battery assembly
    bat.atts$power <- bat.atts$power*bats.per.charger
    bat.atts$energy <- bat.atts$energy*bats.per.charger
    
    if(is.null(bat.add.subset)) {
      bat.nodes <- unique(dt.nodal.agg$node)
    } else {
      bat.nodes <- bat.add.subset
    }
    
    dt.write <- dt.nodal.agg[node %in% bat.nodes, 
                             list(paste0(node, "_bat"), paste0(node, "_bat"), area, NA, NA, 
                                  bat.atts$power*stalls, -bat.atts$power*stalls,
                                  NA, NA, NA, bat.atts$vom, NA,
                                  NA, NA)]
    
    setnames(dt.write, chr.names)
  }
  if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
  # 
  tbl.name <- "_INJ_NET"
  catn(tbl.name)
  chr.names <- c("//Injector","Node","PhysicalArea","LossFactor","IgnoreLoss")
  
  if(grepl("COPY", bat.add.subset.RAW[1])) {
    ref.net <- fread(paste0(PATH.IO, "Scn_", bat.add.subset.RAW[2], "/Scn_", bat.add.subset.RAW[2], tbl.name, ".csv"))
    dt.write <- ref.net[grepl("_bat", `//Injector`)]
  } else {
    dt.write <- dt.nodal.agg[node %in% bat.nodes, list(paste0(node, "_bat"), node, NA, NA, NA)]
    setnames(dt.write, chr.names)
  }
  if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
  # 
  tbl.name <- "_SRG_ID"
  catn(tbl.name)
  chr.names <- c("//Injector","MinStorage","MaxStorage","Efficiency","Storage0","RechargeCost","DischargeCost")
  
  if(grepl("COPY", bat.add.subset.RAW[1])) {
    ref.srg <- fread(paste0(PATH.IO, "Scn_", bat.add.subset.RAW[2], "/Scn_", bat.add.subset.RAW[2], tbl.name, ".csv"))
    dt.write <- ref.srg[grepl("_bat", `//Injector`)]
  } else {
    dt.write <- dt.nodal.agg[node %in% bat.nodes, 
                             list(paste0(node, "_bat"), 0, bat.atts$energy*stalls, bat.atts$efficiency, 0, 0, 0)]
    setnames(dt.write, chr.names)
  }
  if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = F)
}

#####
# Some diagnostic/plotting functions
plotChargerFeederMap <- function(circle.nodes = NA) {
  library(tmap)
  library(sf)
  library(sp)
  
  # Texas state boundaries
  tx_st_filename <- "EIA and DOE topo data/TX State/Texas_State_Boundary.shp"
  sf.tx_st_bound <- st_read(dsn = paste0(PATH.INPUTS, tx_st_filename))
  
  # Features
  plot.chargers <- dt.map[, list(mw, area, wkt = paste0("POINT (", long, " ", lat, ")"))]
  sf.chargers <- st_as_sf(plot.chargers, wkt = "wkt", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  plot.nodes <- unique(dt.map[, list(node, x, y, area, volt)])[, list(node, area, volt, wkt = paste0("POINT (", x, " ", y, ")"))]
  sf.nodes <- st_as_sf(plot.nodes, wkt = "wkt", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  plot.feeders <- dt.map[, list(wkt = paste0("LineString (", long, " ", lat, ", ", x, " ", y, ")"))]
  sf.feeders <- st_as_sf(plot.feeders, wkt = "wkt", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  
  if(!is.na(circle.nodes)) {
    special.nodes <- plot.nodes[node %in% circle.nodes]
    sf.special <- st_as_sf(special.nodes, wkt = "wkt", crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")
  }
  
  # Get a bounding box to zoom in on the DFW-H-SA triangle
  bbox.new <- st_bbox(sf.tx_st_bound)
  # bbox.new[1] <- -99.4; bbox.new[2] <- 29.0; bbox.new[3] <- -94.3; bbox.new[4] <- 34.0;
  
  tmap.plt <- tm_shape(sf.tx_st_bound, bbox = bbox.new, unit = "mi") + # Texas
    tm_polygons(col = "gray95") + tm_borders(lwd = 2, col = "black") + 
    
    tm_shape(sf.feeders, bbox = bbox.new) + # Feeders
    tm_lines(lwd = 1, col = "black") +
    
    tm_shape(sf.nodes, bbox = bbox.new) + # Nodes
    tm_symbols(size = .75, col = "black", alpha = .5, border.lwd = 1, scale = 1, shape = 1, border.col = "black") +
    
    tm_shape(sf.chargers, bbox = bbox.new) + # Chargers
    tm_symbols(size = .75, col = "area", alpha = 1, border.lwd = 1, scale = 1, shape = 18, border.col = "black") +
    
    tm_layout(frame = T, outer.margins = 0.01, inner.margins = 0, # Formatting
              outer.bg.color = "gray50", bg.color = "lightblue")
  
  if(!is.na(circle.nodes)) tmap.plt <- tmap.plt + 
    tm_shape(sf.special, bbox = bbox.new) + # Special chargers
    tm_symbols(size = .75, col = "red", alpha = 1, border.lwd = 1, scale = 2, shape = 1)
  
  return(tmap.plt)
}
getStationSummaryStats <- function() {
  
  # summary(dt.map$volt)
  # dt.map[node %in% c(7315, 60091, 38131, 38360)]
  
  dt.ret <- dt.map[, list(stations = .N, `chg/stat` = round(sum(stalls)/.N), nameplate = sum(mw), median_volt = median(volt, na.rm = T)), 
         by = area][order(nameplate, decreasing = T)]
  return(dt.ret)
}
plotFeederLineDist <- function() {
  library(geosphere)
  library(ggplot2)
  
  dt.dist <- copy(dt.map)
  # dt.dist <- dt.map[node %in% c(7315, 60091, 38131, 38360)]
  
  dt.dist[, meters := Matrix::diag(distm(x = matrix(c(dt.dist$long, dt.dist$lat), ncol = 2, byrow = F), 
                                         y = matrix(c(dt.dist$x, dt.dist$y), ncol = 2, byrow = F), 
                                         fun = distHaversine))]
  # Feeder line distribution
  plt1 <- ggplot(dt.dist) + theme_bw(12) + geom_hline(yintercept = 0) +
    geom_histogram(mapping = aes(x = meters/1000, fill = ifelse(area %in% c("WZ_WEST", "WZ_FAR_WEST", "WZ_NORTH"), "North/West", "Other")),
                   color = "black", binwidth = 10) + 
    labs(x = "Interconnection Length (km)", y = "# of Stations") +
    scale_fill_brewer("", type = "qual", palette = 6)
  
  
  plt2 <- ggplot(dt.dist) + theme_bw(12) + geom_hline(yintercept = 0) +
    geom_histogram(mapping = aes(x = meters/1000, 
                                 fill = node %in% c(38131, 38360, 60091, 7315, 6560)),
                   color = "black", binwidth = 10) + 
    labs(x = "Interconnection Length (km)", y = "# of Stations") +
    scale_fill_brewer("Problem\nStation?", type = "qual", palette = 6)
  
  return(list(plt1, plt2, dt.dist))
  
  # # 38131 charger upgrade lines
  # dt.up <- data.table(i = c(38090, 38120), 
  #                     j = c(38120, 38129), 
  #                     long = c(-103.288, -103.447), 
  #                     lat = c(31.5537, 31.4916), 
  #                     x = c(-103.447, -103.530), 
  #                     y = c(31.4916, 31.4232))
  # dt.up[, meters := Matrix::diag(distm(x = matrix(c(dt.up$long, dt.up$lat), ncol = 2, byrow = F), 
  #                                      y = matrix(c(dt.up$x, dt.up$y), ncol = 2, byrow = F), 
  #                                      fun = distHaversine))]
}

if(F) {
  plotChargerFeederMap() 
  getStationSummaryStats()
}