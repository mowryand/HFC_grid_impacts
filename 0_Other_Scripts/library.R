# Library of functions

##### PACKAGES -----

library(lubridate)
library(data.table)
library(R.utils)
library(ggplot2)

##### LIBRARY: UTILITY FUNCTIONS -----

catn <- function(...) cat(paste0("\n", ...))
singleScenarioDeprecatedSplitter <- function(check.scn) {
  if(length(gregexpr("_", check.scn)[[1]]) == 1) {
    ret.scn <- check.scn
  } else {
    ret.scn <- paste0(unlist(tstrsplit(check.scn, "_"))[c(1,2)], collapse = "_")
  }
  return(ret.scn)
}
convertTimeToStop <- function(vec) {
  as.POSIXct(vec, format = "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")
}
getIntervalMapping <- function(scenario, interval = NULL) {
  
  path.results <-   paste0(PATH.IO, scenario, "/results/")
  
  interval.map <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_MS_Int.csv"))
  setnames(interval.map, c("int", "start", "stop"))
  interval.map <- interval.map[, list(int, stop = as.POSIXct(stop, format = "%Y.%m.%d %H:%M", tz = "ETC/GMT+6"))]
  
  if(!is.null(interval)) {
    interval.map <- interval.map[stop > interval[1] & stop <= interval[2]]
  }
  
  suppressWarnings({
    interval.map <- interval.map[, list(int, stop, 
                                        he = as.integer(format(stop, "%H", tz = "ETC/GMT+6")), 
                                        wkdy = as.integer(format(stop, "%u", tz = "ETC/GMT+6")), 
                                        date = as.Date(format(stop, "%Y-%m-%d", tz = "ETC/GMT+6")))]
    interval.map[he == 0, c("he", "wkdy", "date") := list(24, wkdy-1, date-1)]
    interval.map[wkdy == 0, wkdy := 7]
    interval.map[, date := format(date)]
  })
  
  return(interval.map)
}
getHourTable <- function(interval, chr.tz) {
  
  vec.times <- seq(from = interval[1], to = interval[2], by = "1 hour")
  
  suppressWarnings({
    interval.map <- data.table(datetime = vec.times, 
                               he = as.integer(format(vec.times, "%H", tz = chr.tz)), 
                               wkdy = as.integer(format(vec.times, "%u", tz = chr.tz)), 
                               date = as.Date(format(vec.times, "%Y-%m-%d", tz = chr.tz)))
    interval.map[he == 0, c("he", "wkdy", "date") := list(24, wkdy-1, date-1)]
    interval.map[wkdy == 0, wkdy := 7]
    interval.map[, date := format(date)]
  })
  
  return(interval.map)
}
getSettingsFromScn <- function(scenario) {
  fread(paste0(PATH.INPUTS, "run_info.csv"))[scn == scenario][id == max(id)]
}
extractScenarioVariables <- function(dt.settings) {
  scenario <<- dt.settings$scn
  cap.mod <<- as.numeric(dt.settings$cap_mod)
  bln.add.batteries <<- as.logical(dt.settings$bat)
  bat.pow.mod <<- as.numeric(dt.settings$bat_mod) 
  bat.pow.min <<- as.numeric(dt.settings$bat_min) 
  bat.add.subset.RAW <<- strsplit(as.character(dt.settings$bat_loc), "_")[[1]]
  bat.add.subset <<- as.numeric(strsplit(as.character(dt.settings$bat_loc), "_")[[1]])
  bln.mod.xm <<- as.numeric(dt.settings$xm_mod) 
  gen.scenario <<- dt.settings$ltsa
  start.date <<- dt.settings$start
  end.date <<- dt.settings$end
  switch.load.treatment <<- dt.settings$fev
  treat.load.as <<- dt.settings$ld_mdl # So I can see explicit power consumption at all load nodes (long run)
  rpt.all.node.lmp <<- dt.settings$rpt_all # LMPs at nodes. all, none, chg
  description <<- dt.settings$desc
  dr.amnt <<- as.numeric(dt.settings$dr_amnt)
  scn.ref <<- dt.settings$scn_ref
  gbl.xm.rlx <<- as.numeric(dt.settings$xm_rlx)
  xm.sf <<- as.logical(dt.settings$xm_sf)
  xm.viol <<- as.logical(dt.settings$xm_viol)
  bln.curtail <<- as.logical(dt.settings$curtail)
}


##### LIBRARY: RESULTS PROCESSING -----

# Optimization results -----
getOptResults <- function(scenario, summed = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  path.results <-   paste0(PATH.IO, scenario, "/results/")
  
  ### Read CSV tables
  dt.res <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_MC_Solution.csv"))
  
  dt.res <- dt.res[, list(int_start = FirstInterval, int_end = LastInterval, 
                          obj = Objective, cost = AllCost, pen = AllPenalty)]
  if(summed) dt.res <- dt.res[, list(int_start = min(int_start), int_end = max(int_end), 
                                     obj = sum(obj), cost = sum(cost), pen = sum(pen))]
   
  dt.map <- getIntervalMapping(scenario)
  
  dt.res <- merge(dt.res, dt.map, by.x = "int_start", by.y = "int", all.x = T)
  dt.res <- merge(dt.res, dt.map, by.x = "int_end", by.y = "int", suffixes = c("_start", "_end"), all.x = T)
  
  return(dt.res)
  
}
getOptResultsDiff <- function(scenario, summed = T) {
  res1 <- getOptResults(scenario[1], summed)
  res2 <- getOptResults(scenario[2], summed)
  
  return(res2$obj - res1$obj)
}

calculatePenalties <- function(scenario, summed = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  path.results <-   paste0(PATH.IO, scenario, "/results/")
  
  dt.map <- getIntervalMapping(scenario)
  
  ### Read CSV tables
  if(file.exists(paste0(PATH.TOPOLOGY, "PSO/", scenario, "/results/", singleScenarioDeprecatedSplitter(scenario), "_ES_INJ.csv"))) {
    dt.sv <- merge(dt.map, fread(paste0(PATH.TOPOLOGY, "PSO/", scenario, "/results/", singleScenarioDeprecatedSplitter(scenario), "_ES_INJ.csv")), by = "int")[
      , list(sv = sum(Penalty)), by = int]
  } else {
    dt.sv <- data.table(int = vector("integer", 0L), sv = vector("numeric", 0L))
  }
  
  ### Pull from other functions
  dt.xv <- getEdgeViolations(scenario)[, list(xv = sum(penalty)), by = int]
  
  ### Combine
  dt.res <- merge(dt.sv, dt.xv, by = "int", all = T)
  dt.res <- dt.res[, list(int_start = int, int_end = int, pen = ifelse(is.na(sv), 0, sv) + ifelse(is.na(xv), 0, xv), sv = ifelse(is.na(sv), 0, sv), xv = ifelse(is.na(xv), 0, xv))]
  if(F) {
    ggplot(melt(dt.res, id.vars = c("int_start", "int_end"))[variable != "pen"]) + theme_bw() + geom_hline(yintercept = 0) +
      geom_bar(mapping = aes(x = int_start, y = value/1e3, fill = variable), stat = "identity", color = "black") +
      labs(x = "Hour", y = "Penalty ($K)") +
      scale_fill_brewer(type = "qual", palette = 6)
    ggplot(melt(copy(dt.res)[, sv:=sv/pen][, xv:=xv/pen], id.vars = c("int_start", "int_end"))[variable != "pen"]) + theme_bw() + geom_hline(yintercept = 0) +
      geom_bar(mapping = aes(x = int_start, y = value, fill = variable), stat = "identity", color = "black") +
      labs(x = "Hour", y = "Penalty Share") +
      scale_fill_brewer(type = "qual", palette = 6)
  }
  if(summed) dt.res <- dt.res[, list(int_start = min(int_start), int_end = max(int_end), pen = sum(pen))]
  
  dt.res <- merge(dt.res, dt.map, by.x = "int_start", by.y = "int", all.x = T)
  dt.res <- merge(dt.res, dt.map, by.x = "int_end", by.y = "int", suffixes = c("_start", "_end"), all.x = T)
  
  return(dt.res)
}
calculateCosts <- function(scenario, summed = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  path.results <-   paste0(PATH.IO, scenario, "/results/")
  
  dt.map <- getIntervalMapping(scenario)
  
  ### Read CSV tables
  dt.gc <- merge(dt.map, fread(paste0(PATH.TOPOLOGY, "PSO/", scenario, "/results/", singleScenarioDeprecatedSplitter(scenario), "_ED_INJ.csv")), by = "int")[
    , list(gc = sum(CostTotal)), by = int]
  # Note that this CostTotal includes costs from the es and the ed libraries. Basically it's all injector costs: startup, running, ramping, variable, fixed, etc.
  
  ### Combine
  dt.res <- dt.gc[, list(int_start = int, int_end = int, cost = gc)]
  if(summed) dt.res <- dt.res[, list(int_start = min(int_start), int_end = max(int_end), cost = sum(cost))]
  
  dt.res <- merge(dt.res, dt.map, by.x = "int_start", by.y = "int", all.x = T)
  dt.res <- merge(dt.res, dt.map, by.x = "int_end", by.y = "int", suffixes = c("_start", "_end"), all.x = T)
  
  return(dt.res)
}
getOptResultsAccurate <- function(scenario, summed = T, recache = F) {
  #### Check inputs
  stopifnot(length(scenario) == 1)

  catn(scenario)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  # Look for result file already cached
  if(file.exists(paste0(path.results, "cost_cache.csv")) & !recache) {
    # The cache exists and desire not to recache, so read and return the cache file
    dt.res <- fread(paste0(path.results, "cost_cache.csv"))
    dt.res[, c("stop_start", "stop_end") := list(ymd_hms(stop_start), ymd_hms(stop_end))]
  } else {
    # Not yet cached, or desired to recache, so run calcs, cache, and return
    dt.res <- merge(calculateCosts(scenario, summed = F),
                    calculatePenalties(scenario, summed = F), 
                    by = c("int_start", "int_end", "stop_start", "stop_end", "he_start", "he_end", "wkdy_start", "wkdy_end", "date_start", "date_end"))
    dt.res[, obj := cost + pen]
    
    fwrite(x = dt.res[, c("stop_start", "stop_end") := list(format(stop_start, "%Y-%m-%d %H:%M:%S"), format(stop_end, "%Y-%m-%d %H:%M:%S"))], 
           file = paste0(path.results, "cost_cache.csv"))
  }
  
  if(summed == T) {
    dt.res <- dt.res[, list(cost = sum(cost), pen = sum(pen), sv = sum(sv), xv = sum(xv), obj = sum(obj)), 
                     by = list(int_start, int_end, stop_start, stop_end, he_start, he_end, wkdy_start, wkdy_end, date_start, date_end)]
  }
  
  return(dt.res)
}
if(F) {
  # Validation
  test.scenario <- "Scn_001961"
  suppressWarnings(round(getOptResults(test.scenario, summed = T)$pen - calculatePenalties(test.scenario, summed = T)$pen))
  suppressWarnings(round(getOptResults(test.scenario, summed = T)$cost - calculateCosts(test.scenario, summed = T)$cost))
  getOptResultsAccurate(test.scenario, F)[]
}


# Network: Node functions -----

# Pull node network info from nde_id and sub_id tables, and characteristics/coords from nodemapping table
getNodes <- function(scenario, mapping.file.name = "parsed_bus_info", bln.fix.missing.nodes = F) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  ### Function defs
  
  ### Read CSV tables
  dt.nodes <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_NDE_ID.csv"))
  setnames(dt.nodes, c("node", "node_name", "bus", "sub", "rpt"))
  
  dt.areas <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_SUB_ID.csv"))
  setnames(dt.areas, c("sub", "sub_name", "area"))
  
  dt.chars <- fread(paste0(PATH.TOPOLOGY, mapping.file.name, ".csv"))
  setnames(dt.chars, c("bus_name", "node", "area_parsed", "zone", "lz", "long", "lat", "z", "wz", "bus_type", "volt"))
  
  dt.ratings <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_STE_NDE.csv"))
  setnames(dt.ratings, c("state", "node", "gen_rating", "load_rating", "is_gen", "is_load"))
  dt.ratings <- dt.ratings[, list(gen_rating = sum(gen_rating), load_rating = sum(load_rating)), by = list(state, node, is_gen, is_load)]
  
  ### Merge data
  dt.return <- merge(dt.nodes, dt.areas, by = "sub", all.x = T)
  dt.return <- merge(dt.return, dt.chars, by = "node", all.x = T)
  dt.return <- merge(dt.return, dt.ratings, by = "node", all.x = T)
  
  ### Handle underjoined data: coordinates
  if(bln.fix.missing.nodes & nrow(dt.return[is.na(long)]) > 0) {
    dt.edges <- getEdges(scenario)
    catn("Interpolating coordinates for nodes missing them...")
    dt.fixed.nodes <- interpolateNodeCoordsFromEdgeMap(dt.return[, list(node, x = long, y = lat)], 
                                                       dt.edges[, list(from, to)],
                                                       bln.remove.unconnected.nodes = F)
    if(nrow(dt.fixed.nodes[is.na(x)])) catn(nrow(dt.fixed.nodes[is.na(x)]), " nodes with missing coordinates remaining.")
    dt.return[, c("long", "lat") := NULL]
    dt.return <- merge(dt.return, dt.fixed.nodes[, list(node, long = x, lat = y)], by = "node", all.x = T)
  }

  dt.return[, volt := as.numeric(substr(volt, 0, 3))] # "Other" is coerced to NA
  
  ### Return
  return(dt.return[, list(node, node_name, sub, sub_name, bus, bus_name, bus_type, is_load, volt, load_rating, 
                          area, area_parsed, zone, lz, wz, long, lat, z)])
}


# Generator information -----

# Pull times series injector data from model outputs and various maps
# Output: list of two data.tables: one with time invariant, two with time variant data
# For DIFF function, only time-variant data is diffed. (So, injections and prices.) Attributes are taken from second scn
getInjectorInfoTimeInvariant <- function(scenario, omit.node.data = F, bln.fix.missing.nodes = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  # Definitive list of modeled injectors
  dt.gens <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_INJ_ID.csv"))
  dt.gens <- dt.gens[, list(i = `//Injector`, max = MaxMw, min = MinMw)]
  
  # Get generator type/tech from input data (not translated to PSO direct inputs)
  dt.cust <- fread(paste0(PATH.INPUTS, "snl_psse_gen_map.csv"))
  dt.cust <- dt.cust[, list(i = psse_node, type, tech)]
  dt.gens <- merge(dt.gens, dt.cust, by = "i", all.x = T)
  
  if(omit.node.data) {
    dt.gens[grepl("_chg", i), type := "Charger"]
    dt.gens[grepl("_bat", i), type := "Battery"]
    setnames(dt.gens, tolower(names(dt.gens)))
    
    dt.gens[, mdl := gsub("[0-9]*_", "", i)]
    dt.gens[mdl == "LOD", type := "Load"]
    
    return(dt.gens[, list(inj = i, type, tech, max, min)])
  } else {
    # Add node mapping
    dt.map <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_INJ_NET.csv"))
    dt.map <- dt.map[, list(node = Node, i = `//Injector`)]
    dt.gens <- merge(dt.gens, dt.map, by = "i", all.x = T)
    dt.gens[grepl("_chg", i), type := "Charger"]
    dt.gens[grepl("_bat", i), type := "Battery"]
    
    # Attach geographic/charactersitic information
    dt.nodes <- getNodes(scenario, bln.fix.missing.nodes = bln.fix.missing.nodes)
    dt.nodes <- dt.nodes[, list(node, node_name, area, bus_type, volt, lz, wz, x = long, y = lat, z)]
    dt.gens <- merge(dt.gens, dt.nodes, by = "node", all.x = T)
    
    setnames(dt.gens, tolower(names(dt.gens)))
    
    dt.gens[, mdl := gsub("[0-9]*_", "", i)]
    dt.gens[mdl == "LOD", type := "Load"]
    
    return(dt.gens[, list(inj = i, node, node_name, 
                          x, y, z, area, lz, wz, 
                          type, tech, bus_type, 
                          volt, max, min)])
  }
}
getInjectorInfoTimeVariant <- function(scenario, interval.map = NULL, interval = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  # Get Interval-Time map
  if(is.null(interval.map)) interval.map <- getIntervalMapping(scenario, interval)
  
  # Get time series info
  dt.inj <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_ED_Inj.csv"))
  # Dumping all these characteristics because it seems like a memory drag to haul around longer than necessary
  dt.inj[, c("//scn", "RC", "Marginal", "LoadPrice", "BalancePrice", "CostTotal", "CostOfEnergy", 
             "CostOfAdder", "CostOfRamp", "Revenue", "Mileage", "ViolationRR", "PenaltyRR", 
             "Up", "UpCap", "UpMax") := NULL]
  dt.inj <- merge(dt.inj, interval.map, by = "int")  
  
  setnames(dt.inj, tolower(names(dt.inj)))
  
  return(dt.inj[, list(inj, int, stop, p, lmp)])
}
getInjectorInfo <- function(scenario, interval.map = NULL, interval = NULL, bln.ts = T, omit.node.data = F, bln.fix.missing.nodes = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  dt.gens <- getInjectorInfoTimeInvariant(scenario, omit.node.data = omit.node.data, bln.fix.missing.nodes = bln.fix.missing.nodes)
  if(bln.ts) {
    dt.inj <- getInjectorInfoTimeVariant(scenario, interval.map, interval)
  } else {
    dt.inj <- data.table()
  }
  
  return(list(dt.inj, dt.gens))
}
getInjectorInfoDiff <- function(scenario, interval = NULL, bln.ts = T, return.inds = F, omit.node.data = F, bln.fix.missing.nodes = T) {
  #### Check inputs
  stopifnot(length(scenario) == 2)
  
  ### Function defs
  
  ###
  # Get individual results files
  lst.scn <- lapply(X = scenario, FUN = function(this.scn) {
    # Redefine relevant paths
    path.write <-     paste0(PATH.IO, this.scn, "/")
    path.results <-   paste0(PATH.IO, this.scn, "/results/")
    
    interval.map <- getIntervalMapping(this.scn, interval)
    
    # Get scenario injector information
    lst.inj <- getInjectorInfo(this.scn, interval.map, bln.ts = bln.ts, omit.node.data = omit.node.data, bln.fix.missing.nodes = bln.fix.missing.nodes)
    
    return(lst.inj)
  })
  
  if(bln.ts) {
    # And difference the TS
    # (assumes same set of resources, just with diff attributes)
    vec.id.vars <- c("int", "stop", "inj")
    dt.inj <- merge(melt(lst.scn[[1]][[1]], id.vars = vec.id.vars), 
                    melt(lst.scn[[2]][[1]], id.vars = vec.id.vars),
                    by = c(vec.id.vars, "variable"), 
                    all = T)
    dt.inj[, value := ifelse(is.na(value.y), 0, value.y) - ifelse(is.na(value.x), 0, value.x)]
    dt.inj[, value_p := ifelse(is.na(value.y), 0, value.y) / ifelse(is.na(value.x), 0, value.x)]
    
    dt.inj.return <- dcast.data.table(dt.inj, int + stop + inj ~ variable, value.var = "value")
    dt.inj[, variable := paste0(variable, "_p")]
    dt.inj_p.return <- dcast.data.table(dt.inj, int + inj ~ variable, value.var = "value_p")
    dt.inj <- merge(dt.inj.return, dt.inj_p.return, by = c("int", "inj"))
  } else {
    dt.inj <- lst.scn[[1]][[1]]
  }
  
  # Taking non-schedule info from second scenario; assumption that it is constant b/w scenarios
  dt.atts <- lst.scn[[1]][[2]]
    
  if(return.inds) {
    return(list(dt.inj, dt.atts, lst.scn))
  } else {
    return(list(dt.inj, dt.atts))
  }
}

getInjectorEmissions <- function(scenario, return.agg = T) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  # Pull injector dispatch
  lst.inj <- getInjectorInfo(scenario, bln.ts = T, omit.node.data = T, bln.fix.missing.nodes = F)
  # Pull injector heat rates
  dt.heat <- fread(paste0(path.inputs, scenario, "_INJ_HEA.csv"))
  dt.emissions <- merge(lst.inj[[2]], dt.heat[, list(inj = `//Injector`, hr = IncHeat)], by = "inj", all.x = T)
  
  # dt.emissions[is.na(hr)][, .N, by = list(type)]
  dt.emissions[is.na(hr), hr := 0]
  
  # Load carbon intensities
  # https://www.eia.gov/tools/faqs/faq.php?id=73&t=11
  # Texas coal estimate: https://www.eia.gov/coal/production/quarterly/co2_article/co2.html
  dt.ci <- data.table(type = c("Natural Gas", "Coal", "Other Fuel"),
                      intensity = c(117, 210, 100)) # lbs/mmBtu

  dt.emissions <- merge(dt.emissions, dt.ci, by = "type", all.x = T)
  # dt.emissions[is.na(intensity)][, .N, by = type]
  dt.emissions[is.na(intensity), intensity := 0]
  
  dt.emissions <- dt.emissions[, list(inj, ci = hr*intensity)] # lbs/MWh

  dt.emissions <- merge(lst.inj[[1]], dt.emissions, by = "inj", all.x = T)
  # dt.emissions[is.na(ci)]
  
  dt.return <- dt.emissions[, list(inj, int, stop, tons_co2 = p*ci/2000)]
  
  if(return.agg) {
    return(dt.return[, list(tons_co2 = sum(tons_co2)), by = list(int, stop)])
  } else {
    return(dt.return)
  }
          
}


# Network: Edge functions -----

# Pull edge network info from brn_id. return.coords pulls lat/long info from getNodes()
getEdges <- function(scenario, return.coords = F) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  dt.net <- fread(paste0(path.inputs, singleScenarioDeprecatedSplitter(scenario), "_BRN_ID.csv"))
  setnames(dt.net, tolower(names(dt.net)))
  
  dt.return <- dt.net[, list(from = frenode, to = toenode, ckt = circuit, name, 
                             kv = voltage, resist = resistance, react = reactance, lim = normallimit, ctg = ctglimit,
                             solve, enforce, monitor, switchable, penalty, anglelimit)]
    if(return.coords) {
    dt.nodes <- getNodes(scenario = scenario, bln.fix.missing.nodes = T)
    dt.nodes <- dt.nodes[, list(node, long, lat)]
    
    dt.return <- merge(dt.return, dt.nodes, by.x = "from", by.y = "node", all.x = T)
    dt.return <- merge(dt.return, dt.nodes, by.x = "to", by.y = "node", all.x = T, suffixes = c("_from", "_to"))
  }

  
  return(dt.return)
}

# Pull edge network power flow RESULTS from pc_brn. is.na(interval.map) does not return interval mapped to time
getEdgeFlows <- function(scenario, interval.map = NULL, interval = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  if(file.exists(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PC_Brn.csv.gz"))) {
    # Grrr, it's zipped!
    chr.tempfile <- tempfile()
    gunzip(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PC_Brn.csv.gz"), destname = chr.tempfile, remove = F)
    dt.flow <- fread(chr.tempfile)
    file.remove(chr.tempfile)
  } else {
    dt.flow <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PC_Brn.csv"))
  }
  setnames(dt.flow, tolower(names(dt.flow)))
  
  
  dt.flow[, c("from", "to") := tstrsplit(pth, split = " ")]
  dt.flow[, c("from", "to") := list(as.integer(from), as.integer(to))]
  
  if(is.null(interval.map)) interval.map <- getIntervalMapping(scenario, interval)
  
  if(identical(NA, interval.map)) return(dt.flow[, list(from, to, int, mw, loss)])
  
  dt.flow <- merge(dt.flow, interval.map, by = "int")
  return(dt.flow[, list(from, to, int, stop, date, wkdy, he, mw, loss)])
}

# Pull thermal line constraint violations and SPs from pn_pth. is.na(interval.map) does not return interval mapped to time
# (table also has flows, but only for the subset of secured lines without "low control"?)
getEdgeViolations <- function(scenario, interval.map = NULL, interval = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  if(file.exists(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PN_Pth.csv.gz"))) {
    # Grrr, it's zipped!
    chr.tempfile <- tempfile()
    gunzip(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PN_Pth.csv.gz"), destname = chr.tempfile, remove = F)
    dt.violations <- fread(chr.tempfile)
    file.remove(chr.tempfile)
  } else {
    dt.violations <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_PN_Pth.csv"))
  }
  setnames(dt.violations, tolower(names(dt.violations)))
  
  dt.violations[, c("from", "to") := tstrsplit(pth, split = " ")]
  dt.violations[, c("from", "to") := list(as.integer(from), as.integer(to))]
  
  if(is.null(interval.map)) interval.map <- getIntervalMapping(scenario, interval)
  
  if(identical(NA, interval.map)) return(dt.violations[, list(from, to, int, max, mw2 = mw, penalty, violation, binding, sp)])
  
  dt.violations <- merge(dt.violations, interval.map, by = "int")
  return(dt.violations[, list(from, to, int, stop, date, wkdy, he, max, mw2 = mw, penalty, violation, binding, sp)])
}

# Pull time series edge data --- network connections, power flows on edges, and constraint data
getEdgeInfo <- function(scenario, interval.map = NULL, interval = NULL, dt.net = NULL, dt.flows = NULL, dt.violations = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 1)
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  path.inputs <- paste0(PATH.IO, scenario, "/")
  
  if(is.null(interval.map)) interval.map <- getIntervalMapping(scenario, interval)
  
  if(is.null(dt.net)) dt.net <- getEdges(scenario)
  setnames(dt.net, c("from", "to"), c("i", "j"))
  
  if(is.null(dt.flows)) dt.flows <- getEdgeFlows(scenario, interval.map = NA)
  setnames(dt.flows, c("from", "to"), c("i", "j"))
  dt.flows <- merge(dt.flows, interval.map[, list(int)], by = "int", all.y = T)
  
  if(is.null(dt.violations)) dt.violations <- getEdgeViolations(scenario, interval.map = NA)
  setnames(dt.violations, c("from", "to"), c("i", "j"))
  dt.violations <- merge(dt.violations, interval.map[, list(int)], by = "int", all.y = T)
  
  # Merge network connections and time series power flows
  # power flow (time series) data can be missing some edges, so not all edges in dt.net will get int/mw data
  dt.net <- merge(dt.net[, list(kv = mean(kv), lim = sum(lim)), by = list(i, j)], # aggregating multiple circuits
                  dt.flows[, list(i, j, int, mw)], 
                  by = c("i", "j"), all = T)
  
  # Add rows for the missing flow data
  dt.net <- rbind(dt.net[!is.na(int)],
                  as.data.table(merge.data.frame(dt.net[is.na(int)][, int := NULL], 
                                                 interval.map[, list(int)], by = NULL)))
  
  # Merge network/power flows with the ts constraint data
  dt.net <- merge(dt.net, 
                  dt.violations[, list(i, j, int, sp, binding, mw2, violation, penalty)], 
                  by = c("i", "j", "int"), all = T)

  # Coalesce the flow information from PN and PC libraries (they can be incomplete if certain options not specified in PSO)
  # But first perform a sanity check. Where both flow sources are present, they should be equal.
  stopifnot(all(dt.net[!is.na(mw) & !is.na(mw2), round(mw,0) == round(mw2,0)]))
  dt.net[is.na(mw), mw := mw2]
  dt.net[, mw2 := NULL]
  if(nrow(dt.net[is.na(mw)])) catn("Missing flows for ", nrow(unique(dt.net[is.na(mw), list(i, j)])), " lines ",
                                   "(", round(nrow(unique(dt.net[is.na(mw), list(i, j)]))/nrow(unique(dt.net[, list(i, j)]))*100), "%), ")
    
  # Rows missing from the constraint data were not binding: sp -> 0, binding -> 0
  dt.net[is.na(sp), c("sp", "binding") := 0]
  
  return(dt.net)
}
getEdgeInfoDiff <- function(scenario, interval = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 2)
  
  ### Function defs
  
  ###
  # Get individual results files
  lst.scn <- lapply(X = scenario, FUN = function(this.scn) {
    # Redefine relevant paths
    path.write <-     paste0(PATH.IO, this.scn, "/")
    path.results <-   paste0(PATH.IO, this.scn, "/results/")
    
    interval.map <- getIntervalMapping(this.scn, interval)
    
    # Create edge table --- Need network connections, power flows on edges, and constraint data
    dt.edges <- getEdgeInfo(this.scn, interval.map)
    
    return(dt.edges)
  })

  # And difference them
  vec.id.vars <- c("i", "j", "int", "kv")
  dt.edges <- merge(melt(lst.scn[[1]], id.vars = vec.id.vars), 
                    melt(lst.scn[[2]], id.vars = vec.id.vars),
                    by = c(vec.id.vars, "variable"))
  dt.edges[, value := value.y - value.x]
  dt.edges <- dcast.data.table(dt.edges, i + j + int + kv ~ variable, value.var = "value")
  
  ### Return  
  return(dt.edges)
}


# Aggregate results processing -----

# Area dispatch info from ed_ara
getAreaDispatch <- function(scenario, interval.map = NULL, interval = NULL) {
  
  path.results <- paste0(PATH.IO, scenario, "/results/")
  
  if(is.null(interval.map)) interval.map <- getIntervalMapping(scenario, interval)
  
  dt.dispatch <- fread(paste0(path.results, singleScenarioDeprecatedSplitter(scenario), "_ED_Ara.csv"))
  setnames(dt.dispatch, tolower(names(dt.dispatch)))
  
  dt.ed <- merge(dt.dispatch, interval.map, by = "int")
  
  dt.return <- dt.ed[, list(ara, int, stop, date, wkdy, he, 
                            load, loss, p, sp, balanceprice, 
                            loadprice, loadcost, sourceprice, sourcerevenue, neticcost, neticrevenue,
                            violation, penalty)]
  return(dt.return)
}
# Note that "scenario" can be a vector of two scenario names, or a list of two getAreaDispatch() results
getAreaDispatchDiff <- function(scenario, interval = NULL) {
  #### Check inputs
  stopifnot(length(scenario) == 2)
  
  # If scenario input is a list of data.tables, great
  # Otherwise, need to get the base info to comp
  if(is.list(scenario) & all(is.data.table(scenario[[1]]), is.data.table(scenario[[2]]))) {
    lst.scn <- scenario
  } else {
    lst.scn <- lapply(X = scenario, FUN = function(this.scn) {
      # Redefine relevant paths
      path.results <-   paste0(PATH.IO, this.scn, "/results/")
      
      interval.map <- getIntervalMapping(this.scn, interval)
      
      dt.ed <- getAreaDispatch(this.scn, interval.map)
      
      return(dt.ed)
    })
  } 
  
  # And difference them
  # vec.id.vars <- c("ara", "int", "stop", "date", "wkdy", "he")
  vec.id.vars <- c("ara", "stop", "date", "wkdy", "he")
  dt.ed <- merge(melt(lst.scn[[1]], id.vars = vec.id.vars), 
                 melt(lst.scn[[2]], id.vars = vec.id.vars),
                 by = c(vec.id.vars, "variable"))
  dt.ed[, value := value.y - value.x]
  dt.ed[, value_p := value.y / value.x]
  
  # dt.ed.return <- dcast.data.table(dt.ed, ara + int + stop + date + wkdy + he ~ variable, value.var = "value")
  dt.ed.return <- dcast.data.table(dt.ed, ara + stop + date + wkdy + he ~ variable, value.var = "value")
  dt.ed[, variable := paste0(variable, "_p")]
  # dt.ed_p.return <- dcast.data.table(dt.ed, ara + int ~ variable, value.var = "value_p")
  # dt.ed <- merge(dt.ed.return, dt.ed_p.return, by = c("ara", "int"))
  dt.ed_p.return <- dcast.data.table(dt.ed, ara + stop ~ variable, value.var = "value_p")
  dt.ed <- merge(dt.ed.return, dt.ed_p.return, by = c("ara", "stop"))
  
  dt.ed <- dt.ed[order(stop, ara)][, int := 1:.N, by = ara]
  
  return(dt.ed)
}


# Miscellaneous functions -----

interpolateNodeCoordsFromEdgeMap <- function(dt.nodes, dt.edges, bln.remove.unconnected.nodes = F) {
  #### Check inputs
  stopifnot(is.data.table(dt.nodes)); stopifnot(is.data.table(dt.edges))
  stopifnot(all(c("node", "x", "y") %in% names(dt.nodes))); stopifnot(all(c("from", "to") %in% names(dt.edges)))
  
  ### Function defs
  # Core interpolation functions that will be looped
  actuallyInterpolate <- function(this.node) {
    # Find connected nodes
    vec.connections <- dt.connect[node == this.node, connect]
    
    # Handle case where there are no connected nodes
    if(length(vec.connections) == 0) {
      if(bln.remove.unconnected.nodes) {
        return(data.table())
      } else {
        return(dt.return[, list(node = this.node, x = NA, y = NA)])
      }
    }
    
    # Handle case where all conencted nodes are NA
    if(nrow(dt.nodes[!is.na(x) & node %in% vec.connections]) == 0) {
      # (no handling, will let external looping handle this)
      return(dt.nodes[, list(node = this.node, x = NA, y = NA)])
      
    # Main case
    } else {
      # Average coords of connected, known nodes
      dt.return <- dt.nodes[!is.na(x) & node %in% vec.connections, list(x = mean(x), y = mean(y))]
      return(dt.return[, list(node = this.node, x, y)])
    }
  }
  
  ### Process input data
  # Making the network connections exhaustive; can look for all connections from the "node" column
  dt.connect <- unique(rbind(dt.edges, dt.edges[, list(from = to, to = from)]))
  setnames(dt.connect, c("node", "connect"))
  setkey(dt.connect, "node")
  
  #### Perform interpolation
  # Loop through interpolation function until number of NAs is stable
  num.compare <- 0
  num.na <- nrow(dt.nodes[is.na(x)])
  while(num.na != num.compare & num.na != 0) {
    num.compare <- num.na
    # Which nodes to calc coords for
    vec.to.interpolate <- dt.nodes[is.na(x), node]
    # Interpolate
    dt.new <- rbindlist(lapply(X = vec.to.interpolate, FUN = actuallyInterpolate))
    dt.nodes <- rbind(dt.nodes[!is.na(x)], dt.new)
    # Test
    num.na <- nrow(dt.nodes[is.na(x)])
  }
  
  #### Return same format data.table as input, just with more blanks filled in
  return(dt.nodes)
}


# Wrapper functions -----

scenarioYearStitch.getInjectorInfo <- function(vector.scenarios) {

  dt.inj.ts <- data.table()
  dt.inj.ti <- data.table()
  
  # Bind together all results for this sequence
  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    
    lst.inj <- getInjectorInfo(this.scn, interval.map = NULL, bln.ts = T, bln.fix.missing.nodes = F)
    dt.inj.ts <- rbind(dt.inj.ts, lst.inj[[1]]) # Only save TS data (big) for charger nodes
    dt.inj.ti <- rbind(dt.inj.ti, lst.inj[[2]])[, .SD[`max` == max(`max`)][1], by = inj]
  }
  
  # Need to reindex "int" to make continuous
  if(length(vector.scenarios) > 1) if(nrow(dt.inj.ts)) dt.inj.ts <- dt.inj.ts[order(stop, inj)][, int := 1:.N, by = list(inj)]

  return(list(dt.inj.ts, dt.inj.ti))
}
scenarioYearStitch.getAreaDispatch <- function(vector.scenarios) {
  dt.ed <- data.table()

  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    dt.ed <- rbind(dt.ed, getAreaDispatch(this.scn, interval.map = NULL))
  }
  
  if(length(vector.scenarios) > 1) dt.ed <- dt.ed[order(stop, ara)][, int := 1:.N, by = list(ara)]
  
  return(dt.ed)
}
scenarioYearStitch.getOptResults <- function(vector.scenarios) {
  dt.obj <- data.table()
  
  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    dt.obj <- rbind(dt.obj, getOptResults(this.scn, summed = F))
  }
  
  if(length(vector.scenarios) > 1) dt.obj <- dt.obj[order(stop_start)][, int := 1:.N]
  
  return(dt.obj)
}
scenarioYearStitch.getOptResultsAccurate <- function(vector.scenarios, summed = F) {
  dt.obj <- data.table()
  
  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    dt.obj <- rbind(dt.obj, getOptResultsAccurate(this.scn, summed))
  }
  
  if(length(vector.scenarios) > 1) dt.obj <- dt.obj[order(stop_start)][, int := 1:.N]
  
  return(dt.obj)
}
scenarioYearStitch.getPC_Nd <- function(vector.scenarios) {
  dt.res <- data.table()
  
  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    res <- fread(paste0(PATH.IO, 
                        this.scn, "/results/", 
                        singleScenarioDeprecatedSplitter(this.scn), "_PC_Nd.csv"))
    res <- merge(res, getIntervalMapping(this.scn)[, list(int, stop)], by = c("int"))
    
    dt.res <- rbind(dt.res, res)
  }
  
  if(length(vector.scenarios) > 1) dt.res <- dt.res[order(stop)][, int := 1:.N, by = nd]
  
  return(dt.res)
}
scenarioYearStitch.getInjectorEmissions <- function(vector.scenarios, return.agg = T) {
  
  dt.return <- data.table()
  
  # Bind together all results for this sequence
  for(this.scn in vector.scenarios) {
    # catn(this.scn)
    
    dt.emissions <- getInjectorEmissions(this.scn, return.agg)
    dt.return <- rbind(dt.return, dt.emissions)
  }
  
  # Need to reindex "int" to make continuous
  if(length(vector.scenarios) > 1 & nrow(dt.return)) {
    if("inj" %in% names(dt.return)) {
      dt.return <- dt.return[order(stop, inj)][, int := 1:.N, by = list(inj)]
    } else {
      dt.return <- dt.return[order(stop)][, int := 1:.N]
    }
  }
  
  return(dt.return)
}


##### LIBRARY: LOAD INPUTS PROCESSING -----


# Load w/o EVs -----

# Return hourly ERCOT load by weather zone over entire input year
getErcotHistoricalLoad <- function(hist.year) {
  # hist.year <- 2013
  
  # Construct the file with historical load data
  hist.file <- paste0(PATH.INPUTS, "Historical ERCOT Load/ERCOT-load-", hist.year, ".csv")
  if(!file.exists(hist.file)) stop("No handling for chosen historical year.")
  
  # Read in data
  dt.hist <- fread(hist.file)
  
  # Format times
  dt.hist[, V1 := gsub(":", "", V1)]
  dt.hist[, datetime := as.POSIXct(V1, format = "%Y-%m-%d %H%M%S", tz = "Etc/GMT+6")]
  dt.hist[, V1 := NULL]
  
  # Zone names
  dt.hist[, ERCOT := NULL]
  setnames(dt.hist, 
           c("FWEST", "NCENT", "SOUTH", "SCENT"),
           c("FAR_WEST", "NORTH_CENTRAL", "SOUTHERN", "SOUTH_CENTRAL"))
  setnames(dt.hist, names(dt.hist)[names(dt.hist) != "datetime"], paste0("WZ_", names(dt.hist)[names(dt.hist) != "datetime"]))
  
  # Melt
  dt.hist <- melt(dt.hist, id.vars = c("datetime"), variable.name = "zone")
  
  return(dt.hist)
}

# Return hourly ERCOT load by weather zone over entire input year. Scales historical data using official ERCOT forecasts
getErcotFutureLoadExEv <- function(future.year, hist.year = 2017, ret.plt.expl = F) {
  # future.year <- 2032
  
  ### Get historical load for given year. The default, 2017, is latest that I have data for
  dt.hist <- getErcotHistoricalLoad(hist.year)
  dt.hist[, month := month(datetime)]
  
  ### Get yearly growth model
  # http://www.ercot.com/gridinfo/load/forecast
  # http://www.ercot.com/content/wcm/lists/166967/ERCOT_Monthly_Peak_Demand_and_Energy_Forecast_2019_2028.xlsx
  dt.forecasts <- fread(paste0(PATH.INPUTS, "ERCOT_Monthly_Peak_Demand_and_Energy_Forecast_2019_2028.csv"))
  setnames(dt.forecasts, c("year", "month", "peak", "energy"))
  dt.forecasts[, year2 := year^2] # quadratic term for model fitting
  
  ### Predict monthly peak/energy for given year from quadratic regression model
  pred.peak <- sapply(X = 1:12, FUN = function(this.month) {
    this.lm <- lm(peak ~ year + year2, data = dt.forecasts[month == this.month])
    predict(this.lm, data.table(year = future.year, year2 = future.year^2))
  })
  pred.energy <- sapply(X = 1:12, FUN = function(this.month) {
    this.lm <- lm(energy ~ year + year2, data = dt.forecasts[month == this.month])
    predict(this.lm, data.table(year = future.year, year2 = future.year^2))
  })
  dt.pred.monthly <- data.table(month = 1:12, peak = pred.peak, energy = pred.energy)
  # ggplot(rbind(copy(dt.pred)[, year := future.year], dt.forecasts[, list(year, month, peak, energy)])) + geom_point(mapping = aes(x = year, y = peak, color = factor(month)))
  
  ### Apply model to extrapolate hourly load
  # Find scaling factors to convert historical data to predicted data
  # For energy scaling, we scale from a 0 baseline, so we're just changing mean, not variance, of distribution
  # For peak scaling, we scale from the mean(value) baseline, so we're just changing variance, not mean, of distribution
  dt.hist.monthly <- dt.hist[, list(value = sum(value)), by = datetime][, list(energy = sum(value)), by = list(month(datetime))]
  dt.scalers <- merge(dt.hist.monthly, dt.pred.monthly, by = "month", suffixes = c("_hist", "_pred"))
  dt.scalers <- dt.scalers[, list(month, energy_scaler = (energy_pred - 0)/(energy_hist - 0))]
  dt.hist <- merge(dt.hist, dt.scalers, by = "month")
  
  plt.expl <- ggplot() + geom_hline(yintercept = 0) + theme_bw() +
    geom_line(data = dt.hist[month(datetime) == 11, list(value = sum(value)/1e3), by = list(datetime)], 
              mapping = aes(x = datetime, y = value, color = "2017 Historical")) +
    geom_line(data = dt.hist[month(datetime) == 11, list(value = sum(value*energy_scaler)/1e3), by = list(datetime)], 
              mapping = aes(x = datetime, y = value, color = "First Rescaling"))
  dt.hist[, value := value*energy_scaler]
  
  dt.hist.monthly <- dt.hist[, list(value = sum(value)), by = datetime][, list(peak = max(value), avg = mean(value)), by = list(month(datetime))]
  dt.scalers <- merge(dt.hist.monthly, dt.pred.monthly, by = "month", suffixes = c("_hist", "_pred"))
  dt.scalers <- dt.scalers[, list(month, peak_scaler = (peak_pred - avg)/(peak_hist - avg))]
  dt.hist <- merge(dt.hist, dt.scalers, by = "month")
  dt.hist[, avg := mean(value), by = list(month, zone)]
  
  plt.expl <- plt.expl + 
    geom_line(data = dt.hist[month(datetime) == 11, list(value = sum((value - avg)*peak_scaler + avg)/1e3), by = list(datetime)], 
              mapping = aes(x = datetime, y = value, color = "Second Rescaling")) +
    labs(x = "", y = "Hourly System Load (GW)") + theme_bw(12) +
    scale_color_manual("", breaks = c("2017 Historical", "First Rescaling", "Second Rescaling"), values = c("black", "gray", "blue"))
  # plot(plt.expl) + theme_bw(12) + labs(x = "", y = "ERCOT Load (GW)")
  dt.hist[, value := (value - avg)*peak_scaler + avg]
  
  dt.hist[, c("energy_scaler", "peak_scaler", "avg", "month") := NULL]
  
  # Fix year
  dt.hist[, datetime := as.POSIXct(paste0(future.year, "-", format(datetime, "%m-%d %T", tz = "ETC/GMT+6")), tz = "ETC/GMT+6")]
  
  ### Return
  if(ret.plt.expl) return(plt.expl)
  return(dt.hist[])
}


# EV loading -----

# Number of EV cars in ERCOT in a given year
getErcotFutureEvStock <- function(future.year) {
  
  # Rough exponential model fitted (by eye) to ERCOT's LTSA report for "Cars"
  # 0 in 2019, 2000000 in 2031, 3000000 in 2033
  
  return( (3000000/1.25^(2033-2019))*1.25^(future.year-2019) )
  
}

# Regresses # EVs against # Charger ports to return estimated number of ports for # of EVs
getChargerFromOfEvs <- function(num.evs, return.plt = F) {
  
  dt.data <- fread(paste0(PATH.INPUTS, "ev_chrg_ratio.csv"))
  dt.data <- rbind(dt.data[, list(juris, year, out = out_stock, ev = ev_stock)],
                   dt.data[, list(juris, year, out = out_stock, ev = ev_stock_2)],
                   dt.data[, list(juris, year, out = out_flow, ev = ev_flow)],
                   dt.data[, list(juris, year, out = out_flow, ev = ev_flow_2)])
  dt.data <- dt.data[!is.na(out) & !is.na(ev)]
  
  if(return.plt) {
    
    plt <- ggplot(dt.data) + theme_bw(12) + geom_hline(yintercept = 0) + geom_vline(xintercept = 0) +
      geom_point(mapping = aes(x = ev/1e3, y = out/1e3, color = juris)) +
      geom_smooth(mapping = aes(x = ev/1e3, y = out/1e3), method = "lm", formula = y~x) +
      labs(x = "Thousands of EVs", y = "Thousands of EVSE") +
      scale_color_discrete("Jurisdiction")
    
    return(plt)
  }
  mod <- lm(data = dt.data, formula = out ~ ev)
  
  return(as.vector(predict(object = mod, newdata = data.table(ev = num.evs))))
}

# Return ratio of fast chargers to total chargers, empircal
getFastChargerRatio <- function(return.plt = F) {
  
  if(return.plt) {
    dt.data <- fread(paste0(PATH.INPUTS, "ev_chrg_ratio.csv"))
    dt.data <- rbind(dt.data[, list(juris, year, out = out_stock, fast = fast_stock)],
                     dt.data[, list(juris, year, out = out_flow, fast = fast_flow)])
    dt.data <- dt.data[!is.na(out) & !is.na(fast)][, ratio := fast/out]
    # ggplot(dt.data[, list(juris, ratio)][order(ratio)][, index := 1:.N]) + geom_point(mapping = aes(x = index, y = ratio, color = juris)) # China a bit extreme
    # ggplot(dt.data[juris == "USA", list(ratio, year)]) + theme_bw(12) + geom_hline(yintercept = 0) +
    #   geom_point(mapping = aes(x = year, y = ratio)) +
    #   labs(x = "", y = "Ratio of fast-chargers to slow-chargers")
    plt.expl <- ggplot(dt.data[, list(juris, year, ratio)][order(ratio)][, index := 1:.N]) + 
      theme_bw(12) + geom_hline(yintercept = 0) +
      geom_histogram(mapping = aes(x = ratio), bins = 5, color = "black", fill = "grey") +
      labs(x = "EVSE, Ratio of Fast/Slow", y = "Sample Count")
    return(plt.expl)
  }
  
  # dt.data[juris == "USA", mean(ratio)]
  # dt.data[juris != "China", mean(ratio)]
  
  # Using 10% since a US context, where ratio has been increasing steadily to around 10% now in 2019
  # And because the world average ex-China is also right now 10%
  
  return(.1)
  
}

# Strings above functions to return total number of fast charger outlets in ERCOT in given year
getErcotFutureFastChargerStock <- function(future.year) {
  num.evs <- getErcotFutureEvStock(future.year)
  num.outs <- getChargerFromOfEvs(num.evs)
  num.fast <- getFastChargerRatio()*num.outs
  
  return(num.fast)
}
getErcotFutureSlowChargerStock <- function(future.year) {
  num.evs <- getErcotFutureEvStock(future.year)
  num.outs <- getChargerFromOfEvs(num.evs)
  num.slow <- (1-getFastChargerRatio())*num.outs
  
  return(num.slow)
}

# Returns normal distribution parameters for each 15 minute interval during weekday and weekend days
getChargerUseProfile <- function(return.plt = F, show.segments = c("All EVSE", "DC Fast", "Residential")) {
  
  dt.data <- fread(paste0(PATH.INPUTS, "EVProject Infrastructure ReportJan13Dec13_HA.csv"))
  setnames(dt.data, c("area", "time", "value", "segment", "pct", "category", "data"))
  dt.sub <- dt.data[data %in% c("RangeAggElectricDemandVsTimeofDayPeakWeekDay", "RangeAggElectricDemandVsTimeofDayPeakWeekEnd") &
                      area == "ALL", 
                    list(time, value, category, segment, peak = ifelse(grepl("End", data), "2x24", "5x24"))]
  dt.sub[, val_norm := value/max(value), by = list(segment, peak)]
  dt.sub[, time := as.integer(tstrsplit(time, ":")[[1]]) + as.integer(tstrsplit(time, ":")[[2]])/60]
  
  ### NOTE THE WILDLY DIFFERENT PROFILES!
  # all
  # Away = Publicly Accessible Level 2 EVSE
  # DCFast = DC Fast Chargesr
  # Home = Residential Level 2 EVSE
  # P = Private Nonresidential Level 2 EVSE
  if(return.plt) {
    dt.plot <- copy(dt.sub)[, quantile := gsub("perc_", "", category)]
    dt.plot[quantile == "0", quantile := "1"]
    dt.plot[quantile == "100", quantile := "99"]
    dt.plot[, peak := ifelse(peak == "5x24", "Weekday", "Weekend")]
    dt.plot[, segment := ifelse(segment == "all", "All EVSE", 
                                ifelse(segment == "DCFast", "DC Fast",
                                       ifelse(segment == "Home", "Residential", segment)))]
    dt.plot <- dt.plot[segment %in% show.segments]
    
    dt.plot[, segment := factor(x = segment, levels = c("All EVSE", "Residential", "DC Fast",
                                                        "Away", "P"))]
    
    plt1 <- ggplot() + theme_bw(12) + geom_hline(yintercept = 0) +
      geom_ribbon(data = dcast(dt.plot[category %in% c("perc_0", "perc_100")], formula = time+segment+peak~category, value.var = "val_norm"),
                  mapping = aes(x = time, ymin = perc_0, ymax = perc_100),
                  fill = "lightgray") +
      geom_line(data = dt.plot, mapping = aes(x = time, y = val_norm, color = quantile)) +
      facet_grid(peak~segment, ) + 
      scale_color_brewer("Quantile", type = "qual", palette = 6) +
      # labs(x = "Hour of Day", y = "EVSE Utilization Factor") +
      theme(strip.background = element_rect(fill = "white")) +
      scale_x_continuous(name = NULL,
                         breaks = c(0, 6, 12, 18, 24),
                         labels = c("12 AM", "6 AM", "12 PM", "6 PM", "")) +
      labs(y = NULL, title = "HFC Utilization Factor")
  }
  
  # Fit a distribution to each time interval: "quantile-matching estimation"
  # Can't find a library that will let me do this directly (fitdistrplus qme is assumes I have the underlying data)
  # So implement a simple version from https://www.johndcook.com/quantiles_parameters.pdf
  
  # Using a normal distribution fit on the 25th and 75th percentiles actually performs very well.
  # Only issue is the sub zero values, but those can easily be floored post-gen
  ### Illustrative example
  if(return.plt) {
    dt.profiles <- dt.sub[segment == "DCFast" & time == 16 & peak == "5x24"]
    x_1 <- dt.profiles[category == "perc_25", val_norm]
    x_2 <- dt.profiles[category == "perc_75", val_norm]
    sigma <- (x_2-x_1)/(qnorm(.75) - qnorm(.25))
    mu <- (x_1*qnorm(.75) - x_2*qnorm(.25))/(qnorm(.75) - qnorm(.25))
    
    plt2 <- ggplot() +
      geom_point(data = data.table(index = (1:1000)/1000, val = sort(rnorm(n = 1000, mean = mu, sd = sigma))),
                 mapping = aes(x = index, y = val)) +
      geom_point(data = dt.profiles[, list(index = c(0, .25, .5, .75, 1), val = val_norm)],
                 mapping = aes(x = index, y = val), color = "red")
  }
  
  # Prepare quantile data
  chr.segment = c("DCFast", "all")[1]
  dt.profiles <- dt.sub[segment == chr.segment & category %in% c("perc_25", "perc_75"), 
                        list(time, peak, q = as.numeric(gsub("perc_", "", category))/100, val_norm)]
  
  # Run through algorithm
  dt.feed <- dcast(dt.profiles, time + peak ~ q, value.var = "val_norm")
  setnames(dt.feed, c("0.25", "0.75"), c("x_1", "x_2"))
  dt.fits <- rbindlist(mapply(FUN = function(x_1, x_2, p_1, p_2) {
    sigma <- (x_2-x_1)/(qnorm(p_2) - qnorm(p_1))
    mu <- (x_1*qnorm(p_2) - x_2*qnorm(p_1))/(qnorm(p_2) - qnorm(p_1))
    return(data.table(sigma, mu))
  }, x_1 = dt.feed$x_1, x_2 = dt.feed$x_2, p_1 = .25, p_2 = .75, SIMPLIFY = F))
  
  dt.dists <- cbind(dt.feed, dt.fits)[, list(time, peak, sigma, mu)]
  
  if(return.plt) {
    # ## Check how the (floored) distributions look by comparing against original data:
    set.seed(123)
    no.samples <- 1
    vec.res <- mapply(function(sigma, mu) rnorm(no.samples, mean = mu, sd = sigma), sigma = dt.dists$sigma, mu = dt.dists$mu, SIMPLIFY = T)
    dt.data <- data.table(time = rep(dt.dists$time, each = no.samples), 
                          peak = rep(dt.dists$peak, each = no.samples), 
                          val = pmax(0, as.vector(vec.res)),
                          segment = "DC Fast")
    dt.data[, peak := ifelse(peak == "5x24", "Weekday", "Weekend")]
    
    plt3.1 <- ggplot(dt.data[peak == "Weekday"]) + 
      theme_bw(12) + geom_hline(yintercept = 0) +
      geom_point(mapping = aes(x = time, y = val)) + 
      geom_line(mapping = aes(x = time, y = val)) +
      facet_grid(peak~segment, ) + 
      theme(strip.background = element_rect(fill = "white")) +
      scale_x_continuous(name = NULL,
                         breaks = c(0, 6, 12, 18, 24),
                         labels = c("12 AM", "6 AM", "12 PM", "6 PM", "")) +
      labs(y = NULL, title = "Sampled utilization factor of one HFC") +
      lims(y = c(0,1))
    
    no.samples <- 500
    vec.res <- mapply(function(sigma, mu) rnorm(no.samples, mean = mu, sd = sigma), sigma = dt.dists$sigma, mu = dt.dists$mu, SIMPLIFY = T)
    dt.data <- data.table(time = rep(dt.dists$time, each = no.samples), 
                          peak = rep(dt.dists$peak, each = no.samples), 
                          val = pmax(0, as.vector(vec.res)),
                          segment = "DC Fast")
    dt.data[, peak := ifelse(peak == "5x24", "Weekday", "Weekend")]
    
    plt3.2 <- ggplot(dt.data[peak == "Weekday", list(val = sum(val)*getTeslaSuperchargerAtts()$power), 
                             by = list(peak, time, segment)]) + 
      theme_bw(12) + geom_hline(yintercept = 0) +
      geom_point(mapping = aes(x = time, y = val)) +
      geom_line(mapping = aes(x = time, y = val)) +
      facet_grid(peak~segment, ) + 
      theme(strip.background = element_rect(fill = "white")) +
      scale_x_continuous(name = NULL,
                         breaks = c(0, 6, 12, 18, 24),
                         labels = c("12 AM", "6 AM", "12 PM", "6 PM", "")) +
      labs(y = NULL, title = "Aggregate load (MW) of a 500-FC station")
    
    plt3 <- gridExtra::arrangeGrob(plt3.1, plt3.2, layout_matrix = rbind(c(1, 2),
                                                                         c(1, 2)))
    
    return(list(plt1, plt2, plt3, plt3.1, plt3.2))
  }
  
  return(dt.dists)
  
}

# Return hourly ERCOT load by weather zone INCLUDING slow EV charging demand
# Use pre-existing zonal ratios-of-total-load to distribute ERCOT-wide charging demand
# Use "mean" "all" charger behavior to shape the EV charging demand, then add on to load
# Assume L2 charging with (mean) 20kW max, according to ERCOT LTSA
getErcotFutureLoadInclEv <- function(future.year, hist.year = 2017, chg.cap = .02, bln.include.inc = T) {
  
  # Pull data
  dt.ex <- getErcotFutureLoadExEv(future.year, hist.year)
  num.slow <- getErcotFutureSlowChargerStock(future.year)
  dt.profiles <- getChargerUseProfile(show.segments = "All EVSE")
  
  # Determine zonal distribution of charger ICAP
  # Base determination on max weekend loads since that probably correlates best with residential AC usage
  # And these slow chargers will probably be best distributed according to residential housing patterns
  dt.ex[, wd := wday(datetime)]
  dt.dist <- dt.ex[wd %in% c(1, 7), list(max(value)), by = zone]
  dt.dist <- dt.dist[, list(zone, ratio = V1/sum(V1))]  
  
  # Get new hourly load
  dt.new <- dt.ex[, list(datetime, value, zone, peak = ifelse(wd %in% c(1, 7), "2x24", "5x24"), 
                         time = as.numeric(format(datetime, "%H")) + as.numeric(format(datetime, "%M"))/60)]
  dt.new <- merge(dt.new, dt.profiles, by = c("peak", "time"), all.x = T)
  dt.new <- merge(dt.new, dt.dist, by = c("zone"), all.x = T)
  dt.new <- dt.new[, list(datetime, zone, value, inc =  num.slow*(chg.cap/max(mu))*mu*ratio)]
  
  if(F) {
    # Slow charger stats
    summary(dt.new[, list(value = sum(value), inc = sum(inc)), by = datetime][, inc/value])
    summary(dt.new[, list(value = sum(value), inc = sum(inc)), by = datetime][, inc])
    ggplot(dt.new) + geom_boxplot(mapping = aes(x = hour(datetime), y = inc/value*100, group = hour(datetime))) + labs(y = "% increase in load")
    ggplot(dt.new[month(datetime) == 2]) + geom_line(mapping = aes(x = datetime, y = inc, color = zone))
    ggplot(dt.new[month(datetime) == 2]) + geom_line(mapping = aes(x = datetime, y = value + inc, color = zone))
    ggplot(dt.new[month(datetime) == 2]) +
      geom_ribbon(mapping = aes(x = datetime, ymin = value, ymax = value + inc, fill = zone), color = "black")
    
    # Fast charger stats
    dt.fast <- scenarioYearStitch.getInjectorInfo(c("Scn_000980", "Scn_000981"))[[1]]
    summary(dt.fast[grepl("chg", inj), sum(p), by = stop]$V1)
    # And see "process_get_charger_network.R" for nameplate stats
  }
  
  dt.new <- dt.new[order(datetime, zone)]
  
  if(bln.include.inc) {
    return(dt.new[, list(datetime, zone, value = value + inc, inc)])
  } else {
    return(dt.new[, list(datetime, zone, value = value + inc)])
  }
}


# Charger network -----

getTeslaPowerpackAtts <- function() {
  return(list(power = .050,
              energy = .200,
              efficiency = .89,
              vom = 1))
}
getTeslaSuperchargerAtts <- function() {
  return(list(power = .150))
}

# Pull charger locations and attributes from source files
# Cap.mod alters the number of stalls at a station. (The technology is fixed at getTeslaSuperchargerAtts())
getChargingNetwork <- function(chr.source = "tesla", chr.state = "TX", cap.mod = 1, future.year = NULL) {
  if(chr.source == "tesla" & chr.state == "TX") {
    stall_model <- getTeslaSuperchargerAtts()
    stall_power <- stall_model$power
    
    # Get the basic charger topology model
    dt.chargers <- as.data.table(XML::readHTMLTable(doc = paste0(PATH.INPUTS, "supercharger_net_table.html"),
                                                    stringsAsFactors = F))
    setnames(dt.chargers, c("id", "address", "city", "state", "zip", "country", "stalls", "coords", "elev", "status", "open", "links"))
    dt.chargers[, c("lat", "long") := list(as.numeric(tstrsplit(coords, ", ")[[1]]), as.numeric(tstrsplit(coords, ", ")[[2]]))]
    dt.chargers[, stalls := as.integer(stalls)]
    dt.chargers <- dt.chargers[state == "TX", list(lat, long, stalls, status)]
    
    if(!is.null(future.year)) {
      # Get total number of chargers in state for that year
      num.fast <- getErcotFutureFastChargerStock(future.year)
      
      # Apportion according to existing infrastructure arrangement
      dt.chargers[, ratio := stalls/sum(stalls)]
      dt.chargers[, stalls := round(stalls + ratio*num.fast)]
    }
    
    # Now combine with the charger model and params to get an electrical model
    dt.chargers[, stalls := stalls*cap.mod]
    dt.chargers[, mw := stall_power*stalls]
    
  } else {
    stop("Unsupported source/state:", chr.source)
  }
  
  return(dt.chargers[, list(lat, long, mw, stalls, status)])
}

if(F) {
  # calculate total energy demand per year from slow chargers and from fast chargers
  dt.slow <- getErcotFutureLoadInclEv(future.year = 2033)
  cat("Slow: ", sum(dt.slow$inc)/1e6, " TWh")
  dt.fast <- scenarioYearStitch.getInjectorInfo(c("Scn_000980", "Scn_000981"))
  cat("Fast: ", dt.fast[[1]][grepl("chg", inj), sum(p)]/1e6, " TWh")
}