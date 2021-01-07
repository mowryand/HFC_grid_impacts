# Generates hourly load and gen schedules and creates the _SCH_TEMP.csv file

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### PRE-PROCESSING #####

dt.load <- fread(paste0(path.write, scenario, "_SCN_ARA_LOD.csv"))
dt.mdl_id <- fread(paste0(path.write, scenario, "_mdl_id.csv"))
dt.cyc_prd <- fread(paste0(path.write, scenario, "_cyc_prd_id.csv"))
horizon <- tail(dt.cyc_prd[, cumsum(Length)], 1)-24 # hours after last day in schedule that we need to look ahead.

# Fix MDL_ID table
time.interval <- c(as.POSIXct(start.date, format = "%Y-%m-%d", tz = "ETC/GMT+6"), as.POSIXct(end.date, format = "%Y-%m-%d", tz = "ETC/GMT+6")+24*60*60+horizon*60*60)
dt.mdl_id$StartDate <- format(as.POSIXct(start.date, format = "%Y-%m-%d", tz = "ETC/GMT+6"), "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")
dt.mdl_id$StopDate <- format(as.POSIXct(end.date, format = "%Y-%m-%d", tz = "ETC/GMT+6"), "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")
dt.mdl_id$MinDate <- format(time.interval[1], "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")
dt.mdl_id$MaxDate <- format(time.interval[2], "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")

#  Write
tbl.name <- "_MDL_ID"
catn(tbl.name)

dt.write <- dt.mdl_id
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

##### PROCESSING #####
##########
catn("Wind and Solar")
##########

##### INPUTS #####
dt.wind <- fread(paste0(PATH.INPUTS, "Historical ERCOT Renewables/ERCOT-wind-icomesh-9subdiv-WINDTKdefault-",
                        hist.renewables.year, "-0.csv"))
dt.solar <- fread(paste0(PATH.INPUTS, "Historical ERCOT Renewables/ERCOT-pv-icomesh-9subdiv-track-0tilt-",
                         hist.renewables.year, "-0.csv"))

##### PRE-PROCESSING #####
# Get the capacity factor schedule
dt.gen <- rbind(cbind(type = "Wind", dt.wind),
                cbind(type = "Solar", dt.solar),
                use.names = T)
setnames(dt.gen, "V1", "datetime")
dt.gen[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H:%M", tz = "ETC/GMT+6")]
dt.gen <- dt.gen[minute(datetime) == 0]

# subset schedules to the model run timeframe
dt.gen[, datetime := as.POSIXct(paste0(substr(start.date, 0, 4), "-", format(datetime, "%m-%d %T", tz = "ETC/GMT+6")), tz = "ETC/GMT+6")] # fix the year
dt.gen <- dt.gen[datetime >= time.interval[1] & datetime <= time.interval[2]] # subset dates
dt.gen[, datetime := format(datetime, "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")] # format dates

# Format zones
setnames(dt.gen, 
         c("FWEST", "NCENT", "SOUTH", "SCENT", "ERCOT"),
         c("FAR_WEST", "NORTH_CENTRAL", "SOUTHERN", "SOUTH_CENTRAL", "0"))
dt.gen[, `0` := NULL]
dt.long.cfs <- melt(dt.gen, id.vars = c("datetime", "type"), variable.name = "zone", value.name = "cf")

##### PROCESSING #####

# Get model gens, types, zones, and capacities
dt.gens <- fread(paste0(PATH.INPUTS, "snl_psse_gen_map.csv"))
dt.gens <- dt.gens[type %in% c("Wind", "Solar"), list(i = psse_node, type)]

dt.caps <- fread(paste0(path.write, scenario, "_INJ_ID.csv"))
dt.caps <- dt.caps[, list(i = `//Injector`, cap = MaxMw)]
dt.gens <- merge(dt.gens, dt.caps, by = "i", all.x = T)
if(nrow(dt.gens[is.na(cap)]) > 0) stop("Missing map (cap)!")

dt.map <- fread(paste0(path.write, scenario, "_INJ_NET.csv"))
dt.map <- dt.map[, list(node = Node, i = `//Injector`)]
dt.gens <- merge(dt.gens, dt.map, by = "i", all.x = T)
if(nrow(dt.gens[is.na(node)]) > 0) stop("Missing map (cap)!")

dt.areas <- fread(paste0(path.write, scenario, "_SUB_ID.csv"))
dt.areas <- dt.areas[, list(node = `//Substation`, zone = substr(Area, start = 4, stop = 999))]
dt.gens <- merge(dt.gens, dt.areas, by = "node", all.x = T)
if(nrow(dt.gens[is.na(zone)]) > 0) stop("Missingmap (zone)!")

# Bind on wind and solar generation from gen mapping file
dt.schedules <- merge(dt.long.cfs, dt.gens, by = c("type", "zone"), all.y = T, allow.cartesian = T)
dt.schedules <- dt.schedules[, list(datetime, i, mw = cap*cf)]

# Append to SCH_TMP
tbl.name <- "_SCH_TMP"
catn(tbl.name)
chr.names <- c("//Schedule", "Time", "Value", "Enforce")

dt.write <- dt.schedules[order(i, datetime)][, list(paste0("gen_", i), datetime, mw, 1)]
setnames(dt.write, chr.names)
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

if(F) {
  ### OLD METHOD OF FIXED WIND/SOLAR
  
  # And map the injectors to the schedules for PSO
  tbl.name <- "_SCN_INJ_DSP"
  catn(tbl.name)
  chr.names <- c("//Scenario", "Injector", "Dispatch", "Enforce", "ScaleFactor", "Schedule", "Sequence")
  
  dt.write <- dt.gens[, list("ScnDA", i, NA, 1, NA, paste0("gen_", i), NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
} else if(T) {
  ### NEW METHOD OF FIXED WIND/SOLAR
  
  # And map the injectors to the schedules for PSO
  tbl.name <- "_SCN_INJ_MAX"
  catn(tbl.name)
  chr.names <- c("//Scenario", "Injector", "MaxMw", "Enforce", "ScaleFactor", "Schedule", "Sequence")
  
  dt.write <- dt.gens[, list("ScnDA", i, NA, 1, NA, paste0("gen_", i), NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
}

##########
catn("Area Loads")
##########

# Get base load data
dt.in <- getErcotFutureLoadInclEv(future.load.year, hist.load.year, bln.include.inc = F)
# ggplot(dt.in) + geom_area(mapping = aes(x = datetime, y = value, fill = zone))

dt.in[, datetime := as.POSIXct(paste0(substr(start.date, 0, 4), "-", format(datetime, "%m-%d %T", tz = "ETC/GMT+6")), tz = "ETC/GMT+6")] # fix the year
dt.in <- dt.in[datetime >= time.interval[1] & datetime <= time.interval[2]] # subset dates
dt.in[, datetime := format(datetime, "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")] # format dates
dt.in[, zone := paste0(zone, "_DA")] # format zones

dt.in <- dt.in[order(zone, datetime)]

# Add "dispersed" fast ev charging in special case
if(switch.load.treatment == "dispersed") {
  
  ##### GET AGGREGATE CHARGING PROFILES #####
  dt.profiles <- getChargerUseProfile(show.segments = "DC Fast")
  dt.profiles <- dt.profiles[time == floor(time)][, he := as.integer(time)]
  dt.profiles[he == 0, he := 24]
  
  dt.frame <- getHourTable(time.interval, chr.tz = "ETC/GMT+6")[]
  dt.frame <- dt.frame[, list(datetime, date, he, peak = ifelse(wday(date) %in% c(1,7), "2x24", "5x24"))]
  dt.frame <- merge(dt.frame, dt.profiles, by = c("peak", "he"), all.x = T)[order(datetime)]
  
  dt.frame[, datetime := format(datetime, format = "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")]
  
  # Attach actual charging nodes
  dt.chg.cap <- fread(paste0(path.write, scenario, "_INJ_ID.csv"))[, list(inj = `//Injector`, cap = MaxMw)]
  dt.schedules <- data.table(merge.data.frame(dt.frame, dt.chg.cap[grepl("_chg", inj)], by = NULL))
  dt.schedules[, stalls := cap/getTeslaSuperchargerAtts()$power]
  
  # Simulate the loading at each station by sampling individual chargers
  # (Making sure to floor at zero)
  vec.cf <- mapply(FUN = function(stalls, mu, sig) {
    sum(pmax(0, rnorm(n = stalls, mean = mu, sd = sig)))
  }, stalls = dt.schedules$stalls, mu = dt.schedules$mu, sig = dt.schedules$sigma, SIMPLIFY = T)
  dt.schedules[, cf := vec.cf/stalls]
  dt.schedules[, val := cap*ifelse(is.na(cf), 0, cf)]
  
  # Map injectors to zones
  dt.map <- getNodes(scenario)[, list(node, wz, area)]
  dt.fev <- dt.schedules[, list(val, node = as.integer(tstrsplit(inj, "_")[[1]]), datetime)]
  
  dt.fev <- merge(dt.fev, dt.map, by = "node", all.x = T)
  dt.fev <- dt.fev[, list(val = sum(val)), by = list(datetime, zone = paste0(wz, "_DA"))]
  # if(any(is.na(dt.fev$val))) stop("Some NAs")
    
  dt.in <- merge(dt.in, dt.fev, by = c("datetime", "zone"), all.x = T)
  # if(any(is.na(dt.in$val))) stop("Some NAs")
  dt.in <- dt.in[, list(datetime, zone, value = value + val)][order(zone, datetime)]
}

# Add rooftop solar if applicable
dt.alter <- melt(fread(gen.scenario.file), id.vars = c("area"), variable.name = "type")[type == "Dist Solar"]
if(nrow(dt.alter[type == "Dist Solar" & value != "na" & value != "0"])) {
  dt.alter[value != "na" & value != "0"]
  
  # Loop through each of the explicitly defined areas to alter that area's generation
  vec.areas <- dt.alter[area != "other" & value != "na", area]
  for(this.area in vec.areas) {
    incremental.capacity <- dt.alter[area == this.area, as.numeric(value)]
    if(nrow(dt.inj_id[Area == this.area]) == 0) warning("NO EXISTING GENERATION")
    stop("TODO")
  }
  
  # Now handle the "other" quantity, which is to be split across explicitly unhandled areas
  incremental.capacity <- dt.alter[area == "other", as.numeric(value)]
  dt.in[, inc_cap := incremental.capacity]
  
  vec.areas <- paste0(setdiff(dt.alter[area != "other", unique(area)], vec.areas), "_DA")
  dt.props <- dt.in[zone %in% vec.areas, list(load = sum(value)), by = list(zone)][, list(zone, prop = load/sum(load))]
  dt.in <- merge(dt.in, dt.props, by = "zone", all.x = T)
  dt.in[is.na(prop), prop := 0]
  
  dt.cfs <- dt.long.cfs[type == "Solar", list(datetime, zone = paste0("WZ_", zone, "_DA"), cf)]
  dt.in <- merge(dt.in, dt.cfs, by = c("datetime", "zone"), all.x = T)
  if(any(is.na(dt.in$cf))) stop()
  
  dt.in <- dt.in[, list(datetime, zone, value = value - prop*inc_cap*cf)][order(zone, datetime)]
}

dt.in <- dt.in[order(zone, datetime)]

if(treat.load.as == "INJ") {
  # Apportion load to nodes
  dt.nodes <- merge(lst.pso[["_SUB_ID"]][, list(node = `//Substation`, zone = paste0(Area, "_DA"))], 
                    lst.pso[["_STE_NDE"]][, list(node = Enode, rating = LoadMw)],
                    by = c("node"), all.y = T)
  dt.nodes <- dt.nodes[, list(node, load_contrib = rating/sum(rating), rating), by = zone]
  
  dt.results <- merge(dt.nodes, dt.in, by = "zone", allow.cartesian = T)
  dt.results <- dt.results[, list(datetime, node, value = load_contrib*value, rating)]
  
  # Append to SCH_TMP
  tbl.name <- "_SCH_TMP"
  catn(tbl.name)
  chr.names <- c("//Schedule", "Time", "Value", "Enforce")
  
  dt.write <- dt.results[order(node, datetime)][, list(paste0("load_", node, "_LOD"), datetime, value, 1)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
  # And map the injectors to the schedules for PSO
  tbl.name <- "_SCN_INJ_DSP"
  catn(tbl.name)
  chr.names <- c("//Scenario", "Injector", "Dispatch", "Enforce", "ScaleFactor", "Schedule", "Sequence")
  
  dt.write <- dt.results[, list(node = unique(node))][, list("ScnDA", paste0(node, "_LOD"), NA, 1, NA, paste0("load_", node, "_LOD"), NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
} else {
  
  ##### Write #####
  tbl.name <- "_SCH_TMP"
  catn(tbl.name)
  chr.names <- c("//Schedule", "Time", "Value", "Enforce")
  
  dt.write <- dt.in[, list(zone, datetime, value, 1)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
}


##########
catn("Fuel")
##########

##### INPUTS #####
dt.gas <- fread(paste0(PATH.INPUTS, "Henry_Hub_Natural_Gas_Spot_Price_Daily.csv"))

##### PRE-PROCESSING #####
setnames(dt.gas, c("gas_date", "price"))
dt.gas <- dt.gas[, list(gas_date = as.Date(gas_date, "%m/%d/%Y"), price)]
dt.gas <- merge(data.table(gas_date = seq(from = as.Date(min(dt.gas$gas_date)), to = as.Date(max(dt.gas$gas_date)), by = "1 day")),
                dt.gas, by = c("gas_date"), all.x = T)
dt.gas <- dt.gas[gas_date >= as.Date(paste0(substr(hist.fuel.year, 0, 4), substr(start.date, 5, 999)))]

# Simplistically assume that missing dates are part of multi-day packages with same price as preceeding day
i <- 1
while(any(is.na(dt.gas$price)) & i < 10) {
  dt.gas[, shifted := data.table::shift(dt.gas$price, n = i, type = "lag", fill = NA)]
  dt.gas[is.na(price), price := shifted]
  dt.gas[, shifted := NULL]
  i <- i + 1
}

# Expand to cal_date and hours
dt.date.map <- data.table(gas_date = rep(dt.gas$gas_date, each = 24), he = rep(1:24, times = length(dt.gas$gas_date)))
dt.date.map[he <= 14, cal_date := gas_date]
dt.date.map[he > 14, cal_date := gas_date+1]
dt.date.map[, he := he + 10]
dt.date.map[he > 24, he := he -24]
dt.gas <- merge(dt.gas, dt.date.map, by = c("gas_date"))
dt.gas <- dt.gas[, list(datetime = as.POSIXct(paste0(cal_date, " ", he, ":00"), "%Y-%m-%d %H:%M", tz = "ETC/GMT+6"), price)]
dt.gas <- dt.gas[format(datetime, "%Y", tz = "ETC/GMT+6") == "2018"]

# subset schedules to the model run timeframe
dt.gas[, datetime := as.POSIXct(paste0(substr(start.date, 0, 4), "-", format(datetime, "%m-%d %T", tz = "ETC/GMT+6")), tz = "ETC/GMT+6")] # fix the year
dt.gas <- dt.gas[datetime >= time.interval[1] & datetime <= time.interval[2]] # subset dates
dt.gas[, datetime := format(datetime, "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")] # format dates

# Rescale the gas prices
dt.gas[, price := 4.5/mean(price)*price]

# construct table
dt.schedules <- rbind(cbind(schedule = "fuel_coal", dt.gas[, list(datetime, price = 1.96)], NA), # EIA 2020
                   cbind(schedule = "fuel_gas", dt.gas[, list(datetime, price)], NA), # $3.69 average (EIA 2020), backscaled from $4.50 2033 nominal price in LTSA
                   cbind(schedule = "fuel_bio", dt.gas[, list(datetime, price = 5)], NA), # http://biomassmagazine.com/articles/8967/eia-releases-consumption-price-data-for-wood-and-waste-biomass
                   cbind(schedule = "fuel_u", dt.gas[, list(datetime, price = .7)], NA)) # EIA 2020

# Append to SCH_TMP
tbl.name <- "_SCH_TMP"
catn(tbl.name)
chr.names <- c("//Schedule", "Time", "Value", "Enforce")

dt.write <- dt.schedules[order(schedule, datetime)]
setnames(dt.write, chr.names)
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)


##########
catn("Fast Chargers")
##########

##### GET AGGREGATE CHARGING PROFILES #####
dt.profiles <- getChargerUseProfile(show.segments = "DC Fast")
dt.profiles <- dt.profiles[time == floor(time)][, he := as.integer(time)]
dt.profiles[he == 0, he := 24]

dt.frame <- getHourTable(time.interval, chr.tz = "ETC/GMT+6")[]
dt.frame <- dt.frame[, list(datetime, date, he, peak = ifelse(wday(date) %in% c(1,7), "2x24", "5x24"))]
dt.frame <- merge(dt.frame, dt.profiles, by = c("peak", "he"), all.x = T)[order(datetime)]

dt.frame[, datetime := format(datetime, format = "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")]

# Attach actual charging nodes
dt.chg.cap <- fread(paste0(path.write, scenario, "_INJ_ID.csv"))[, list(inj = `//Injector`, cap = MaxMw)]
dt.schedules <- data.table(merge.data.frame(dt.frame, dt.chg.cap[grepl("_chg", inj)], by = NULL))
dt.schedules[, stalls := cap/getTeslaSuperchargerAtts()$power]

# Simulate the loading at each station by sampling individual chargers
# (Making sure to floor at zero)
vec.cf <- mapply(FUN = function(stalls, mu, sig) {
  sum(pmax(0, rnorm(n = stalls, mean = mu, sd = sig)))
}, stalls = dt.schedules$stalls, mu = dt.schedules$mu, sig = dt.schedules$sigma, SIMPLIFY = T)
dt.schedules[, cf := vec.cf/stalls]
dt.schedules[, val := cap*ifelse(is.na(cf), 0, cf)]

if(dr.amnt != 0) {
  # Pull in prices from ref scenario
  dt.prices <- getInjectorInfo(scn.ref, omit.node.data = T, bln.fix.missing.nodes = F)[[1]]
  dt.prices <- dt.prices[grepl("_chg", inj), list(inj, stop, lmp)]
  # Get the cheaper price neighboring each hour (destination hour)
  dt.prices <- dt.prices[order(inj, stop)]
  dt.prices[, nxt := shift(lmp, n = 1, type = "lead")]
  dt.prices[, lst := shift(lmp, n = 1, type = "lag")]
  dt.prices[, dest := ifelse(lmp == pmin(lmp, nxt, lst, na.rm = T), "ths", 
                             ifelse(nxt == pmin(lmp, nxt, lst, na.rm = T), "nxt", 
                                    "lst"))]
  # Merge on to schedules
  dt.prices[, datetime := format(stop, format = "%Y.%m.%d %H:%M", tz = "ETC/GMT+6")]
  dt.schedules <- merge(dt.schedules, dt.prices, by = c("inj", "datetime"), all.x = T)
  # Pull X% value of each hour and move to destination hour
  dt.schedules[, move_forward := ifelse(is.na(dest), 0, ifelse(dest == "nxt", dr.amnt*val, 0))]
  dt.schedules[, move_back := ifelse(is.na(dest), 0, ifelse(dest == "lst", dr.amnt*val, 0))]
  dt.schedules[, new_val := val - move_forward - move_back]
  
  dt.schedules[, from_back := shift(move_forward, 1, type = "lag", fill = 0)]
  dt.schedules[, from_forward := shift(move_back, 1, type = "lead", fill = 0)]
  dt.schedules[, new_val := new_val + from_forward + from_back]
  
  # ggplot(dt.schedules[inj == "9395_chg", list(inj, peak, he, date, val, new_val)]) +
  #   geom_line(mapping = aes(x = he, y = val, group = date), size = 2, color = "black") +
  #   geom_line(mapping = aes(x = he, y = new_val, group = date), size = 1, color = "red") +
  #   facet_wrap(~peak)
  # ggplot(dt.schedules[inj == "9395_chg" & as.Date(stop) == as.Date("2013-06-29"), list(inj, peak, he, date, val, new_val)]) +
  #   geom_line(mapping = aes(x = he, y = val, group = date), size = 2, color = "black") +
  #   geom_line(mapping = aes(x = he, y = new_val, group = date), size = 1, color = "red") +
  #   facet_wrap(~peak) + labs(x = "HE", y = "MW")
  # ggplot(dt.schedules[inj == "9395_chg", list(inj, peak, he, date, val, new_val)]) +
  #   geom_density(mapping = aes(x = val), size = 2, color = "black") +
  #   geom_density(mapping = aes(x = new_val), size = 1, color = "red") +
  #   facet_wrap(~peak)
  # dt.schedules[, list(old = sum(lmp*val, na.rm = T), new = sum(lmp*new_val, na.rm = T))][, ratio := new/old][]
  
  dt.schedules[, val := new_val]
  
}

# ggplot(dt.schedules) +
#   geom_line(mapping = aes(x = datetime, y = val, color = inj, group = inj, linetype = "sample")) +
#   geom_line(mapping = aes(x = datetime, y = cap*mu, color = inj, group = inj, linetype = "mean"))

if(switch.load.treatment != "dispersed") {
  
  # Append to SCH_TMP
  tbl.name <- "_SCH_TMP"
  catn(tbl.name)
  chr.names <- c("//Schedule", "Time", "Value", "Enforce")

  # dt.write <- dt.schedules[, list(sch = paste0("chg_", inj), date, val = cap*mu, NA)]
  dt.write <- dt.schedules[, list(sch = paste0("chg_", inj), datetime, val = val, 1)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
} else if(switch.load.treatment == "dispersed") {
  
  # Append to SCH_TMP
  tbl.name <- "_SCH_TMP"
  catn(tbl.name)
  chr.names <- c("//Schedule", "Time", "Value", "Enforce")
  
  dt.write <- dt.schedules[, list(sch = paste0("chg_", inj), datetime, val = 0, 1)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)
  
}

# And map the injectors to the schedules for PSO
tbl.name <- "_SCN_INJ_DSP"
catn(tbl.name)
chr.names <- c("//Scenario", "Injector", "Dispatch", "Enforce", "ScaleFactor", "Schedule", "Sequence")

dt.write <- dt.chg.cap[grepl("_chg", inj), list("ScnDA", inj, NA, 1, NA, paste0("chg_", inj), NA)]
setnames(dt.write, chr.names)
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"), append = T)