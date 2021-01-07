# Script creates generation groups for PSO

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### PRE-PROCESSING #####

dt.gens <- fread(paste0(PATH.INPUTS, manual.gen.mapping, ".csv"))

# Calc tech-type avg params for thermal gens
dt.avgs <- dt.gens[, list(vom = mean(cost_vom, na.rm = T), hr = mean(hr, na.rm = T)), 
                   by = list(type, tech)][order(type, tech)]
# Deal with renewables
dt.avgs[hr == 0, hr := NA]

# Add on ramp info, from https://www.researchgate.net/post/What_is_the_typical_MW_minute_ramping_capability_for_each_type_of_reserve
dt.info <- fread(paste0(PATH.INPUTS, manual.gen.defaults, ".csv"))
dt.avgs <- merge(dt.avgs, dt.info, by = c("type", "tech"), all.x = T)

##### PROCESSING #####

if(bln.create.groups) {
  catn("GROUPED INJECTOR DATA")
  # Identify groupings
  tbl.name <- "_GRP_INJ"
  catn(tbl.name)
  chr.names <- c("//Group", "Injector", "UsageFactor")
  
  dt.write <- dt.gens[, list(paste0(type, " - ", tech), psse_node, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
  # Write dispatch data for groupings
  tbl.name <- "_GRP_DSP"
  catn(tbl.name)
  chr.names <- c("//Group", "MinMw", "RaiseRR", "LowerRR", "RampCapOnly", "EnergyCost", "CostAdder",
                 "RampUpCost", "RampDnCost")
  
  dt.write <- dt.avgs[, list(paste0(type, " - ", tech), NA, mw_min, mw_min, NA, NA, vom, NA, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
  # Write heatrate data
  tbl.name <- "_GRP_HEA"
  catn(tbl.name)
  chr.names <- c("//Group", "HeatCost", "IncHeat", "BaseHeat", "HotStartHeat", "WarmStartHeat", "ColdStartHeat")
  
  dt.write <- dt.avgs[, list(paste0(type, " - ", tech), NA, hr, NA, NA, NA, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
  # Write fuel mapping
  tbl.name <- "_FUE_GRP"
  catn(tbl.name)
  chr.names <- c("//FuelType", "Group", "FuelFactor", "Cost", "CostAdder")
  
  dt.write <- dt.avgs[fuel != "", list(fuel, paste0(type, " - ", tech), 1, fuel_cost, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
} else {
  catn("INDIVIDUAL INJECTOR DATA")
  dt.gens <- merge(dt.gens, dt.info, by = c("type", "tech"), all.x = T)
  dt.gens <- merge(dt.gens, dt.avgs[, list(type, tech, vom, hr, mw_min)], 
                   by = c("type", "tech"), all.x = T, suffixes = c("", "_avg"))
  dt.gens[is.na(hr) & fuel != "", c("hr", "mw_min") := list(hr_avg, mw_min_avg)]
  dt.gens[is.na(cost_vom), c("cost_vom") := list(vom)]
  
  #
  tbl.name <- "_INJ_ID"
  catn(tbl.name)
  dt.old <- fread(paste0(path.write, scenario, tbl.name, ".csv"))  
  
  dt.write <- merge(dt.old, dt.gens, by.x = "//Injector", by.y = "psse_node", all.x = T)
  dt.write <- dt.write[, list(`//Injector`, Name, Area, LoadFlag, Link, MaxMw, MinMw, 
                              RaiseRR = mw_min, LowerRR = mw_min, RampCapOnly, EnergyCost, 
                              CostAdder = cost_vom, RampUpCost, RampDnCost)]
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
  #
  tbl.name <- "_INJ_HEA"
  catn(tbl.name)
  chr.names <- c("//Injector", "HeatCost", "IncHeat", "BaseHeat", 
                 "HotStartHeat", "WarmStartHeat", "ColdStartHeat")
  
  dt.write <- dt.gens[fuel != "", list(psse_node, NA, hr, NA, NA, NA, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  #
  tbl.name <- "_FUE_UTH"
  catn(tbl.name)
  chr.names <- c("//FuelType", "ThermalUnit", "FuelFactor", "Cost", "CostAdder")
  
  dt.write <- dt.gens[fuel != "", list(fuel, psse_node, 1, fuel_cost, NA)]
  setnames(dt.write, chr.names)
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
}

# Write fuel data
tbl.name <- "_FUE_ID"
catn(tbl.name)
chr.names <- c("//FuelType", "Name", "Quantity", "Heat", "Cost")

dt.write <- unique(dt.info[fuel != "", list(fuel, fuel, "MMBtu", 1, fuel_cost)])
setnames(dt.write, chr.names)
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))