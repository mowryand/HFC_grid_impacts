# Script rescales gen nameplate capacity by zone according to input csv file

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### PRE PROCESSING #####

dt.alter <- melt(fread(gen.scenario.file), id.vars = c("area"), variable.name = "type")
dt.inj_id <- fread(paste0(path.write, scenario, "_INJ_ID.csv"))
dt.tech_type <- fread(paste0(PATH.INPUTS, "snl_psse_gen_map.csv"))

##### PROCESSING #####

### First need to alter the INJ_ID table, which contains INJECTOR capacity (no distributed solar handling though)
# It also already has the gen <-> area mapping
write.names <- names(dt.inj_id)

# First bind on tech types from SNL mapping
dt.tech.map <- dt.tech_type[, list(`//Injector` = psse_node, type, tech)]
dt.inj_id <- merge(dt.inj_id, dt.tech.map, by = "//Injector", all.x = T)

# Loop through the techs to have their capacities altered
vec.types <- dt.alter[value != "na", unique(type)]
for(this.type in vec.types) {
  
  # Loop through each of the explicitly defined areas to alter that area's generation
  vec.areas <- dt.alter[area != "other" & type == this.type & value != "na", area]
  for(this.area in vec.areas) {
    catn(this.type, "-", this.area)
    
    incremental.capacity <- dt.alter[type == this.type & area == this.area, as.numeric(value)]
    if(nrow(dt.inj_id[type == this.type & Area == this.area]) == 0) warning("NO EXISTING GENERATION")
    dt.inj_id[type == this.type & Area == this.area, new_cap := MaxMw/sum(MaxMw) * incremental.capacity]
    
  }
  
  # Now handle the "other" quantity, which is to be split across explicitly unhandled areas
  incremental.capacity <- dt.alter[area == "other" & type == this.type, as.numeric(value)]
  vec.areas <- setdiff(dt.alter[area != "other", unique(area)], vec.areas)
  if(this.type == "peaking") {
    dt.inj_id[tech %in% c("Internal Combustion", "Combustion Turbine") & Area %in% vec.areas, new_cap := MaxMw/sum(MaxMw) * incremental.capacity]
  } else {
    dt.inj_id[type == this.type & Area %in% vec.areas, new_cap := MaxMw/sum(MaxMw) * incremental.capacity]
  }
    
}

# Alter capacities and rewrite table
dt.inj_id[, MaxMw := MaxMw + ifelse(is.na(new_cap), 0, new_cap)]
tbl.name <- "_INJ_ID"
dt.write <- dt.inj_id[, write.names, with = F]
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

