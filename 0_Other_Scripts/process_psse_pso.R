# Script converts PSSE formatted data into PSO formatted data, optionally writes data to disk

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### PRE PROCESSING #####
lst.pso <- list()
# path.write <- paste0(PATH.IO, scenario, "/")

# Convert PSSE to R list of data.tables
if(!exists("lst.psse") | bln.force.psse.repro) {
  source(paste0(PATH.SCRIPTS, "process_raw_psse.R"))
  lst.psse <- lst.outputs
  rm(lst.outputs)
}


if(bln.use.kml.wz.mapping) {
  path.mapping.file <- paste0(PATH.TOPOLOGY, mapping.file.name, ".csv")
  dt.map <- fread(path.mapping.file)
  
  dt.map[, `:=`(Name = wz, PriceNode = paste0(id, " ", name))][, BusName := PriceNode]
  dt.map <- unique(dt.map, by = c("PriceNode"))
  
} else {
  # Take node <-> LZ mapping (ignore nodes not mapped to LZ)
  mapping.file <- list.files(PATH.TOPOLOGY)[grepl("SourcesAndSinks\\.csv", list.files(PATH.TOPOLOGY))]
  path.mapping.file <- paste0(PATH.TOPOLOGY, mapping.file)
  dt.map <- fread(path.mapping.file)
  
  dt.map <- dt.map[grepl("LZ_", Name)]
  dt.map[, Name := substr(Name, 4, 99)]
  dt.map <- unique(dt.map[, list(Name, PriceNode, BusName)], by = c("Name", "PriceNode"))
}

if(nrow(lst.psse[["BRANCH"]][(! I %in% lst.psse[["BUS"]]$I ) | (! J %in% lst.psse[["BUS"]]$I )]) > 0) {
  stop("SOME PSSE BRANCHES NOT MAPPED TO PSSE BUSES")
}
lost <- lst.psse[["BUS"]][! (I %in% lst.psse[["BRANCH"]][, c(I, J)] | I %in% lst.psse[["TRANSFORMER"]][, c(I, J)])]
if(nrow(lost) > 0) {
  warning("Removing ", nrow(lost), " PSSE buses that weren't attached to branches or XFs")
}

dt.valid.buses <- lst.psse[["BUS"]]
dt.valid.buses <- lst.psse[["BUS"]][(BASKV >= minimum.kv | IDE == 2) & !I %in% lost$I] # save the gens
if(nrow(dt.valid.buses) < nrow(lst.psse[["BUS"]])) {
  warning("Dropping ", nrow(lst.psse[["BUS"]]) - nrow(dt.valid.buses), " low KV buses")
}
dt.valid.lines <- lst.psse[["BRANCH"]][I %in% dt.valid.buses$I & J %in% dt.valid.buses$I]
if(nrow(dt.valid.lines) < nrow(lst.psse[["BRANCH"]])) {
  warning("Dropping ", nrow(lst.psse[["BRANCH"]]) - nrow(dt.valid.lines), " low KV lines")
}
dt.valid.xf <- lst.psse[["TRANSFORMER"]][I %in% dt.valid.buses$I & J %in% dt.valid.buses$I]
if(nrow(dt.valid.xf) < nrow(lst.psse[["TRANSFORMER"]])) {
  warning("Dropping ", nrow(lst.psse[["TRANSFORMER"]]) - nrow(dt.valid.xf), " low KV XFs")
}
dt.valid.gens <- lst.psse[["GENERATOR"]][PT > 0]
if(nrow(dt.valid.gens) < nrow(lst.psse[["GENERATOR"]])) {
  warning("Dropping ", nrow(lst.psse[["GENERATOR"]]) - nrow(dt.valid.gens), " MaxMW <= 0 Gens")
}
if(nrow(dt.valid.gens[I %in% dt.valid.buses$I]) < nrow(dt.valid.gens)) {
  # dt.valid.gens[!I %in% dt.valid.buses$I]
  warning("Dropping ", nrow(dt.valid.gens) - nrow(dt.valid.gens[I %in% dt.valid.buses$I]), " Gens because their buses were eliminated")
}
dt.valid.gens <- dt.valid.gens[I %in% dt.valid.buses$I]

warning("Dumping 4 problematic reverse parallel branches")
dt.valid.lines <- dt.valid.lines[!(I == 6310 & J == 6309) & !(I == 6359 & J == 6390) & !(I == 37900 & J == 37920) & !(I == 110377 & J == 110376)]
dt.valid.xf <- dt.valid.xf[!(I == 6310 & J == 6309) & !(I == 6359 & J == 6390) & !(I == 37900 & J == 37920) & !(I == 110377 & J == 110376)]

##### MAIN PROCESSING #####

# Area file (ARA_ID), defining child areas of system for load scheduling
tbl.name <- "_ARA_ID"
catn(tbl.name)
chr.names <- c("//Area","Name","ParentArea","Balance","External")

dt.write <- rbind(data.table("0", "ERCOT", NA, 1, 0),
                  unique(dt.map, by = "Name")[, list(Name, Name, "0", 0, 0)],
                  use.names = F)
setnames(dt.write, chr.names)
lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

# Substation file (SUB_ID) facilitates mapping of nodes to areas. Only need an area mapping for load?
# In any case, > 50% nodes not mapped to an LZ
tbl.name <- "_SUB_ID"
catn(tbl.name)
chr.names = c("//Substation","Name","Area")

dt.merge <- dt.valid.buses[, list(I, name = gsub(" *", "", gsub("'", "", NAME)))]
dt.merge2 <- unique(dt.map[, list(name, area = Name)])

dt.raw <- merge(dt.merge, dt.merge2, by = "name", all.x = T)
dt.raw[, area := ifelse(is.na(area), "0", area)]

dt.write <- dt.raw[, list(I, name, area)]
setnames(dt.write, chr.names)

if(bln.assign.missing.mapping.by.neighbor) {
  dt.subs <- copy(dt.write)
} else {
  lst.pso[[tbl.name]] <- dt.write
  if(bln.write.to.file) fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
}

# Node file (NDE_ID):
tbl.name <- "_NDE_ID"
catn(tbl.name)
chr.names = c("//Enode","Name","Busbar","Substation","ReportNode")

dt.raw <- dt.valid.buses
dt.raw[, NAME := gsub(" *", "", gsub("'", "", NAME))]

if(rpt.all.node.lmp == "all") {
  dt.write <- dt.raw[, list(I, NAME, NA, I, 1)]
} else if(rpt.all.node.lmp == "none") {
  dt.write <- dt.raw[, list(I, NAME, NA, I, 0)]  
} else if(rpt.all.node.lmp == "chg") {
  # Processed in get_charger_network.R, after we attach chargers
  dt.write <- dt.raw[, list(I, NAME, NA, I, 0)]  
} else {
  stop()
}
setnames(dt.write, chr.names)
lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

# Branch file (BRN_ID):
tbl.name <- "_BRN_ID"
catn(tbl.name)
chr.names <- c("//Branch","Name","FrEnode","ToEnode","Circuit","Voltage","Resistance",
               "Reactance","NormalLimit","CtgLimit","Solve","Enforce","Monitor","Switchable",
               "Penalty","AngleLimit","Hvdc","CID")

dt.raw <- merge(dt.valid.lines[, list(I, J, CKT, R, X, RATEA, RATEB)], 
                dt.valid.buses[, list(I, NAME, BASKV, check1 = 1)], 
                by.x = "I", by.y = "I", all.x = T, suffixes = c("", "_I"))
dt.raw <- merge(dt.raw, 
                dt.valid.buses[, list(I, NAME, BASKV, check2 = 1)], 
                by.x = "J", by.y = "I", all.x = T, suffixes = c("", "_J"))
if(nrow(rbind(dt.raw[is.na(check1) | check1 != 1], dt.raw[is.na(check2) | check2 != 1])) > 0) stop("Unmapped branches")
dt.raw[, CKT := gsub("'", "", CKT)]
dt.raw[, NAME := gsub(" *", "", gsub("'", "", NAME))]
dt.raw[, NAME_J := gsub(" *", "", gsub("'", "", NAME_J))]
dt.raw[, name := paste(I, NAME, BASKV, J, NAME_J, BASKV_J)]

dt.write <- dt.raw[, list(paste(I, J), name, I, J, CKT, pmax(BASKV, BASKV_J), R, 
                          X, 
                          (RATEA/limit.scaler)*(1 + gbl.xm.rlx), 
                          (RATEB/limit.scaler)*(1 + gbl.xm.rlx), 1, 1, NA, NA, NA, NA, NA, NA)]

if(bln.xf.to.xm) {
  
  dt.raw <- merge(dt.valid.xf[, list(I, J, CKT, R = `R1-2`, X = `X1-2`, RATEA = `RATA1`, RATEB = `RATB1`)], 
                  dt.valid.buses[, list(I, NAME, BASKV, check1 = 1)], 
                  by.x = "I", by.y = "I", all.x = T, suffixes = c("", "_I"))
  dt.raw <- merge(dt.raw, 
                  dt.valid.buses[, list(I, NAME, BASKV, check2 = 1)], 
                  by.x = "J", by.y = "I", all.x = T, suffixes = c("", "_J"))
  if(nrow(rbind(dt.raw[is.na(check1) | check1 != 1], dt.raw[is.na(check2) | check2 != 1])) > 0) stop("Unmapped branches")
  dt.raw[, CKT := gsub("'", "", CKT)]
  dt.raw[, NAME := gsub(" *", "", gsub("'", "", NAME))]
  dt.raw[, NAME_J := gsub(" *", "", gsub("'", "", NAME_J))]
  dt.raw[, name := paste(I, NAME, BASKV, J, NAME_J, BASKV_J)]
  
  dt.raw <- dt.raw[, list(paste(I, J), name, I, J, CKT, pmax(BASKV, BASKV_J), R, 
                          X, 
                          (RATEA/limit.scaler)*(1 + gbl.xm.rlx), 
                          (RATEB/limit.scaler)*(1 + gbl.xm.rlx), 
                          1, 1, NA, NA, NA, NA, NA, NA)]
  
  dt.write <- rbind(dt.write, dt.raw)
}

setnames(dt.write, chr.names)
# dt.write[Voltage >= 345, Enforce := 1]

lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
  
if(bln.assign.missing.mapping.by.neighbor) {
  # Get edges
  dt.raw <- dt.write[, list(I = tstrsplit(`//Branch`, " ")[[1]], J = tstrsplit(`//Branch`, " ")[[2]])]
  dt.raw[, `:=`(I = as.numeric(I), J = as.numeric(J))]
  dt.edge <- unique(rbind(dt.raw, dt.raw[, list(I = J, J = I)]))
  
  dt.lost <- dt.subs[Area == 0]
  dt.okay <- dt.subs[Area != 0]
  
  # Attach edges
  dt.merge <- rbind(merge(dt.lost, dt.edge, by.x = "//Substation", by.y = "I", all.x = T),
                    merge(dt.lost, dt.edge, by.x = "//Substation", by.y = "I", all.x = T))
  dt.merge <- dt.merge[J != `//Substation`]
  dt.merge[, "Area" := NULL]
  
  dt.merge <- merge(dt.merge, dt.okay[, list(`//Substation`, Area)], 
                    by.x = "J", by.y = "//Substation", all.x = T)
  
  dt.okay <- rbind(dt.okay, unique(dt.merge[!is.na(Area), list(`//Substation`, Name, Area)], by = "//Substation"))
  setnames(dt.merge, "J", "s2")
  dt.lost <- unique(dt.merge[is.na(Area) & !`//Substation` %in% dt.okay$`//Substation`], 
                    by = c("//Substation", "s2"))
  
  # Again
  dt.merge <- rbind(merge(dt.lost, dt.edge, by.x = "s2", by.y = "I", all.x = T),
                    merge(dt.lost, dt.edge, by.x = "s2", by.y = "I", all.x = T))
  dt.merge <- dt.merge[J != `//Substation` & J != s2]
  dt.merge[, "Area" := NULL]
  
  dt.merge <- merge(dt.merge, dt.okay[, list(`//Substation`, Area)], 
                    by.x = "J", by.y = "//Substation", all.x = T)
  
  dt.okay <- rbind(dt.okay, unique(dt.merge[!is.na(Area), list(`//Substation`, Name, Area)], by = "//Substation"))
  setnames(dt.merge, "J", "s3")
  dt.lost <- unique(dt.merge[is.na(Area) & !`//Substation` %in% dt.okay$`//Substation`], 
                    by = c("//Substation", "s2", "s3"))
  
  # Again
  dt.merge <- rbind(merge(dt.lost, dt.edge, by.x = "s3", by.y = "I", all.x = T),
                    merge(dt.lost, dt.edge, by.x = "s3", by.y = "I", all.x = T))
  dt.merge <- dt.merge[J != `//Substation` & J != s2 & J != s3]
  dt.merge[, "Area" := NULL]
  
  dt.merge <- merge(dt.merge, dt.okay[, list(`//Substation`, Area)], 
                    by.x = "J", by.y = "//Substation", all.x = T)
  
  dt.okay <- rbind(dt.okay, unique(dt.merge[!is.na(Area), list(`//Substation`, Name, Area)], by = "//Substation"))
  setnames(dt.merge, "J", "s4")
  dt.lost <- unique(dt.merge[is.na(Area) & !`//Substation` %in% dt.okay$`//Substation`], 
                    by = c("//Substation", "s2", "s3", "s4"))
  
  if(nrow(dt.lost) > 0) stop("Still unmapped buses!")
  
  lst.pso[["_SUB_ID"]] <- dt.okay
  if(bln.write.to.file) fwrite(dt.okay, paste0(path.write, scenario, "_SUB_ID", ".csv"))
} else {
}

if(bln.populate.load.weights) {
  tbl.name <- "_STE_NDE"
  catn(tbl.name)
  chr.names <- c("//State", "Enode", "GenMw", "LoadMw", "GenNode", "LoadNode")
  
  dt.loads <- lst.psse[["LOAD"]][, list(I, PL)]
  if(scenario %in% c("Scn_900", "Scn_901")) dt.loads <- dt.loads[I == 675]
  # dt.loads <- merge(lst.pso[["_SUB_ID"]][, list(I = `//Substation`)], dt.loads, by = c("I"), all.x = T)
  dt.loads <- merge(lst.pso[["_SUB_ID"]][, list(I = `//Substation`)], dt.loads, by = c("I"), all = T)
  
  # Truncating loads
  # dt.loads <- dt.loads[!I %in% dt.valid.gens$I, list(I, PL = ifelse(is.na(PL), 0, pmax(0, PL)))]
  warning("Trimming ", nrow(dt.loads[!(!is.na(PL) & PL > 0)]), " load nodes.")
  dt.loads <- dt.loads[!is.na(PL) & PL > 0]
  dt.loads <- dt.loads[, list(PL = sum(PL)), by = list(I)]
  
  dt.write <- dt.loads[, list(0, I, NA, PL, NA, 1)]
  setnames(dt.write, chr.names)
  lst.pso[[tbl.name]] <- dt.write
  if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))
}

##### Generation #####

# Generator mapping file (INJ_NET), identifying node mapping
# (using node ID as injector ID)
tbl.name <- "_INJ_NET"
catn(tbl.name)
chr.names <- c("//Injector","Node","PhysicalArea","LossFactor","IgnoreLoss")

dt.raw <- dt.valid.gens
dt.write <- dt.raw[, list(paste0(I, "_", ID), I, NA, NA, NA)]

if(treat.load.as == "INJ") {
  dt.write <- rbind(dt.write, 
                    lst.pso[["_STE_NDE"]][, list(paste0(Enode, "_LOD"), Enode, NA, NA, NA)],
                    use.names = F)
}

setnames(dt.write, chr.names)
lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

# Generator file (INJ_ID), identifying area mapping, costing, capacity
tbl.name <- "_INJ_ID"
catn(tbl.name)
chr.names = c("//Injector","Name","Area","LoadFlag","Link","MaxMw","MinMw",
              "RaiseRR","LowerRR","RampCapOnly","EnergyCost","CostAdder",
              "RampUpCost","RampDnCost")

dt.raw <- merge(dt.valid.gens, dt.valid.buses[, list(I, node_name = NAME)], by = "I")
dt.raw[, node_name := gsub(" *", "", gsub("'", "", node_name))]

dt.raw <- merge(dt.raw, lst.pso[["_SUB_ID"]][, list(I = `//Substation`, Area)], by = "I", all.x = T)
dt.raw[, Name := ifelse(is.na(Area), "0", Area)]

if(bln.filler.gen.stats) {
  dt.write <- dt.raw[, list(paste0(I, "_", ID), paste0(node_name, "_", ID), Area, 0, NA, PT, NA,
                            NA, NA, NA, 1:.N, NA,
                            NA, NA)]
} else {
  dt.write <- dt.raw[, list(paste0(I, "_", ID), paste0(node_name, "_", ID), Area, 0, NA, PT, NA,
                            NA, NA, NA, NA, NA,
                            NA, NA)]
}

if(treat.load.as == "INJ") {
  
  dt.add <- merge(lst.pso[["_SUB_ID"]][, list(I = `//Substation`, Area, Name)],
                  lst.pso[["_STE_NDE"]][, list(I = Enode, LoadMw)],
                  by = c("I"), all.y = T)
  ## Encountered a problem when using LoadMw as the load injector rating (as above merge does)
  ## This causes "clipping" of load in the model: the given LoadMw numbers, when interpreted as load rating
  ## do not combine to at least ERCOT's full load in every hour. So I need to beef these up significantly (just to be sure).
  dt.add[, LoadMw := 1000]
  
  dt.write <- rbind(dt.write,
                    dt.add[, list(paste0(I, "_LOD"), paste0(Name, "_LOD"), Area, 1, NA, LoadMw, NA,
                                  NA, NA, NA, NA, NA, NA, NA)],
                    use.names = F)
}

setnames(dt.write, chr.names)
lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv"))

# Generator file (INJ_CMT), identifying min MW when on
tbl.name <- "_INJ_CMT"
catn(tbl.name)
chr.names <- c("//Injector","MinDispatch","MinOn","MinOff","BaseCost",
               "HotStartCost","WarmStartCost","ColdStartCost","TimeToWarm","TimeToCold",
               "HotUpTime","WarmUpTime","ColdUpTime")

dt.raw <- dt.valid.gens

dt.write <- dt.raw[, list(paste0(I, "_", ID), pmax(PB, 1), NA, NA, NA,
                          NA, NA, NA, NA, NA,
                          NA, NA, NA)]
setnames(dt.write, chr.names)
dt.write[MinDispatch == 0, MinDispatch := 1]
lst.pso[[tbl.name]] <- dt.write
if(bln.write.to.file)  fwrite(dt.write, paste0(path.write, scenario, tbl.name, ".csv")) # (not really necessary in base run)

