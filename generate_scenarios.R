# Control script that prepares all files for PSO runs
# Reads data inputs
# Transforms according to settings in "run_info.csv"
# Writes to scenario directory


##### PACKAGES #####

library(data.table)
library(XML)
library(RANN)

set.seed(123456)


##### PATH DEFS #####

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

# Pull in miscellaneous functions
source(paste0(PATH.SCRIPTS, "library.R"))


##### SCENARIO DEFINITIONS #####

dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))

if(F) {
  # To regenerate all scenarios, run this section of code. Otherwise, previously generated scenarios
  # (as evidenced by an existing "id") will not be generated.
  dt.run.info[, id := NULL]
}

dt.to.run <- dt.run.info[is.na(id)]
fwrite(file = paste0(PATH.INPUTS, "run_info.csv"), dt.run.info[!is.na(id)], append = F)
if(!nrow(dt.to.run)) {
  stop("No scenarios to run.")
} else {
  for(this.index in 1:nrow(dt.to.run)) {
    
    dt.settings <- dt.to.run[this.index]
    extractScenarioVariables(dt.settings)
    
    path.write <- paste0(PATH.IO, scenario, "/")
    
    # Find the files from the last run (independent of scenario... we just want the latest structure and config)
    dt.prev <- fread(paste0(PATH.INPUTS, "run_info.csv"))
    prev.run <- max(dt.prev$id, na.rm = T)
    prev.scn <- dt.prev[id == prev.run, scn]
    prev.write <- paste0(PATH.IO, prev.scn, "/")
    prev.files <- list.files(prev.write)
    prev.files.copy <- prev.files[grepl(paste0(prev.scn, "\\.csv"), prev.files) |
                                    grepl("MDL_ID|CYC_ID|CYC_PRD_ID|SCN_ARA_LOD|SCN_CYC|SCN_FUE_CST|LIB_RPT", prev.files)]
    
    # Copy files to new temp dir, later to be renamed
    path.tmp <- paste0(PATH.IO, "tmp/")
    dir.create(path.tmp)
    file.copy(from = paste0(prev.write, prev.files.copy), to = paste0(path.tmp, gsub(prev.scn, scenario, prev.files.copy)))
    
    # Check if renaming will overwrite anything
    if(dir.exists(path.write)) {
      # If yes, archive that-which-will-be-overwritten by renaming it to new directory
      last.this.scn.id <- max(dt.prev[scn == scenario, id])
      archive.scn <- paste0(scenario, "_", last.this.scn.id)
      archive.dir <- paste0(PATH.IO, archive.scn)
      file.rename(from = path.write, to = archive.dir)
    }
    
    # Rename tmp dir
    file.rename(from = paste0(PATH.IO, "tmp"), paste0(PATH.IO, scenario))
    
    # Record run
    dt.write <- rbind(fread(paste0(PATH.INPUTS, "run_info.csv")),
                      data.table(id = prev.run + 1, 
                                 scn = scenario, 
                                 start = start.date, 
                                 end = end.date, 
                                 cap_mod = cap.mod, 
                                 bat = bln.add.batteries, 
                                 bat_mod = bat.pow.mod, 
                                 bat_min = bat.pow.min,
                                 bat_loc = paste0(bat.add.subset, collapse = "_"),
                                 xm_mod = bln.mod.xm,
                                 dr_amnt = dr.amnt, 
                                 scn_ref = scn.ref,
                                 xm_rlx = gbl.xm.rlx,
                                 xm_sf = xm.sf,
                                 xm_viol = xm.viol,
                                 ltsa = gen.scenario, 
                                 fev = switch.load.treatment, 
                                 ld_mdl = treat.load.as,
                                 desc = description, 
                                 ts = format(Sys.time(), "%Y-%m-%d %T %z", tz = "ETC/GMT+5"), 
                                 rpt_all = rpt.all.node.lmp,
                                 curtail = bln.curtail))
    fwrite(file = paste0(PATH.INPUTS, "run_info.csv"), dt.write[order(scn, id, decreasing = F)], append = F)
    
    #####
    # 1. Read in PSSE file from ERCOT. (R list from .raw file)
    # source(paste0("C:/Users/", CHR.USER, "/Dropbox (MIT)/EV Charging Infra/ERCOT_PSO, scripts/process_raw_psse.R"))
    # This step is now unnecessary and integrated into #3
    
    #####
    # 2. Parse .KML file to get geographic info for buses in the ERCOT files
    mapping.file.name <- "parsed_bus_info"
    # bln.write <- T
    # USE THIS SCRIPT TO REGENERATE THE FILE IF MISSING: util_extract_bus_lz_map.R
    
    #####
    # 3. Write most PSO input CSVs (CSV from R list)
    bln.write.to.file <- T # Write PSO files to directory?
    bln.xf.to.xm <- T # Treat PSSE transformers as PSO branches?
    bln.remove.0mw.gen <- T # Remove 0 MAX MW injectors?
    bln.force.psse.repro <- F # Even if lst.psse in memory, reprocess it?
    minimum.kv <- 0 # Exclude lower kv buses/lines below this threshold
    limit.scaler <- .9 # scaling up line capacities (b/c not full capacity available in CRR auctions)
    # gbl.xm.rlx <- 0
    bln.use.kml.wz.mapping <- T # If not, use PSSE lower res lz data
    bln.assign.missing.mapping.by.neighbor <- T # Assign missing area maps by looking at branch neighbors
    bln.populate.load.weights <- T # Construct STE_NDE table for loads
    bln.filler.gen.stats <- F # True to put in filler VOMs for gens for dispatch if not being more granular
    
    source(paste0(PATH.SCRIPTS, "process_psse_pso.R"))
    
    #####
    # 4. Overwrite injector tables, thermal injector and fuel characteristics
    manual.gen.mapping <- "snl_psse_gen_map"  
    manual.gen.defaults <- "gen_type_defaults"
    bln.write.to.file <- T
    bln.create.groups <- F # If TRUE, makes groupings. If FALSE, writes same info to individual records
    
    source(paste0(PATH.SCRIPTS, "process_gen_groups.R"))
    
    #####
    # 5. Append to injector tables for charging network AND batteries (attached to chargers but independent)
    bln.write.to.file <- T
    future.year <- 2033
    
    source(paste0(PATH.SCRIPTS, "process_get_charger_network.R"))
    
    #####
    # 6. Adapt the current grid to 2033 LTSA projections
    bln.write.to.file <- T
    gen.scenario.file <- paste0(PATH.INPUTS, gen.scenario, ".csv")
    source(paste0(PATH.SCRIPTS, "process_alter_gen.R"))
    
    #####
    # 7.1 Simplify transmission table by collapsing parallel lines
    if(T) {
      dt.brn <- fread(paste0(path.write, scenario, "_BRN_ID.csv"))
      write.names <- names(dt.brn)
      dt.brn <- dt.brn[, list(Name = Name[1], Circuit = 1, 
                              NormalLimit = sum(NormalLimit), CtgLimit = sum(CtgLimit), 
                              Resistance = weighted.mean(Resistance, NormalLimit), Reactance = weighted.mean(Reactance, NormalLimit)), 
                       by = list(`//Branch`, FrEnode, ToEnode, Voltage, Solve, Enforce, Monitor, Switchable, Penalty, AngleLimit, Hvdc, CID)]
      dt.brn <- dt.brn[order(`//Branch`), write.names, with = F]
      fwrite(dt.brn, paste0(path.write, scenario, "_BRN_ID.csv"), append = F)
    }
    
    # 7.2 Perform selective transmission upgrades
    if(T) {
      dt.upgrades <- fread(paste0(PATH.INPUTS, "xm_upgrades.csv"))[scn == scenario]
      # Get charger sizes from injector tables
      dt.chg <- fread(paste0(path.write, scenario, "_INJ_ID.csv"))[`//Injector` %in% dt.upgrades$inj, list(inj = `//Injector`, cap = MaxMw)]
      dt.upgrades <- merge(dt.upgrades, dt.chg, by = c("inj"))
      dt.brn <- fread(paste0(path.write, scenario, "_BRN_ID.csv"))
      for(i in unique(dt.upgrades$inj)) {
        dt.brn <- merge(dt.brn, dt.upgrades[inj == i, list(branch, cap)], by.x = "//Branch", by.y = "branch", all.x = T)
        dt.brn[!is.na(cap), c("NormalLimit", "CtgLimit") := list(NormalLimit + cap*bln.mod.xm, CtgLimit + cap*bln.mod.xm)]
        dt.brn[, cap := NULL]
      }
      fwrite(dt.brn, paste0(path.write, scenario, "_BRN_ID.csv"), append = F)
    }
    
    # 7.3 Original, shift-factor found upgrades for renewables export
    if(xm.sf == T) {
      catn("SF-based upgrades")
      dt.upgrades <- fread(paste0(PATH.INPUTS, "xm_upgrades_sf_601.csv"))
      dt.brn <- fread(paste0(path.write, scenario, "_BRN_ID.csv"))
      dt.brn <- merge(dt.brn, dt.upgrades[, list(branch, mw)], by.x = "//Branch", by.y = "branch", all.x = T)
      dt.brn[, N := .N, by = list(`//Branch`)]
      dt.brn[!is.na(mw), mw := mw/N][, N := NULL]
      dt.brn[!is.na(mw), c("NormalLimit", "CtgLimit") := list(NormalLimit + mw, CtgLimit + mw)]
      dt.brn[, mw := NULL]
      fwrite(dt.brn, paste0(path.write, scenario, "_BRN_ID.csv"), append = F)
    }
    
    # 7.4 Brute force, violation smashing upgrades
    if(xm.viol == T) {
      catn("Violation-based upgrades")
      dt.upgrades <- fread(paste0(PATH.INPUTS, "xm_upgrades_surgical_NEW.csv"))
      dt.brn <- fread(paste0(path.write, scenario, "_BRN_ID.csv"))
      dt.brn <- merge(dt.brn, dt.upgrades[, list(branch, mw)], by.x = "//Branch", by.y = "branch", all.x = T)
      dt.brn[, N := .N, by = list(`//Branch`)]
      dt.brn[!is.na(mw), mw := mw/N][, N := NULL]
      dt.brn[!is.na(mw), c("NormalLimit", "CtgLimit") := list(NormalLimit + mw, CtgLimit + mw)]
      dt.brn[, mw := NULL]
      fwrite(dt.brn, paste0(path.write, scenario, "_BRN_ID.csv"), append = F)
    }
    
    #####
    # 8. Write schedules for load, renewables, ev chargers
    bln.write.to.file <- T
    hist.load.year <- 2017
    hist.renewables.year <- 2013
    hist.fuel.year <- hist.load.year
    future.load.year <- future.year
    
    source(paste0(PATH.SCRIPTS, "process_create_sch_tmp.R"))
    
    
  }
}
