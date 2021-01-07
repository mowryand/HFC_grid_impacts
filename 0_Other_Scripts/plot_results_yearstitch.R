# Charts for comparing series of Base -> Disp -> Conc

##### LIBRARIES #####
library(data.table)
library(ggplot2)
library(viridis)

factorizeCapMod <- function(cap_mod) {
  labs <- ifelse(cap_mod == .75, "75% Pen.", 
                 ifelse(cap_mod == 1.00, "LTSA EV Penetration", 
                        ifelse(cap_mod == 1.25, "125% Pen.", 
                               ifelse(cap_mod == .5, "50% Pen.", 
                                      ifelse(cap_mod == 1.5, "150% Pen.", NA)))))
  labs <- factor(labs, levels = c("50% Pen.", "75% Pen.", "LTSA EV Penetration", "125% Pen.", "150% Pen."))
  return(labs)
}

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

getChargerImpacts <- function(scn.list, metric = c("cost", "emissions")[1]) {
  
  stopifnot(metric %in% c("cost", "emissions"))
  
  # Get scenario information
  dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))
  dt.series <- merge(data.table(scn = unique(unlist(lapply(scn.list, function(x) unlist(x))))), 
                     dt.run.info[, list(id, scn, cap_mod, bat, bat_mod, xm_rlx, ltsa, fev, dr_amnt)], 
                     by = "scn")[, .SD[id == max(id)], by = scn]
  
  if(metric == "cost") {
    # Calculate penalties and costs per scenario
    dt.opts1 <- rbindlist(lapply(X = scn.list, FUN = function(sequences) {
      
      dt.base <- scenarioYearStitch.getOptResultsAccurate(sequences[["base"]], summed = F)[, list(cost, pen, stop = stop_start, int)]
      dt.disp <- scenarioYearStitch.getOptResultsAccurate(sequences[["disp_0"]], summed = F)[, list(cost, pen, stop = stop_start, int)]
      dt.conc <- scenarioYearStitch.getOptResultsAccurate(sequences[["conc_0"]], summed = F)[, list(cost, pen, stop = stop_start, int)]
      
      dt.transfer <- merge(dt.base, dt.disp, by = c("int", "stop"), suffixes = c("_base", "_disp"))[
        , list(cost_base, cost_disp, cost_transfer = cost_disp - cost_base, pen_base, pen_disp, pen_transfer = pen_disp - pen_base,
               int, stop, 
               scn_base = sequences[["base"]][1], scn_disp = sequences[["disp_0"]][1], scn_conc = sequences[["conc_0"]][1])]
      dt.local <- merge(dt.disp, dt.conc, by = c("int", "stop"), suffixes = c("_disp", "_conc"))[
        , list(cost_conc, cost_local = cost_conc - cost_disp, pen_conc, pen_local = pen_conc - pen_disp,
               int, stop)]
      
      dt.return <- merge(dt.transfer, dt.local, by = c("int", "stop"))
      
      return(dt.return)
      
    }))
    
    # Attach scenario information
    dt.plot1 <- merge(dt.opts1, dt.series, by.x = "scn_conc", by.y = "scn")
    dt.plot1 <- melt(dt.plot1, id.vars = c("stop", "cap_mod", "bat_mod"), measure.vars = c("pen_transfer", "pen_local", "cost_transfer", "cost_local"))
    
  } else if(metric == "emissions") {
    
    # Calculate penalties and costs per scenario
    dt.opts1 <- rbindlist(lapply(X = scn.list, FUN = function(sequences) {
      
      dt.base <- scenarioYearStitch.getInjectorEmissions(sequences[["base"]])
      dt.disp <- scenarioYearStitch.getInjectorEmissions(sequences[["disp_0"]])
      dt.conc <- scenarioYearStitch.getInjectorEmissions(sequences[["conc_0"]])
      
      dt.transfer <- merge(dt.base, dt.disp, by = c("int", "stop"), suffixes = c("_base", "_disp"))[
        , list(int, stop, tons_co2_transfer = tons_co2_disp - tons_co2_base, 
               scn_base = sequences[["base"]][1], scn_disp = sequences[["disp_0"]][1], scn_conc = sequences[["conc_0"]][1])]
      dt.local <- merge(dt.disp, dt.conc, by = c("int", "stop"), suffixes = c("_disp", "_conc"))[
        , list(int, stop, tons_co2_local = tons_co2_conc - tons_co2_disp)]
      
      dt.return <- merge(dt.transfer, dt.local, by = c("int", "stop"))
      
      return(dt.return)
    }))
    
    # Attach scenario information
    dt.plot1 <- merge(dt.opts1, dt.series, by.x = "scn_conc", by.y = "scn")
    dt.plot1 <- melt(dt.plot1, id.vars = c("stop", "cap_mod", "bat_mod"), measure.vars = c("tons_co2_local", "tons_co2_transfer"))
  } 
  
  return(dt.plot1)
}
plotChargerImpacts <- function(scn.list, metric = c("cost", "emissions")[1], dt.plot1 = NULL) {
  
  stopifnot(metric %in% c("cost", "emissions"))
  
  # Get impacts
  if(is.null(dt.plot1)) dt.plot1 <- getChargerImpacts(scn.list, metric)
  
  
  # Daily and Yearly Plots
  if(F) {
    # Daily
    plt1.1 <- ggplot(dt.plot1) + theme_bw(GGPLOT_SIZE) + geom_hline(yintercept = 0) + 
      geom_bar(mapping = aes(x = as.Date(date_start), y = value/1e6, fill = variable), stat = "identity") +
      labs(x = NULL, y = "Objective Function ($/MWh)", title = "Incremental system costs from highway HFC deployment") +
      scale_fill_manual("", 
                        values = c("#9ecae1", "#3182bd", "#a1d99b", "#31a354"),
                        breaks = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"), 
                        labels = c("Local Effects - Economic", "Local Effects - Penalty", 
                                   "Zonal Effects - Economic", "Zonal Effects - Penalty")) +
      facet_wrap(~cap_mod)
    
    # Yearly
    plt1.4 <- ggplot(dt.plot1[, list(value = sum(value)), by = list(cap_mod, bat_mod, variable)]) + theme_bw(GGPLOT_SIZE) + geom_hline(yintercept = 0) + 
      geom_bar(mapping = aes(x = 1, y = value/sum(monthly.loads$monthly_load), fill = variable), stat = "identity") +
      labs(x = NULL, y = "Objective Function ($/MWh)", title = "Incremental system costs from highway HFC deployment") +
      scale_fill_manual("", 
                        values = c("#9ecae1", "#3182bd", "#a1d99b", "#31a354"),
                        breaks = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"), 
                        labels = c("Local Effects - Economic", "Local Effects - Penalty", 
                                   "Zonal Effects - Economic", "Zonal Effects - Penalty")) +
      facet_wrap(~cap_mod) + theme(strip.background = element_rect(fill = "white")) +
      theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  
  # Aggregate
  dt.plot <- dt.plot1[, list(value = sum(value)), by = list(month = format(stop, "%Y-%m-01"), cap_mod, bat_mod, variable)]
  
  
  # Pull monthly loads to use in normalizing the results
  if(metric == "cost") {
    monthly.loads <- scenarioYearStitch.getAreaDispatch(scn.to.load[["cap_mod 100%"]][["base"]])
    monthly.loads <- monthly.loads[ara != "0", list(monthly_load = sum(load)), by = list(month = format(as.Date(date), "%Y-%m-01"))]
    dt.plot <- merge(dt.plot, monthly.loads, by = "month")
    dt.plot[, value := value/monthly_load]
  }
  
  
  # Label cases 
  dt.plot[, cap_name := factorizeCapMod(cap_mod)]
  
  
  # Plot
  plt1.4 <- ggplot(dt.plot) + theme_bw(GGPLOT_SIZE) + 
    geom_bar(mapping = aes(x = as.Date(month), y = value, fill = variable), stat = "identity") +
    geom_hline(yintercept = 0) +
    facet_grid(.~cap_name) + theme(strip.background = element_rect(fill = "white")) +
    scale_x_date(date_labels = "%b", date_breaks = "3 months")
  
  if(metric == "cost") { 
    plt1.4 <- plt1.4 + 
      labs(x = NULL, y = "Incremental average cost ($/MWh)", title = "Incremental system costs from HFC deployment") +
      scale_fill_manual("", 
                        values = c("#9ecae1", "#3182bd", "#a1d99b", "#31a354"),
                        breaks = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"), 
                        labels = c("Local Effects - Economic", "Local Effects - Penalty", 
                                   "Zonal Effects - Economic", "Zonal Effects - Penalty"))
  } else if(metric == "emissions") {
    plt1.4 <- plt1.4 + 
      labs(x = NULL, y = "Incremental Tons CO2", title = "Incremental system emissions from HFC deployment") +
      scale_fill_manual("", 
                        values = c("#9ecae1", "#a1d99b"),
                        breaks = c("tons_co2_local", "tons_co2_transfer"), 
                        labels = c("Local Effects - Economic", "Zonal Effects - Economic"))
  } 
  
  return(plt1.4)
  
}
plotBatteryMitigation <- function(scn.list, metric = c("cost", "emissions")[1]) {
  
  # Get scenario information
  dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))
  dt.series <- merge(data.table(scn = unique(unlist(lapply(scn.list, function(x) unlist(x))))), 
                     dt.run.info[, list(id, scn, cap_mod, bat, bat_mod, xm_rlx, ltsa, fev, dr_amnt)], 
                     by = "scn")[, .SD[id == max(id)], by = scn]
  
  # Pull monthly loads to use in normalizing the results
  monthly.loads <- scenarioYearStitch.getAreaDispatch(scn.to.load[["cap_mod 100%"]][["base"]])
  monthly.loads <- monthly.loads[ara != "0", list(monthly_load = sum(load)), by = list(month = format(as.Date(date), "%Y-%m-01"))]
  
  # Calculate penalties and costs per scenario
  dt.opts2 <- rbindlist(lapply(X = scn.list, FUN = function(sequences) {
    
    if(metric == "cost") {
      dt.disp_0 <- scenarioYearStitch.getOptResultsAccurate(sequences[["disp_0"]])[, list(cost = sum(cost), pen = sum(pen), scn = sequences[["disp_0"]][1]), by = list(date_start)]
      
      names.conc <- names(sequences)[grepl("conc_", names(sequences))]
      dt.local <- rbindlist(lapply(names.conc, FUN = function(this.name) {
        dt.conc <- scenarioYearStitch.getOptResultsAccurate(sequences[[this.name]])[, list(cost = sum(cost), pen = sum(pen), scn = sequences[[this.name]][1]), by = list(date_start)]
        merge(dt.disp_0, dt.conc, by = "date_start", suffixes = c("_disp", "_conc"))[
          , list(cost_conc, cost_local = cost_conc - cost_disp, pen_conc, pen_local = pen_conc - pen_disp, date_start, scn_conc)]
      }))
    } else if(metric == "emissions") {
      dt.disp_0 <- scenarioYearStitch.getInjectorEmissions(sequences[["disp_0"]])
      
      names.conc <- names(sequences)[grepl("conc_", names(sequences))]
      
      dt.local <- rbindlist(lapply(names.conc, FUN = function(this.name) {
        dt.conc <- scenarioYearStitch.getInjectorEmissions(sequences[[this.name]])
        
        merge(dt.disp_0, dt.conc, by = c("int", "stop"), suffixes = c("_disp", "_conc"))[
          , list(tons_co2_local = tons_co2_conc - tons_co2_disp,
                 int, stop, scn_disp = sequences[["disp_0"]][1], scn_conc = sequences[[this.name]][1])]
        
        dt.base <- scenarioYearStitch.getInjectorEmissions(sequences[["base"]])
        dt.disp <- scenarioYearStitch.getInjectorEmissions(sequences[["disp_0"]])
        dt.conc <- scenarioYearStitch.getInjectorEmissions(sequences[["conc_0"]])
        
        dt.transfer <- merge(dt.base, dt.disp, by = c("int", "stop"), suffixes = c("_base", "_disp"))[
          , list(int, stop, tons_co2_transfer = tons_co2_disp - tons_co2_base, 
                 scn_base = sequences[["base"]][1], scn_disp = sequences[["disp_0"]][1], scn_conc = sequences[["conc_0"]][1])]
        dt.local <- merge(dt.disp, dt.conc, by = c("int", "stop"), suffixes = c("_disp", "_conc"))[
          , list(int, stop, tons_co2_local = tons_co2_conc - tons_co2_disp)]
        
        dt.return <- merge(dt.transfer, dt.local, by = c("int", "stop"))
        
      }))
    }
    
    return(dt.local)
  }))
  
  dt.plot2 <- merge(dt.opts2, dt.series, by.x = "scn_conc", by.y = "scn")
  dt.props <- rbindlist(lapply(X = unique(dt.plot2$scn_conc), FUN = function(this.scn) {
    x <- getInjectorInfoTimeInvariant(scenario = this.scn, omit.node.data = T, bln.fix.missing.nodes = F)[, sum(max), by = type]
    data.table(scn = this.scn, prop = x[type == "Battery", V1]/x[type == "Charger", V1])
  }))
  
  # Plot
  dt.plot2 <- merge(dt.plot2, dt.props, by.x = "scn_conc", by.y = "scn")[is.na(prop), prop := 0]
  if(metric == "cost") {
    dt.plot2 <- dt.plot2[, list(cost_local = sum(cost_local), pen_local = sum(pen_local)), by = list(cap_mod, bat_mod, prop)]
  } else {
    dt.plot2 <- dt.plot2[, list(tons_co2_local = sum(tons_co2_local)), by = list(cap_mod, bat_mod, prop)]
  }
  dt.plot2 <- melt(dt.plot2, id.vars = c("cap_mod", "bat_mod", "prop"))
  dt.plot2[, variable := factor(variable, levels = c("tons_co2_local", "cost_transfer", "cost_local", "pen_transfer", "pen_local"))]
  dt.plot2[, cap_name := factorizeCapMod(cap_mod)]
  
  if(metric == "cost") {
    # Yearly, normalized by base annual load
    plt2.4 <- ggplot(dt.plot2) + theme_bw(GGPLOT_SIZE) + 
      geom_bar(mapping = aes(x = factor(prop), y = value/sum(monthly.loads$monthly_load), fill = factor(variable)),
               position = position_stack(), stat = "identity") +
      geom_hline(yintercept = 0) + 
      labs(y = "Incremental average cost ($/MWh)", title = "Energy Storage Mitigation") +
      scale_fill_manual("",
                        values = c("#9ecae1", "#3182bd", "#a1d99b", "#31a354"),
                        breaks = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"),
                        labels = c("Local Effects - Economic", "Local Effects - Penalty",
                                   "Zonal Effects - Economic", "Zonal Effects - Penalty")) +
      scale_x_discrete(name = "Storage Nameplate as % of HFC Nameplate", 
                       label = function(x) paste0(round(as.numeric(x)*100,1), "%")) +
      facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))
  } else {
  
  plt2.4 <- ggplot(dt.plot2) + theme_bw(GGPLOT_SIZE) + 
    geom_bar(mapping = aes(x = factor(prop), y = value, fill = factor(variable)), 
             position = position_stack(), stat = "identity") +
    geom_hline(yintercept = 0) + 
    labs(y = "Incremental Tons CO2", title = "Energy Storage Mitigation") +
    scale_x_discrete(name = "Storage Nameplate as % of HFC Nameplate", 
                     label = function(x) paste0(round(as.numeric(x)*100,1), "%")) +
    facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))
  }
  
  
  return(plt2.4)
}
plotFlexibilityMitigation <- function(scn.list) {
  
  # Get scenario information
  dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))
  dt.series <- merge(data.table(scn = unique(unlist(lapply(scn.list, function(x) unlist(x))))), 
                     dt.run.info[, list(id, scn, cap_mod, bat, bat_mod, xm_rlx, ltsa, fev, dr_amnt)], 
                     by = "scn")[, .SD[id == max(id)], by = scn]
  
  # Pull monthly loads to use in normalizing the results
  monthly.loads <- scenarioYearStitch.getAreaDispatch(scn.to.load[["cap_mod 100%"]][["base"]])
  monthly.loads <- monthly.loads[ara != "0", list(monthly_load = sum(load)), by = list(month = format(as.Date(date), "%Y-%m-01"))]
  
  # Calculate penalties and costs per scenario
  dt.opts4 <- rbindlist(lapply(X = scn.list, FUN = function(sequences) {
    
    dt.disp_0 <- scenarioYearStitch.getOptResultsAccurate(sequences[["disp_0"]])[, list(cost = sum(cost), pen = sum(pen), scn = sequences[["disp_0"]][1]), by = list(date_start)]
    
    dt.dr_0 <- scenarioYearStitch.getOptResultsAccurate(sequences[["conc_0"]])[, list(cost = sum(cost), pen = sum(pen), scn = sequences[["conc_0"]][1]), by = list(date_start)]
    dt.local_0 <- merge(dt.disp_0, dt.dr_0, by = "date_start", suffixes = c("_disp", "_conc"))[
      , list(cost_conc, cost_local = cost_conc - cost_disp, pen_conc, pen_local = pen_conc - pen_disp, date_start, scn_conc)]
    
    names.dr <- names(sequences)[grepl("dr_", names(sequences))]
    dt.local <- rbindlist(lapply(names.dr, FUN = function(this.name) {
      dt.dr <- scenarioYearStitch.getOptResultsAccurate(sequences[[this.name]])[, list(cost = sum(cost), pen = sum(pen), scn = sequences[[this.name]][1]), by = list(date_start)]
      merge(dt.disp_0, dt.dr, by = "date_start", suffixes = c("_disp", "_conc"))[
        , list(cost_conc, cost_local = cost_conc - cost_disp, pen_conc, pen_local = pen_conc - pen_disp, date_start, scn_conc)]
    }))
    
    dt.return <- rbind(rbind(dt.local_0, dt.local))
    
    return(dt.return)
  }))
  
  # Plot
  dt.plot4 <- merge(dt.opts4, dt.series, by.x = "scn_conc", by.y = "scn")
  dt.plot4 <- dt.plot4[, list(cost_local = sum(cost_local), pen_local = sum(pen_local)), by = list(cap_mod, dr_amnt)]
  dt.plot4 <- melt(dt.plot4, id.vars = c("cap_mod", "dr_amnt"))
  dt.plot4[, variable := factor(variable, levels = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"))]
  dt.plot4[, cap_name := factorizeCapMod(cap_mod)]
  
  plt4.4 <- ggplot(dt.plot4) + theme_bw(GGPLOT_SIZE) + 
    geom_bar(mapping = aes(x = factor(dr_amnt), y = value/sum(monthly.loads$monthly_load), fill = factor(variable)), 
             position = position_stack(), stat = "identity") +
    geom_hline(yintercept = 0) + 
    labs(y = "Incremental average cost ($/MWh)", title = "Demand Flexibility") +
    scale_fill_manual("", 
                      values = c("#9ecae1", "#3182bd", "#a1d99b", "#31a354"),
                      breaks = c("cost_local", "pen_local", "cost_transfer", "pen_transfer"), 
                      labels = c("Local Effects - Economic", "Local Effects - Penalty", 
                                 "Zonal Effects - Economic", "Zonal Effects - Penalty")) +
    scale_x_discrete(name = "Flexible Demand as % of HFC Nameplate", 
                     label = function(x) paste0(round(as.numeric(x)*100,1), "%")) +
    facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))
  
  return(plt4.4)
}
plotStorageCostBenefit <- function(scn.list) {
  
  # Get scenario information
  dt.run.info <- fread(paste0(PATH.INPUTS, "run_info.csv"))
  dt.series <- merge(data.table(scn = unique(unlist(lapply(scn.list, function(x) unlist(x))))), 
                     dt.run.info[, list(id, scn, cap_mod, bat, bat_mod, xm_rlx, ltsa, fev, dr_amnt)], 
                     by = "scn")[, .SD[id == max(id)], by = scn]
  
  # Calculate incremental system impacts of storage
  storage.scenarios <- unlist(lapply(X = scn.list, FUN = function(sequences) {
    sequences[grepl("conc", names(sequences))]
  }), recursive = F)
  # storage.scenarios[[length(storage.scenarios)+1]] = c("Scn_000906", "Scn_000907")
  dt.bat.seq <- rbindlist(lapply(X = storage.scenarios, FUN = function(these.scn) {
    scenarioYearStitch.getOptResultsAccurate(vector.scenarios = these.scn)[
      , list(scn = these.scn[1], cost, pen, total = cost + pen, date_start, 
             cap = getInjectorInfoTimeInvariant(scenario = these.scn[1], omit.node.data = T, bln.fix.missing.nodes = F)[type == "Battery", sum(max)])]
  }))
  dt.bat.seq <- merge(dt.bat.seq, dt.series[, list(scn, cap_mod)], by = "scn")
  dt.bat.seq <- dt.bat.seq[, list(cost = sum(cost), pen = sum(pen), total = sum(total)), by = list(scn, cap_mod, cap, date_start)]
  # dt.bat.seq[cap_mod == 0, run_type := "bat_only"]
  # dt.bat.seq[cap_mod != 0, run_type := "bat_and_ev"]
  # dt.bat.seq <- merge(copy(dt.bat.seq)[cap_mod == 0, cap_mod := 1], 
  dt.bat.seq <- merge(dt.bat.seq,
                      dt.bat.seq[cap == 0, list(date_start, cap_mod, cost_base = cost, pen_base = pen, total_base = total)], 
                      by = c("date_start", "cap_mod"))
  dt.bat.seq[, `:=`(cost = cost - cost_base, pen = pen - pen_base, total = total - total_base)]
  
  dt.props <- rbindlist(lapply(X = unlist(storage.scenarios), FUN = function(this.scn) {
    x <- getInjectorInfoTimeInvariant(scenario = this.scn, omit.node.data = T, bln.fix.missing.nodes = F)[, sum(max), by = type]
    data.table(scn = this.scn, prop = x[type == "Battery", V1]/x[type == "Charger", V1])
  }))
  # dt.props[prop == Inf, prop := 0.024529625]
  dt.props[is.na(prop), prop := 0]
  dt.bat.seq <- merge(dt.bat.seq, dt.props, by = "scn")
  
  # dt.total.diffs <- dt.bat.seq[, list(total = sum(total), cost = sum(cost), pen = sum(pen)), by = list(mw_bat = cap, cap_mod, prop, run_type)]
  dt.total.diffs <- dt.bat.seq[, list(total = sum(total), cost = sum(cost), pen = sum(pen)), by = list(mw_bat = cap, cap_mod, prop)]
  dt.total.diffs <- melt(dt.total.diffs, id.vars = c("mw_bat", "cap_mod", "prop"), variable.name = "value_type", value.name = 'total')
  # dt.total.diffs <- melt(dt.total.diffs, id.vars = c("mw_bat", "cap_mod", "prop", "run_type"), variable.name = "value_type", value.name = 'total')
  dt.total.diffs[, prop := paste0(round(prop, 3)*100, "%")]
  dt.total.diffs[, cap_name := ifelse(cap_mod == .75, "Low Pen.", ifelse(cap_mod == 1.00, "Base EV Penetration", ifelse(cap_mod == 1.25, "High Pen.", NA)))]
  dt.total.diffs[, cap_name := factor(cap_name, levels = c("Low Pen.", "Base EV Penetration", "High Pen."))]
  
  if(F) {
    # Pure, annual value
    ggplot(dt.total.diffs[value_type != "total"]) +  theme_bw(GGPLOT_SIZE) + geom_hline(yintercept = 0) +
      geom_bar(mapping = aes(x = factor(prop), y = -total/mw_bat/1e6, fill = value_type),
               position = position_stack(), stat = "identity") +
      facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))+
      labs(x = "Battery size relative to HFC system", y = "System Value ($/W)", title = "Total Annual Value of Storage") +
      scale_fill_manual("Mitigation", values = c("#9ecae1", "#3182bd"), breaks = c("cost", "pen"), labels = c("Developer\nCapture", "Grid\nBenefits"))
    
    ggplot(dt.total.diffs[value_type == "cost"]) +  theme_bw(GGPLOT_SIZE) + geom_hline(yintercept = 0) +
      geom_bar(mapping = aes(x = factor(prop), y = -total/mw_bat/1e6, fill = value_type),
               position = position_stack(), stat = "identity") +
      facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))+
      labs(x = "Battery size relative to HFC system", y = "System Value ($/W)", title = "Total Annual Value of Storage") +
      scale_fill_manual("Mitigation", values = c("#9ecae1", "#3182bd"), breaks = c("cost", "pen"), labels = c("Developer\nCapture", "Grid\nBenefits"))
  }
  
  # From Cole and Frazier 2020, using the 2030 estimates
  CAP_LOW <- 144 # $/kWh
  CAP_MED <- 208 # $/kWh
  CAP_HIGH <- 293 # $/kWh
  OPEX <- 39 # $/kW-yr
  LIFETIME <- 15 # years
  DISCOUNT_RATE <- 1.07
  
  LCOE_HIGH <- 1.8 # $/W, from Lazard LCOS
  LCOE_LOW <- 0.9 # $/W, from Lazard LCOS
  
  # Lifetime value, discounted to installation date (all at once)
  dt.discounted.diffs <- merge(cbind(join = "a", dt.total.diffs), data.table(join = "a", year = 1:LIFETIME), by = "join", allow.cartesian = T)[, join := NULL]
  dt.discounted.diffs[, discounted := total*(1/DISCOUNT_RATE)^(year-1)]
  dt.discounted.diffs <- dt.discounted.diffs[, list(discounted = sum(discounted), undiscounted = sum(total), annual_value = unique(total)), 
                                             by = list(mw_bat, cap_mod, prop, value_type, cap_name)]
  
  plt5.1 <- ggplot(dt.discounted.diffs[!is.na(cap_name)][value_type != "total"]) +  theme_bw(GGPLOT_SIZE) + 
    geom_bar(mapping = aes(x = factor(prop), y = -discounted/mw_bat/1e6, fill = value_type),
             position = position_stack(), stat = "identity") +
    geom_hline(yintercept = 0) + 
    facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))+
    labs(x = "Battery size relative to HFC system", y = "Value ($/W)", title = "Total Lifetime Value of Storage") +
    scale_fill_manual("Value Type", values = c("#9ecae1", "#3182bd"), breaks = c("cost", "pen"), labels = c("Developer\nCapture", "Grid\nBenefits"))
  
  plt5.2 <- ggplot(dt.discounted.diffs[!is.na(cap_name)][value_type == "cost"]) +  theme_bw(GGPLOT_SIZE) + 
    geom_bar(mapping = aes(x = factor(prop), y = -discounted/mw_bat/1e6, fill = value_type), 
             position = position_stack(), stat = "identity", fill = "#9ecae1") +
    geom_hline(yintercept = 0) + 
    facet_wrap(~cap_name) + theme(strip.background = element_rect(fill = "white"))+
    labs(x = "Battery size relative to HFC system", y = "Value ($/W)", title = "Total Lifetime Value of Storage to Developer") +
    # scale_fill_manual("Mitigation", values = c("#9ecae1", "#3182bd"), breaks = c("cost", "pen"), labels = c("Developer\nCapture", "Grid\nBenefits")) +
    geom_hline(mapping = aes(yintercept = LCOE_LOW, color = "Wholesale --- Low")) +
    geom_hline(mapping = aes(yintercept = LCOE_HIGH, color = "Wholesale --- High")) +
    scale_color_brewer(name = "Cost estimates", type = "qual", palette = 6)
  # plt5.1
  # plt5.2
  
  dt.total.diffs[, cap_name := ifelse(cap_mod == .75, "Low Pen.", ifelse(cap_mod == 1.00, "Base Pen.", ifelse(cap_mod == 1.25, "High Pen.", ifelse(cap_mod == 0, "No EVs", NA))))]
  dt.total.diffs[, cap_name := factor(cap_name, levels = c("No EVs", "Low Pen.", "Base Pen.", "High Pen."))]
  dt.discounted.diffs <- merge(cbind(join = "a", dt.total.diffs), data.table(join = "a", year = 1:LIFETIME), by = "join", allow.cartesian = T)[, join := NULL]
  dt.discounted.diffs[, discounted := total*(1/DISCOUNT_RATE)^(year-1)]
  dt.discounted.diffs <- dt.discounted.diffs[, list(discounted = sum(discounted), undiscounted = sum(total), annual_value = unique(total)), 
                                             by = list(mw_bat, cap_mod, prop, value_type, cap_name)]
  plt5.3 <- ggplot(dt.discounted.diffs[value_type == "cost"]) +  theme_bw(GGPLOT_SIZE) + 
    geom_line(mapping = aes(x = mw_bat, y = -discounted/mw_bat/1e6, color = cap_name, group = cap_name),
              size = 1) +
    geom_point(mapping = aes(x = mw_bat, y = -discounted/mw_bat/1e6, color = cap_name, group = cap_name),
               size = 2) +
    theme(strip.background = element_rect(fill = "white"))+
    labs(x = "System Battery Capacity (MW)", y = "Value ($/W)", title = "Total Lifetime Value of Storage to Developer") +
    geom_hline(yintercept = 0) +
    geom_hline(mapping = aes(yintercept = LCOE_LOW), linetype = "dashed") +
    geom_text(mapping = aes(x = 0, y = LCOE_LOW, label = "Cost, High"), hjust = "inward", nudge_y = +.25) +
    geom_hline(mapping = aes(yintercept = LCOE_HIGH), linetype = "dashed") +
    geom_text(mapping = aes(x = 0, y = LCOE_HIGH, label = "Cost, Low"), hjust = "inward", nudge_y = +.25) +
    scale_color_brewer(NULL, type = "qual", palette = 6)
  # plt5.3
  
  if(F) {
    # Now take a look at what the revenue to the batteries is. Take just one case to start, the 2.5% in the base case
    dt.injectors <- scenarioYearStitch.getInjectorInfo(paste0("Scn_", c("000936", "000937")))
    # dt.injectors <- scenarioYearStitch.getInjectorInfo(paste0("Scn_", c("000906", "000907")))
    dt.batteries <- dt.injectors[[2]][type == "Battery"]
    dt.battery.ops <- dt.injectors[[1]][inj %in% dt.batteries$inj]
    dt.battery.ops <- dt.battery.ops[, list(int, stop, p, lmp, revenue = p*lmp, cum_revenue = cumsum(p*lmp)), by = list(inj)]
    
    ggplot(dt.battery.ops) + geom_line(mapping = aes(x = stop, y = cum_revenue/1e6, color = inj)) + 
      labs(x = NULL, y = "Cum Revenue ($M)", title = "Cumulative revenue for each battery system")
    
    ggplot(rbind(dt.total.diffs[cap_mod == 1 & prop == "2.5%", list(mw_bat, cap_mod, value_type, total = -total)],
                 data.table(mw_bat = 125.94, cap_mod = 1, value_type = "bat_rev", total = dt.battery.ops[, sum(revenue)]))) +
      geom_bar(mapping = aes(x = value_type, y = total/1e6), stat = "identity") +
      labs(x = NULL, y = "Value ($M)", title = "Annual values of storage, and battery revenue")
    
    # When is storage value (to the system) happening?
    dt.monthly <- dt.bat.seq[, list(total = sum(total), cost = sum(cost), pen = sum(pen)), by = list(mw_bat = cap, cap_mod, prop, week = week(date_start))]
    dt.monthly <- melt(dt.monthly, id.vars = c("week", "cap_mod", "mw_bat", "prop"), measure.vars = c("total", "cost", "pen"))
    dt.monthly[, variable := factor(variable, levels = c("total", "pen", "cost"))]
    ggplot(dt.monthly[cap_mod == 1 & round(prop, 3) == round(max(prop), 3) & variable != "total"]) + theme_bw(GGPLOT_SIZE) + 
      geom_bar(mapping = aes(x = week, y = value/1e6, fill = variable), position = "stack", stat = "identity") +
      labs(x = "Week of Year", y = "Increase in System Cost ($M)", title = "Annual value of batteries") +
      geom_hline(yintercept = 0) +
      scale_fill_brewer(type = "qual", palette = 6)
    
  }
  
  return(list(plt5.1, plt5.2, plt5.3))

}

if(F) {
  GGPLOT_SIZE = 11
  
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
  
  plt1.4 <- plotChargerImpacts(scn.list = scn.to.load[2:6], "cost")
  plt2.4 <- plotBatteryMitigation(scn.list = scn.to.load[3:5])
  plt4.4 <- plotFlexibilityMitigation(scn.list = scn.to.load[3:5])
  
  plt5.3 <- plotStorageCostBenefit(scn.list = scn.to.load[c(1,3,4,5)])[[3]]
  
  plotChargerImpacts(scn.list = scn.to.load[2:6], "emissions")
  plotBatteryMitigation(scn.list = scn.to.load[c(3,4)])
  plotFlexibilityMitigation(scn.list = scn.to.load[3:5])
  
  plt5.3 <- plotStorageCostBenefit(scn.list = scn.to.load[c(1,3,4,5)])[[3]]
}

