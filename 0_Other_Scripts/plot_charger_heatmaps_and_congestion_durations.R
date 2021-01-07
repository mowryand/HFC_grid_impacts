# Creates a series of diagnostic charts for output

##### LIBRARIES #####
library(data.table)
library(ggplot2)
library(viridis)
library(patchwork)

##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

##### PRE-PROCESSING #####
if(F) {
  # scenario <- c("Scn_013")
  scenario <- c("Scn_009", "Scn_002")
  
  stopifnot(length(scenario) %in% c(1, 2))
  if(length(scenario) == 2) {
    dt.ed <- getAreaDispatchDiff(scenario = scenario[c(1, 2)])
    lst.inj <- getInjectorInfoDiff(scenario = scenario[c(1, 2)])
  } else {
    dt.ed <- getAreaDispatch(scenario, interval.map = NULL)
    lst.inj <- getInjectorInfo(scenario, interval.map = NULL)
  }
  dt.inj <- merge(lst.inj[[1]], lst.inj[[2]], by = "inj")
}

if(F) {
  lst.inj <- scenarioYearStitch.getInjectorInfo(vector.scenarios = c("Scn_000980", "Scn_000981")) # 100% concentrated no mitigation
  dt.inj <- merge(lst.inj[[1]], lst.inj[[2]], by = "inj")
}


### day vs time price heatmap for individual chargers
plotHeatsandDurations <- function(top.nth, bln.omit.title = F, these.inj = NA, dt.inj = NULL, scenario = NULL) {
  
  if(is.null(dt.inj)) {
    stopifnot(length(scenario) %in% c(1, 2))
    ##### PRE-PROCESSING #####
    if(length(scenario) == 2) {
      dt.ed <- getAreaDispatchDiff(scenario = scenario[c(1, 2)])
      lst.inj <- getInjectorInfoDiff(scenario = scenario[c(1, 2)])
    } else {
      dt.ed <- getAreaDispatch(scenario, interval.map = NULL)
      lst.inj <- getInjectorInfo(scenario, interval.map = NULL)
    }
    dt.inj <- merge(lst.inj[[1]], lst.inj[[2]], by = "inj")
  }
  
  if(!is.null(top.nth)) {
    these.inj <- dt.inj[grepl("chg", inj)][, list(mlmp = max(abs(lmp))), by = inj][order(mlmp, decreasing = T)][1:top.nth, inj]
  } else {
    these.inj <- dt.inj[grepl("chg", inj), unique(inj)]
  }
  
  lst.res <- lapply(X = these.inj, FUN = function(this.inj, omit.title = bln.omit.title) {
    
    dt.plot <- dt.inj[inj %in% this.inj, list(inj, stop, lmp, p,
                                              date = format(stop, "%Y-%m-%d", tz = "ETC/GMT+6"), 
                                              he = format(stop, "%H", tz = "ETC/GMT+6"))]
    
    dt.plot[, date := as.Date(date, tz = "ETC/GMT+6")]
    dt.plot[he == "00", `:=`(date = date - 1, he = "24")]
    dt.plot[, he := as.integer(he)]
    dt.plot[, day := ifelse(wday(date) %in% 2:6, "weekday", "weekend")]
    
    dt.plot[, type := ifelse(grepl("chg", inj), "chg", "bat")]
    dt.plot[, node := gsub("_.*", "", inj)]
    dt.plot <- merge(unique(dt.plot[, list(node, stop, date, he, lmp, day)]),
                     dcast(dt.plot, node + stop ~ type, value.var = "p"),
                     by = c("node", "stop"), all = T)
    
    dt.plot.norm.ind <- copy(dt.plot)[, `:=`(lmp = lmp/max(abs(lmp)), 
                                             chg = chg/max(abs(chg))), by = node] # Show dynamic range per node
    
    # LMP Heat maps
    if(T) plt1.1 <- ggplot(dt.plot) + theme_bw(12) +
      geom_raster(mapping = aes(x = he, y = date, fill = lmp)) +
      scale_fill_viridis("d(LMP)") + 
      labs(x = "HE", y = "", title = paste0(this.inj, "; ", paste0(scenario, collapse = "->"))) +
      facet_wrap(~node)
    if(T) plt1.2 <- ggplot(dt.plot.norm.ind) + theme_bw(12) +
      geom_raster(mapping = aes(x = he, y = date, fill = lmp)) +
      scale_fill_viridis("NORM(d(LMP))") + 
      labs(x = "HE", y = "", title = paste0(this.inj, "; ", paste0(scenario, collapse = "->"))) +
      facet_wrap(~node)
    
    # LMP measure: Full Width at QUARTER Maximum
    dt.fwhm <- dt.plot[, list(lmp = mean(lmp)), by = list(node, he)]
    # ggplot(dt.fwhm) + geom_line(mapping = aes(x = he, y = lmp)) + facet_wrap(~node)
    # threshold <- (max(dt.fwhm$lmp) - mean(dt.fwhm$lmp))/4 + mean(dt.fwhm$lmp)
    threshold <- (max(dt.fwhm$lmp) - min(dt.fwhm$lmp))/4 + min(dt.fwhm$lmp)
    dt.bounds <- dt.fwhm[lmp >= threshold, list(begin = min(he) - .5, end = max(he) + .5)]
    
    plt2 <- ggplot(dt.fwhm) + theme_bw(12) + geom_hline(yintercept = 0) +
      geom_line(mapping = aes(x = he, y = lmp)) + 
      geom_hline(yintercept = threshold, color = "red", size = 1) +
      geom_linerange(mapping = aes(y = threshold, xmin = dt.bounds$begin, xmax = dt.bounds$end), color = "red", size = 2) +
      labs(x = "Hour of Day", y = "d(LMP) ($/MWh)") + annotate("text", label = this.inj, x = 0, y = max(dt.fwhm$lmp), hjust = 0, vjust = 1)
    if(!omit.title) plt2 <- plt2 + labs(title = paste0(this.inj, "; ", paste0(scenario, collapse = "->")))
    
    duration <- dt.bounds[, end - begin]
    
    return(list(plt1.1, plt1.2, plt2, duration))
  })
  
  
  plt.hm1 <- lapply(lst.res, function(x) `[[`(x, 1))
  plt.hm2 <- lapply(lst.res, function(x) `[[`(x, 2))
  plt.exp <- lapply(lst.res, function(x) `[[`(x, 3))
  dur <- sapply(lst.res, function(x) `[[`(x, 4))
  
  plt1 <- wrap_plots(plt.hm1, nrow = 2)
  plt2 <- wrap_plots(plt.hm2, nrow = 2)
  plt3 <- wrap_plots(plt.exp, nrow = 2)
  dt.dur <- data.table(inj = these.inj, dur = dur)

  return(list(plt1, plt2, plt3, dt.dur, plt.hm1, plt.hm2, plt.exp))
  
}

# Faceted charts of congestion durations
plotDurations2 <- function(dt.inj, top.nth = NULL, these.inj = NULL, bln.omit.title = F) {
  
  if(!is.null(top.nth)) {
    these.inj <- dt.inj[grepl("chg", inj)][, list(mlmp = max(abs(lmp))), by = inj][order(mlmp, decreasing = T)][1:top.nth, inj]
  } 
  
  dt.plot <- dt.inj[inj %in% these.inj, list(inj, stop, lmp, p,
                                             date = format(stop, "%Y-%m-%d", tz = "ETC/GMT+6"), 
                                             he = format(stop, "%H", tz = "ETC/GMT+6"))]
  
  dt.plot[, date := as.Date(date, tz = "ETC/GMT+6")]
  dt.plot[he == "00", `:=`(date = date - 1, he = "24")]
  dt.plot[, he := as.integer(he)]
  dt.plot[, day := ifelse(wday(date) %in% 2:6, "weekday", "weekend")]
  
  dt.plot[, type := ifelse(grepl("chg", inj), "chg", "bat")]
  dt.plot[, node := gsub("_.*", "", inj)]
  
  dt.plot <- merge(unique(dt.plot[, list(node, stop, date, he, lmp, day)]),
                   dcast(dt.plot, node + stop ~ type, value.var = "p"),
                   by = c("node", "stop"), all = T)
  
  dt.plot.norm.ind <- copy(dt.plot)[, `:=`(lmp = lmp/max(abs(lmp)), 
                                           chg = chg/max(abs(chg))), by = node] # Show dynamic range per node
  
  # plt1 <- ggplot(dt.plot.norm.ind) + theme_bw(12) +
  #   geom_raster(mapping = aes(x = he, y = date, fill = lmp)) +
  #   scale_fill_viridis("NORM(d(LMP))") + 
  #   labs(x = "HE", y = "", title = paste0(this.inj, "; ", paste0(scenario, collapse = "->"))) +
  #   facet_wrap(~node)
  
  # LMP measure: Full Width at HALF Maximum
  CONSTANT = 2
  dt.fwhm <- dt.plot[, list(lmp = mean(lmp)), by = list(node, he)] # average hourly LMPs by node
  dt.fwhm <- dt.fwhm[, list(lmp = lmp/max(lmp), he), by = list(node)] # then normalize
  
  dt.fwhm[, node_name := names(these.inj)[match(node, as.integer(tstrsplit(these.inj, "_")[[1]]))]]
  
  dt.threshold <- dt.fwhm[, list(threshold = (max(lmp) - min(lmp))/CONSTANT + min(lmp)), by = list(node)] # Get thresholds by node
  dt.threshold <- merge(dt.fwhm, dt.threshold, by = "node")
  dt.threshold <- dt.threshold[lmp >= threshold, list(begin = min(he) - .5, end = max(he) + .5), by = list(node, node_name, threshold)]
  
  plt.return <- ggplot() + theme_bw(12) + geom_hline(yintercept = 0) +
    geom_line(data = dt.fwhm, mapping = aes(x = he, y = lmp)) + 
    geom_linerange(data = dt.threshold, mapping = aes(y = threshold, xmin = 0, xmax = 25), color = "red", size = 1) +
    geom_linerange(data = dt.threshold, mapping = aes(y = threshold, xmin = begin, xmax = end), color = "red", size = 2) +
    labs(x = "Hour of Day", y = "Normalized LMP", title = "Characteristic congestion event durations at HFCs") + 
    facet_wrap(~paste0("HFC ", node_name))
  
  return(plt.return)
}
