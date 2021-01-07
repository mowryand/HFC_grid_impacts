# Plotting charger LMP distribution lineups

##### LIBRARIES #####
library(data.table)
library(ggplot2)
library(viridis)


##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

boxplots.getData <- function(scn.to.load) {
  
  lst.res.inj <- lapply(names(scn.to.load), FUN = function(this.case) {
    lst.ret <- scenarioYearStitch.getInjectorInfo(vector.scenarios = scn.to.load[[this.case]])
    
    dt.frame <- CJ("stop" = lst.ret[[1]][, unique(stop)], "inj" = lst.ret[[2]][type == "Charger", unique(inj)])
    dt.ret <- merge(dt.frame,
                    lst.ret[[2]][type == "Charger", list(inj, area)],
                    by = "inj")
    
    dt.ret <- merge(dt.ret,
                    lst.ret[[1]][, list(inj, lmp, p, stop, int)],
                    by = c("inj", "stop"), all.x = T)
    dt.ret[, scn := this.case]
    
    # If there are NA LMPs, it's because chargers weren't added to injector tables. Look for nodes in PC_Nd
    if(any(is.na(dt.ret$lmp))) {
      # dt.missing <- getInjectorInfo("Scn_001", interval.map = NULL, bln.fix.missing.nodes = F)[[1]] 
      dt.missing <- scenarioYearStitch.getPC_Nd(vector.scenarios = scn.to.load[[this.case]])
      dt.missing <- dt.missing[, list(inj = paste0(nd, "_chg"), stop, lmp_replace = LMP)]
      dt.ret <- merge(dt.ret, dt.missing, by = c("inj", "stop"), all.x = T)
      dt.ret[is.na(lmp), lmp := lmp_replace]
    }
    
    return(dt.ret[, list(scn, inj, area, int, stop, lmp, p)])
  })
  
  dt.lmps <- dcast(rbindlist(lst.res.inj), inj + area + stop ~ scn, value.var = "lmp")
  dt.lmps[, Scn_dif := `to` - `from`]
  dt.lmps2 <- dcast(rbindlist(lst.res.inj), inj + area + stop ~ scn, value.var = "p")
  dt.lmps2[, Scn_dif := `to` - `from`]
  dt.lmps <- merge(dt.lmps[, list(inj, area, stop, lmp_dif = Scn_dif)], 
                   dt.lmps2[, list(inj, area, stop, p_to = to, p_dif = Scn_dif)], 
                   by = c("inj", "area", "stop"))
  dt.lmps <- melt(dt.lmps, id.vars = c("inj", "area", "stop"))
  
  # dt.lmps[, scn := ifelse(scn == "from", "Base", ifelse(scn == "to", "Sim.", "Dif."))]
  # dt.lmps[, scn := factor(scn, levels = c("Base", "Sim.", "Dif."), ordered = T)]
  
  return(dt.lmps[])
}
boxplots.plotChargers <- function(scn.to.load, suppress.title = F, monthly = F) {
  
  dt.lmps <- boxplots.getData(scn.to.load)
  
  # Boxplot lineup showing two base LMPs and difference
  # plt1 <- ggplot(dt.lmps) + theme_bw(12) + 
  #   geom_boxplot(mapping = aes(x = inj, y = lmp, group = interaction(scn, inj), fill = scn),
  #                position = position_dodge()) +
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #   labs(x = "Charging Station", y = "Hourly LMP ($/MWh)") +
  #   scale_fill_brewer("", type = "qual", palette = 6) +
  #   facet_grid(.~area, scales = "free_x", space = "free_x")
  # if(!suppress.title) plt1 <- plt1 + ggtitle(paste0(names(scn.to.load), collapse = "->"))
  
  # Boxplot lineup of chargers with vanilla LMP diff
  # vec.levels <- dt.lmps[scn == "Dif.", list(mlmp = mean(lmp)), by = inj][order(mlmp, decreasing = F)][, i := 1:.N]$inj
  # dt.plot <- dt.lmps[scn == "Dif."][, inj := factor(x = inj, levels = vec.levels)]
  # plt2 <- ggplot(dt.plot) + theme_bw(12) + 
  #   geom_boxplot(mapping = aes(x = inj, y = lmp/1e3, group = inj, fill = area), alpha = .8) + 
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #   labs(x = "Charging Station", y = "d(Hourly LMP) ($K/MWh)") +
  #   scale_fill_discrete("")
  # if(!suppress.title) plt2 <- plt2 + ggtitle(paste0(names(scn.to.load), collapse = "->"))
  
  # Ordered boxplot lineup of chargers according to mean LMP experienced
  # vec.levels <- dt.lmps[scn == "Dif.", list(mlmp = mean(abs(lmp))), by = inj][order(mlmp, decreasing = F)][, i := 1:.N]$inj
  # dt.plot <- dt.lmps[scn == "Dif."][, inj := factor(x = inj, levels = vec.levels)]
  # plt3 <- ggplot(dt.plot) + theme_bw(12) + 
  #   geom_boxplot(mapping = aes(x = inj, y = abs(lmp), group = inj, fill = area)) + 
  #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  #   labs(x = "Charging Station", y = "abs(d(Hourly LMP)) ($/MWh)") +
  #   scale_y_log10() + scale_fill_discrete("") +
  #   facet_wrap( ~ ifelse(lmp > 0, "Positive Diff", "Negative Diff")) ##### OPTIONAL
  # if(!suppress.title) plt3 <- plt3 + ggtitle(paste0(names(scn.to.load), collapse = "->"))
  
  # Violins
  vec.levels <- dt.lmps[variable == "lmp_dif", list(mlmp = mean(abs(value))), by = inj][order(mlmp, decreasing = F)][, i := 1:.N]$inj
  dt.plot <- dt.lmps[variable == "lmp_dif"][, inj := factor(x = inj, levels = vec.levels)]
  dt.plot[, mnth := month(stop)]
  plt4 <- ggplot(dt.plot) + theme_bw(12) + 
    geom_violin(mapping = aes(x = inj, y = abs(value), group = inj, fill = area), draw_quantiles = .5) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Charging Station", y = "abs(d(Hourly LMP)) ($/MWh)") +
    scale_y_log10() + scale_fill_discrete("")
  if(monthly) {
    plt4 <- plt4 + 
      geom_point(data = dt.plot[, list(avg_lmp = mean(abs(value))), by = list(inj, mnth)], mapping = aes(x = inj, y = avg_lmp))
    plt4 <- plt4 + facet_wrap(~mnth, nrow = 3)
  } else {
    plt4 <- plt4 + 
      geom_point(data = dt.plot[, list(avg_lmp = mean(abs(value))), by = list(inj)], mapping = aes(x = inj, y = avg_lmp))
  }
  # facet_wrap( ~ ifelse(lmp > 0, "Positive Diff", "Negative Diff")) ##### OPTIONAL
  if(!suppress.title) plt4 <- plt4 + ggtitle(paste0(names(scn.to.load), collapse = "->"))
  
  return(list(plt1, plt2, plt3, plt4))
}
boxplots.plotSystem <- function(scn.to.load) {
  
  dt.ed <- getAreaDispatchDiff(scenario = scn.to.load)
  plt5 <- ggplot(dt.ed[ara == 0]) + theme_bw(12) + 
    geom_boxplot(mapping = aes(x = 1, y = sourceprice/1e3), fill = "grey", alpha = .8) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    labs(x = "Charging Station", y = "d(Hourly LMP) ($K/MWh)", title = paste0(names(scn.to.load), collapse = "->")) +
    scale_fill_discrete("")
  dt.ed <- getAreaDispatch(scenario = scn.to.load[2])
  plt6 <- ggplot(dt.ed[ara == 0]) + theme_bw(12) + 
    geom_boxplot(mapping = aes(x = 1, y = sourceprice/1e3), fill = "grey", alpha = .8) + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1, color = "white")) +
    labs(x = "", y = "Hourly LMP ($K/MWh)", title = paste0(names(scn.to.load)[2])) +
    scale_fill_discrete("") +
    coord_cartesian(ylim = c(0, 15))
  
  return(list(plt1, plt2)) 
}
boxplots.plotQuantiles <- function(scn.to.load, dt.lmps = NULL, vec.inj = c("A" = "9395_chg")) {
  
  dt.inj <- data.table(label = names(vec.inj), inj = vec.inj)
  
  if(is.null(dt.lmps)) dt.lmps <- boxplots.getData(scn.to.load)
  dt.quants <- dt.lmps[variable == "lmp_dif", list(mean_val = mean(value),
                              mean_abs = mean(abs(value)),
                              # quant_val = stats::quantile(x = value, probs = c(0, .01, .25, .5, .75, .99, 1)),
                              # quant_abs = stats::quantile(x = abs(value), probs = c(0, .01, .25, .5, .75, .99, 1)),
                              # quant_lab = c(0, .01, .25, .5, .75, .99, 1),
                              quant_val = stats::quantile(x = value, probs = c(.01, .25, .5, .75, .99)),
                              quant_abs = stats::quantile(x = abs(value), probs = c(.01, .25, .5, .75, .99)),
                              quant_lab = c(.01, .25, .5, .75, .99)
                              ),
                       by = list(inj, area)]
  dt.quants <- merge(dt.quants, dt.inj, by = "inj", all.x = T)
  
  # LMP
  # vec.levels <- dt.quants[scn == "Dif." & quant_lab == .50][order(quant_val, decreasing = F)]$inj
  # dt.plot <- dt.quants[scn == "Dif."][, inj := factor(x = inj, levels = vec.levels)]
  # dt.plot2 <- dcast(dt.plot, inj ~ quant_lab, value.var = "quant_val")[, inj := factor(x = inj, levels = vec.levels)]
  # plt1 <- ggplot(dt.plot) + theme_bw(12) +
  #   geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.01`, ymax = `0.99`), fill = "grey90", color = "black") +
  #   geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.25`, ymax = `0.75`), fill = "grey80", color = "black") +
  #   geom_line(mapping = aes(x = as.integer(inj), y = quant_val, group = quant_lab, color = factor(quant_lab))) +
  #   geom_point(mapping = aes(x = as.integer(inj), y = quant_val, color = factor(quant_lab))) +
  #   labs(y ="HFC abs(LMP) ($/MWh)") +
  #   scale_x_continuous(name = NULL, labels = NULL) +
  #   scale_color_brewer("Quantiles", type = "qual", palette = 6)
  
  # Abs(LMP) + LOG
  vec.levels <- dt.quants[quant_lab == .50][order(mean_abs, decreasing = F)]$inj
  dt.plot <- copy(dt.quants)[, inj := factor(x = inj, levels = vec.levels)]
  dt.plot2 <- dcast(dt.plot, inj ~ quant_lab, value.var = "quant_abs")[, inj := factor(x = inj, levels = vec.levels)]
  # plt2 <- ggplot(dt.plot) + theme_bw(12) +
  #   geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.01`, ymax = `0.99`), fill = "grey90") +
  #   geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.25`, ymax = `0.75`), fill = "grey80") +
  #   geom_line(data = dt.plot2, mapping = aes(x = as.integer(inj), y = `0.5`, group = 1), color = "grey50") +
  #   geom_point(mapping = aes(x = as.integer(inj), y = mean_abs), color = "black") +
  #   labs(y ="HFC ABS(LMP) ($/MWh)") +
  #   scale_x_continuous(name = NULL, labels = NULL) +
  #   scale_color_brewer("Quantiles", type = "qual", palette = 6) +
  #   scale_y_log10(labels = function(x) format(x, scientific = F, drop0trailing = T))
  
  plt2 <- ggplot() + theme_bw(12) +
    geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.01`, ymax = `0.99`, 
                                               fill = " 1% - 99%")) +
    geom_ribbon(data = dt.plot2, mapping = aes(x = as.integer(inj), ymin = `0.25`, ymax = `0.75`, 
                                               fill = "25% - 75%")) +
    geom_line(data = dt.plot2, mapping = aes(x = as.integer(inj), y = `0.5`, group = 1, 
                                             color = "50%")) +
    geom_point(data = dt.plot, mapping = aes(x = as.integer(inj), y = mean_abs, 
                                             color = "mean")) +
    geom_point(data = dt.plot[inj %in% vec.inj], mapping = aes(x = as.integer(inj), y = mean_abs), color = "red") +
    geom_text(data = dt.plot[inj %in% vec.inj], mapping = aes(x = as.integer(inj), y = mean_abs, label = label), color = "red", nudge_y = .25, nudge_x = -.75) +
    labs(title = "Local Effects: |Δ(HFC LMPs)| ($/MWh)") +
    scale_x_continuous(name = NULL, labels = NULL) + theme(axis.ticks.x = element_blank()) +
    scale_y_log10(name = NULL, labels = function(x) format(x, scientific = F, drop0trailing = T)) +
    scale_color_manual("", breaks = c("mean", "50%"), 
                       values = c("black", "grey50"),
                       labels = c("Mean", "Quantile: 50%")) +
    scale_fill_manual("", breaks = c("25% - 75%", " 1% - 99%"), 
                      values = c("grey80", "grey90"),
                      labels = c("Quantile: 25% - 75%", "Quantile:  1% - 90%"))
  
  return(plt2)
}
boxplots.plotSpecViolins <- function(scn.to.load, dt.lmps = NULL, vec.inj = c("A" = "9395_chg")) {
  
  if(is.null(dt.lmps)) dt.lmps <- boxplots.getData(scn.to.load)
  
  dt.inj <- data.table(label = names(vec.inj), inj = vec.inj)
  
  # Violins
  dt.plot <- merge(dt.lmps[variable == "lmp_dif"], dt.inj, by = "inj")
  # dt.plot <- dt.lmps[scn == "Dif." & inj %in% vec.inj]
  vec.levels <- dt.plot[, list(mlmp = mean(abs(value))), by = inj][order(mlmp, decreasing = F)][, i := 1:.N]$inj
  dt.plot <- dt.plot[, inj := factor(x = inj, levels = vec.levels)]
  dt.plot[, mnth := month(stop)]
  
  dt.sizes <- dt.lmps[variable == "p_to", list(max_load = max(abs(value))), by = list(inj)][inj %in% vec.inj][order(max_load, decreasing = F)][, index := 1:.N]
  dt.plot <- merge(dt.plot, dt.sizes, by = "inj")
  
  plt1 <- ggplot() + theme_bw(12) + 
    # geom_violin(data = dt.plot,
    #             mapping = aes(x = index, y = abs(value), group = inj, fill = inj), draw_quantiles = .5) + 
    geom_violin(data = dt.plot,
                mapping = aes(x = paste0(round(max_load,1)), y = abs(value), group = inj, fill = inj), draw_quantiles = .5) + 
    geom_point(data = dt.plot[, list(avg_lmp = mean(abs(value))), by = list(max_load, inj)], 
               mapping = aes(x = paste0(round(max_load,1)), y = avg_lmp)) +
    labs(title = "Local Effects: |Δ(HFC LMPs)| ($/MWh)", x = "Max Demand (MW)") +
    # theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    # scale_x_discrete(name = NULL, labels = NULL) + theme(axis.ticks.x = element_blank()) +
    scale_fill_brewer("HFC\nStation", type = "qual", palette = 6, 
                      breaks = vec.inj,
                      labels = names(vec.inj)) +
    scale_y_log10(name = NULL, labels = function(x) format(x, scientific = F, drop0trailing = T))
  
  
  return(plt1)
}
boxplots.plotScatter <- function(scn.to.load, dt.lmps = NULL, vec.inj = c("A" = "9395_chg")) {
  
  if(is.null(dt.lmps)) dt.lmps <- boxplots.getData(scn.to.load)
  
  dt.inj <- data.table(label = names(vec.inj), inj = vec.inj)
  dt.scatter <- merge(dt.lmps[variable == "lmp_dif", list(mean_abs = mean(abs(value))), by = list(inj)],
                      dt.lmps[variable == "p_to", list(max_load = max(abs(value))), by = list(inj)], by = "inj")
  dt.scatter <- merge(dt.scatter, dt.inj, by = "inj", all.x = T)
  
  plt1 <- ggplot(dt.scatter) + theme_bw() +
    geom_point(mapping = aes(x = max_load, y = mean_abs)) +
    geom_point(data = dt.scatter[!is.na(label)], mapping = aes(x = max_load, y = mean_abs), color = "red") +
    geom_text(data = dt.scatter[!is.na(label)], mapping = aes(x = max_load, y = mean_abs, label = label), color = "red", nudge_y = .1, nudge_x = .5) +
    scale_y_log10(labels = function(x) format(x, scientific = F, drop0trailing = T)) +
    labs(x = "Max Charger Power (MW)", y = NULL, title = "Local Effects vs. Charger Size")
  
  return(plt1)
}

if(F) {
  scn.to.load <- list(
    # from = paste0("Scn_9", c(90,91)), # Base
    # to = paste0("Scn_9", c(70,71)) # Disp
    from = paste0("Scn_", c(970, 971)), # Disp
    to = paste0("Scn_", c(980, 981)) # Conc
  )
}