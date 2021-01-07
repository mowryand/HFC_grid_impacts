# Plots network + schedule info on more abstract network graph

##### LIBRARIES #####
library(data.table)
library(ggplot2)
library(igraph)
library(ggraph)
library(gridExtra)

##### INPUTS #####

if(F) {
  # An interesting set of inputs
  # Not to run on sourcing so that calling script can spec own inputs
  scenario <- c("Scn_000981") 
  node.of.interest <- 38131
  num.order <- 6
  time.interval <- as.POSIXct(c("2013-08-21 17:00:00", "2013-08-21 18:00:00"), tz = "ETC/GMT+6")
  graph.network.layout <- c("geo", "auto")[2]
  graph.subset.type <- c("ego", "bbox")[1]
  bbox <- c(-98-25/60, 29+9/60, -95-30/60, 30+10/60)
}


##### FUNCTIONS #####
source(paste0(PATH.SCRIPTS, "library.R"))

# Calculate meaningful edge metrics for the whole time window, for all edges
processEdges <- function(dt.edges.pre) {
  
  # Sum across circuits and Average across intervals
  dt.edges <- dt.edges.pre[, list(mw = sum(mw), sp = mean(sp), binding = pmax(binding)), 
                           by = list(i, j, kv, lim, int)]
  dt.edges <- dt.edges[, list(mwh = sum(mw), mw = mean(mw), min = min(mw), max = max(mw),
                              sp = mean(sp), sc = sum(sp), binding = sum(binding)), 
                       by = list(i, j, kv, lim)]
  
  return(dt.edges)
}
processNodes <- function(dt.nodes.pre, dt.edges, dt.inj) {
  # collapse multiple injectors at the same node
  dt.inj <- dt.inj[, list(inj = paste(unique(inj), collapse = "; "), type = paste(unique(type), collapse = "; "),
                          p = mean(p), lmp = mean(lmp), max = sum(max)), by = list(int, node)]
  # Average across intervals
  dt.inj <- dt.inj[, list(mwh = sum(p), mw = mean(p), lmp = mean(lmp), lmc = sum(lmp)), 
                   by = list(inj, node, type, max)]
  
  # Assume nodes without injectors are load buses (for labeling purposes)
  dt.nodes <- merge(dt.nodes.pre, dt.inj[, list(node, type, mw)], all.x = T)
  dt.nodes[is.na(type), c("type", "mw") := list("Load", 0)]
  
  # Do the net-injection math for a check. Because of missing power flow info (that was assumed to be = 0) this will not be accurage
  dt.check <- merge(dt.edges[, list(outbound = sum(mw)), by = list(node = i)],
                    dt.edges[, list(inbound = sum(mw)), by = list(node = j)],
                    by = "node", all = T)
  dt.check <- dt.check[, list(node, injection = ifelse(is.na(outbound), 0, outbound) - ifelse(is.na(inbound), 0, inbound))]
  dt.nodes <- merge(dt.nodes, dt.check, by = "node", all.x = T)
  
  return(dt.nodes)
}
plotNetwork <- function(subnet, l, style = c("bare", "trad")) {
  # https://kateto.net/network-visualization
  if(style == "bare") {
    plt <- ggraph(subnet, layout = l) + theme_bw() +
      
      geom_node_circle(mapping = aes(
        fill = type,
        # r = (abs(mw)+1)^(1/4)/max((abs(mw)+1)^(1/4))/4
        r = .25
      ),
      color = "black",
      alpha = .5) +
      
      geom_edge_link(mapping = aes(
        edge_color = as.character(kv),
        # edge_width = as.character(kv),
        label = NA
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      start_cap = circle(5, "mm"),
      end_cap = circle(5, "mm"),
      alpha = .75,
      angle_calc = "along",
      label_dodge = unit(4, "mm")) +
      
      # geom_node_text(mapping = aes(label = node_id)) +
      
      # scale_edge_width_manual("Line kV", 
      #                         breaks = c("69", "138", "230", "345"), 
      #                         labels = c("69", "138", "230", "345"), 
      #                         values = c("69" = 1, "138" = 1.5, "230" = 2, "345" = 2.5)) +
      
      scale_edge_color_manual("Line kV", 
                              breaks = c("69", "138", "230", "345"), 
                              labels = c("69", "138", "230", "345"), 
                              values = c("69" = "black", "138" = "darkblue", "230" = "darkgreen", "345" = "red")) +
      
      # scale_edge_color_viridis("Shadow Price", limits = c(-5000, 5000)) +
      
      labs(x = "", y = "")
    
  } else if(style == "trad") {
    plt <- ggraph(subnet, layout = l) + theme_bw() +
      geom_edge_link(mapping = aes(
        edge_color = sp,
        edge_width = as.character(kv),
        label = paste0(round(mw), " / ", round(lim))
        # label = NA
        # label = paste0(round(binding))
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      start_cap = circle(5, "mm"),
      end_cap = circle(5, "mm"),
      alpha = .75,
      angle_calc = "along",
      label_dodge = unit(4, "mm")) +
      
      geom_node_circle(mapping = aes(
        fill = type,
        r = (abs(mw)+1)^(1/4)/max((abs(mw)+1)^(1/4))/4
      ),
      color = "black",
      alpha = .5) +
      
      geom_node_text(mapping = aes(label = node_id)) +
      # geom_node_text(mapping = aes(label = round(mw))) +
      # geom_node_text(mapping = aes(label = round(injection))) +
      
      scale_edge_width_manual("Line kV", 
                              breaks = c("69", "138", "230", "345"), 
                              labels = c("69", "138", "230", "345"), 
                              values = c("69" = 1, "138" = 1.5, "230" = 2, "345" = 2.5)) +
      
      scale_edge_color_viridis("Shadow Price", limits = c(-5000, 5000)) +
      
      labs(x = "", y = "")
    
  } else if(style == "tradmax") {
    plt <- ggraph(subnet, layout = l) + 
      
      theme_bw(GGPLOT_SIZE) +
      
      geom_node_circle(mapping = aes(
        fill = type,
        # r = (abs(mw)+1)^(1/4)/max((abs(mw)+1)^(1/4))/4
        r = .25
      ),
      color = "black",
      alpha = .5) +
      
      geom_edge_link(mapping = aes(
        # edge_color = sp,
        # edge_width = as.character(kv),
        label = paste0(round(max), " / ", round(lim))
        # label = NA
        # label = paste0(round(binding))
      ),
      arrow = arrow(length = unit(3, "mm"), type = "closed"),
      start_cap = circle(5, "mm"),
      end_cap = circle(5, "mm"),
      alpha = .75,
      angle_calc = "along",
      label_dodge = unit(4, "mm"),
      check_overlap = T) +
      
      # geom_node_text(mapping = aes(label = node_id)) +
      # geom_node_text(mapping = aes(label = round(mw))) +
      # geom_node_text(mapping = aes(label = round(injection))) +
      
      # scale_edge_width_manual("Line kV", 
      #                         breaks = c("69", "138", "230", "345"), 
      #                         labels = c("69", "138", "230", "345"), 
      #                         values = c("69" = 1, "138" = 1.5, "230" = 2, "345" = 2.5)) + 
      
      scale_fill_brewer(name = NULL, type = "qual", palette = 6) +
      
      # scale_edge_color_viridis("Shadow Price", limits = c(-5000, 5000)) +
      
      labs(x = "", y = "") +
      theme(legend.position = c(.75, .75))
    
  } else {
    
  }
  
  plt <- plt + theme(axis.ticks = element_blank(), axis.text = element_blank())
  
  return(plt)
}
plotEdgeStats <- function(dt.edges.pro, dt.edges, subnet, variable = c("binding", "sp", "mw")[1], cut.zero = F,
                          suppress.title = F) {
  
  dt.plot <- dt.edges.pro[i %in% names(V(subnet)) | j %in% names(V(subnet))][, label := paste0(i, "-", j)]
  dt.plot[, label := factor(label, levels = dt.plot[order(binding, decreasing = T), label])]
  
  if(cut.zero) dt.plot <- dt.plot[sc != 0]
  
  if(variable == "binding") {
    plt <- ggplot(dt.plot) + theme_bw(12) +
      geom_point(mapping = aes(x = label, y = binding)) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(x = "", y = "Binding Hours")
    if(!suppress.title) plt <- plt + ggtitle(paste0(paste0(scenario, collapse = "->"), ", ", node.of.interest, "+", num.order))
  } else if(variable == "sp") {
    plt <- ggplot(dt.plot) + theme_bw(12) +
      geom_point(mapping = aes(x = label, y = sc/1e3, color = binding), size = 4) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_color_viridis("Binding\nHours") +
      labs(x = "", y = "Sum of Shadow Prices ($K)")
    if(!suppress.title) plt <- plt + ggtitle(paste0(paste0(scenario, collapse = "->"), ", ", node.of.interest, "+", num.order))
  } else if(variable == "mw") {
    dt.plot2 <- merge(dt.edges, dt.plot[, list(i, j)], by = c("i", "j"))[, label := paste0(i, "-", j)]
    # ggplot(dt.plot) + theme_bw(12) + geom_hline(yintercept = 0) +
    #   geom_linerange(mapping = aes(x = label, ymin = -lim, ymax = lim, color = "nameplate range"), size = 3) +
    #   geom_linerange(mapping = aes(x = label, ymin = min, ymax = max, color = "simulation range"), size = 1) +
    #   theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    #   scale_color_manual("", breaks = c("nameplate range", "simulation range"), values = c("grey", "red")) +
    #   labs(x = "Edge", y = "Line Power Flow (MW)", title = paste0(scenario, ", ", node.of.interest))
    plt <- ggplot() + theme_bw(12) + geom_hline(yintercept = 0) +
      geom_linerange(data = dt.plot, mapping = aes(x = label, ymin = -lim, ymax = lim, color = "Nameplate"), size = 3) +
      geom_violin(data = dt.plot2, mapping = aes(x = label, y = mw, color = "Simulation", group = label), bw = 5, trim = T, alpha = .5) +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      scale_color_manual("", breaks = c("Nameplate", "Simulation"), values = c("grey", "red")) +
      labs(x = "", y = "Line Power Flow (MW)")
    if(!suppress.title) plt <- plt + ggtitle(paste0(paste0(scenario, collapse = "->"), ", ", node.of.interest, "+", num.order))
  }
  return(plt)
}
buildSubnet <- function(dt.edges.pro, dt.nodes.pro) {
  
  # Build network
  net <- graph_from_data_frame(d = dt.edges.pro, vertices = copy(dt.nodes.pro)[, node_id := node], directed = T)
  if(graph.subset.type == "ego") {
    subnet <- make_ego_graph(net, order = num.order, mode = "all", 
                             nodes = V(net)[names(V(net)) == node.of.interest])[[1]]
  } else if(graph.subset.type == "bbox") {
    dt.nodes <- dt.nodes.pro[long %between% c(bbox[1], bbox[3]) & lat %between% c(bbox[2], bbox[4])]
    # dt.nodes <- dt.nodes[volt > 69]
    subnet <- graph_from_data_frame(d = dt.edges.pro[i %in% dt.nodes$node & j %in% dt.nodes$node], vertices = copy(dt.nodes.pro)[, node_id := node], directed = T)
  } else {
    stop("Unhandled graph.subset.type")
    vec.nodes <- c(5536, 7258, 7636, 7639, 7640, 8100, 8102, 8523, 8527, 8529, 8541,
                   78103, 78104, 78105, 78106, 78107, 78111, 78520, 78524, 78526, 78528, 78530,
                   78538, 78539, 78542, 78543, 170401, 170402, 170403)
    subnet <- graph_from_data_frame(d = dt.edges.pro[i %in% vec.nodes & j %in% vec.nodes], vertices = copy(dt.nodes.pro)[, node_id := node][node %in% vec.nodes], directed = T)
  }
  
  if(graph.network.layout == "geo") {
    indices <- match(as.integer(names(V(subnet))), dt.nodes.pro$node)
    dt.l <- dt.nodes[indices, list(x = long, y = lat)]
    
    # rescale to the auto scale for plotting purposes
    dt.auto <- as.data.table(layout_with_fr(subnet))
    dt.scaled <- dt.l[, list((x - min(x))/(max(x) - min(x))*(dt.auto[, max(V1) - min(V1)]), 
                             (y - min(y))/(max(y) - min(y))*(dt.auto[, max(V2) - min(V2)]))]
    
    l <- as.matrix(dt.scaled)
  } else if(graph.network.layout == "auto") {
    l <- layout_with_fr(subnet)
  } else {
    stop("Unhandled graph.network.layout")
  }
  
  return(list(subnet, l))
}

##### PRE-PROCESSING #####
if(T) {
  
  stopifnot(length(scenario) %in% c(1, 2))
  if(length(scenario) == 2) {
    dt.edges <- getEdgeInfoDiff(scenario, interval = time.interval)
    lst.inj <- getInjectorInfoDiff(scenario, interval = time.interval)
  } else {
    dt.edges <- getEdgeInfo(scenario, interval = time.interval) 
    lst.inj <- getInjectorInfo(scenario, interval = time.interval)
  }
  
  dt.inj <- merge(lst.inj[[1]], lst.inj[[2]], by = "inj")
  dt.nodes <- getNodes(scenario[1], bln.fix.missing.nodes = T)
  
  # Fix directions
  dt.edges[mw < 0, c("i", "j", "mw", "sp", "violation") := list(j, i, -mw, -sp, -violation)]
  
  dt.edges.pro <- processEdges(dt.edges)
  dt.nodes.pro <- processNodes(dt.nodes, dt.edges.pro, dt.inj)
}

plotPfNet <- function(dt.edges.pro, dt.nodes.pro) {
  set.seed(123)
  subnets <- buildSubnet(dt.edges.pro, dt.nodes.pro)
  subnet <- subnets[[1]]
  l <- subnets[[2]]
  
  ### AND PLOT
  plt0 <- plotNetwork(subnet, l, "bare")
  # plt1 <- plotNetwork(subnet, l, "trad")
  plt1 <- plotNetwork(subnet, l, "tradmax")
  plt2 <- plotEdgeStats(dt.edges.pro, dt.edges, subnet, "sp", cut.zero = T, suppress.title = T)
  plt3 <- plotEdgeStats(dt.edges.pro, dt.edges, subnet, "mw", cut.zero = T, suppress.title = T)
  
  plt4 <- arrangeGrob(plt1, plt2, plt3, layout_matrix = rbind(c(1,1,1,2,2),
                                                              c(1,1,1,3,3)))
  return(list(plt1, plt2, plt3, plt4, plt0))
}
