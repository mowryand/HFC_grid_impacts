# Script converts PSSE formatted data into PSO formatted data, optionally writes data to disk

# ##### LIBRARIES #####
# library(data.table)
# library(XML)
# 
# ##### INPUTS #####
# chr.user <- Sys.info()["user"]
# scenario <- "Scn_2"
# 
# path.data <- paste0("C:/Users/", chr.user, "/Dropbox (MIT)/EV Charging Infra/Data/")
# path.scripts <- paste0("C:/Users/", chr.user, "/Dropbox (MIT)/EV Charging Infra/ERCOT_PSO, scripts/")
# path.topology <- paste0("C:/Users/", chr.user, "/Dropbox (MIT)/EV Charging Infra/Data/Network1/")
# path.write <- paste0(path.data, "Network1/PSO/", scenario, "/")
# 
# topology.name <- "2018.JUN.Monthly.Auction.NetworkModel_PeakWD.raw"
# mapping.file.name <- "parsed_bus_info"
# 
# bln.write <- F

##### FUNCTIONS #####
catn <- function(...) cat("\n", ...)

f.descendIntoPlacemark <- function(lst.buses, chr.id) {
  this.name <- as.character(xmlValue(lst.buses[["name"]]))
  
  child.names <- names(lst.buses)
  
  if(any(child.names == "Placemark")) {
    # Extract info from the Placemark folders!
    to.loop <- which(child.names == "Placemark")
    dt.info <- rbindlist(lapply(X = to.loop, FUN = function(i) {
      bus.id <- as.integer(strsplit(as.character(lst.buses[[i]])[2], split = " ")[[1]][2]) # my god what a mess
      
      f.getInfoFromPlacemarkFolder(lst.buses[[i]], bus.id)
    }))
    extra.info <- tail(strsplit(chr.id, "; ")[[1]], 2)
    
    dt.info[, `:=`(wz = extra.info[1],
                   t1 = extra.info[2],
                   t2 = this.name)]
    return(dt.info)
  } else {
    # Loop through all the children named "Folder" to continue looking for Placemarks
    to.loop <- which(child.names == "Folder")
    dt.return <- rbindlist(lapply(X = to.loop, FUN = function(i) {
      catn(paste0(chr.id, "; ", this.name))
      f.descendIntoPlacemark(lst.buses[[i]], chr.id = paste0(chr.id, "; ", this.name))
    }))
    return(dt.return)
  }
}

f.getInfoFromPlacemarkFolder <- function(lst.pl, bus_id) {
  # Bus Name
  chr.name <- as.character(xmlValue(lst.pl[["name"]]))
  # HTML data
  df.html <- readHTMLTable(as.character(xmlValue(lst.pl[["description"]])))[[1]] # Could be dupes
  
  table.index <- which(as.vector(df.html[1,]) == bus_id)[1]
  
  vec.names <- c("ID", "Area", "Zone", "Settlement Zone")
  vec.data <- as.character(df.html[[table.index]][match(vec.names, df.html[[1]])])
  
  lst.return <- list(name = chr.name,
                     id = vec.data[1],
                     area = vec.data[2],
                     zone = vec.data[3],
                     lz = vec.data[4])
  
  # Lat/long data (taking just the first corner of the polygon)
  coords <- as.character(xmlValue(lst.pl[["Polygon"]][["outerBoundaryIs"]][["LinearRing"]][["coordinates"]]))
  xyz <- strsplit(strsplit(coords, " ")[[1]][1], ",")[[1]][1:3]
  lst.return <- append(lst.return, list(long = xyz[1],
                                        lat = xyz[2],
                                        z = xyz[3]))
  
  return(lst.return)
}

##### PRE PROCESSING #####

chr.write <- paste0(path.topology, mapping.file.name, ".csv")
xmlfile <- xmlRoot(xmlTreeParse(paste0(path.topology, topology.name)))

# Find the "buses" node and convert it into an R list
vec.folders <- which(names(xmlfile[[1]]) == "Folder")
names(vec.folders) <- NULL
vec.named.folders <- sapply(X = vec.folders, FUN = function(x) {
  res <- xmlValue(xmlfile[[1]][[x]][["name"]])
  return(res)
})
which.folder <- which(vec.named.folders == "Buses")
lst.buses <- xmlfile[[1]][[vec.folders[which.folder]]]

# Parse the bus KML doc for releveant information
dt.bus.info <- f.descendIntoPlacemark(lst.buses, chr.id = "top")

# Do some post processing
dt.bus.info[, `:=`(id = as.integer(id),
                   long = as.numeric(long),
                   lat = as.numeric(lat),
                   z = as.numeric(z),
                   wz = gsub("Zone ", "", wz),
                   t1 = gsub(" Buses", "", t1),
                   t2 = gsub(" Buses", "", t2))]

if(bln.write) fwrite(unique(dt.bus.info, by = "id"), chr.write)
