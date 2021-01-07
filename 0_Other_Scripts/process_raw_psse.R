# Script processes the PSSE RAW network file distributed by ERCOT for CRR auctions
# Output: list of datatables in memory; one each for the different sections of the PSSE file

##### FUNCTIONS #####
f.process1 <- function(dt.data, dt.frame, remove.last = 1) {
  cat("\n", "START")
  # Disaggregate entries, trim whitespace, and add to DT
  for(i in 1:length(dt.data)) {
    if(round(i/length(dt.data)*100,2) %% 5 == 0) cat("\n", round(i/length(dt.data)*100), "%")
    vec.disagg <- tstrsplit(dt.data[i], ",")
    vec.disagg <- gsub(pattern = "*", "", vec.disagg)
    if(remove.last != 0) vec.disagg <- head(vec.disagg, -1*remove.last)
    if(length(vec.disagg) < length(dt.frame)) vec.disagg <- c(vec.disagg, rep(NA, times = (length(dt.frame)-length(vec.disagg))))
    dt.frame <- rbind(dt.frame, t(vec.disagg), use.names = F)
  }
  # Make into numbers what can be made into numbers
  for(i in 1:length(dt.frame)) {
    # cat("\n", i)
    suppressWarnings(if(all(!is.na(as.numeric(dt.frame[[i]])))) dt.frame[[i]] <- as.numeric(dt.frame[[i]]))
  }
  return(dt.frame)
}

##### PRE PROCESSING #####

# Read RAW file into memory, each line as one entry in a vector
chr.raw <- readLines(con = paste0(PATH.TOPOLOGY, topology.name))
chr.boilerplate <- head(chr.raw, 3)
stopifnot(grepl("PSS(R)E", chr.boilerplate[1], fixed = T)) # Verify boilerplate
chr.raw <- tail(chr.raw,-3) # chop off boilerplate

end.indices = grep("end of", chr.raw, ignore.case = T)
begin.indices = c(1, head(end.indices+1, -1))

# Parse the file into chunks; store each chunk as named vector in list
lst.inputs = list()
for(i in 1:length(end.indices)){
  chr.title =  substr(chr.raw[end.indices[i]],
                      start = regexpr("END OF ", chr.raw[end.indices[i]])[1] +
                        attributes(regexpr("END OF ", chr.raw[end.indices[i]]))[[1]],
                      stop = regexpr(" DATA,", chr.raw[end.indices[i]]) - 1)
  cat("\n", chr.title)
  
  chr.entries = chr.raw[begin.indices[i] : (end.indices[i]-1)]
  
  lst.inputs[[chr.title]] = chr.entries
}

lst.outputs <- list()

##### MAIN PROCESSING #####

chr.category <- "BUS"
dt.frame <- data.table("I" = numeric(), "NAME" = character(), "BASKV" = numeric(), 
                     "IDE" = numeric(), "GL" = numeric(), "BL" = numeric(), 
                     "AREA" = numeric(), "ZONE" = numeric(), "VM" = numeric(), 
                     "VA" = numeric(), "OWNER" = character())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "LOAD"
dt.frame <- data.table("I" = numeric(), "ID" = character(), "STATUS" = numeric(), 
                     "AREA" = numeric(), "ZONE" = numeric(), "PL" = numeric(), 
                     "QL" = numeric(), "IP" = numeric(), "IQ" = numeric(), 
                     "YP" = numeric(), "YQ" = numeric(), "OWNER" = character())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "GENERATOR"
dt.frame <- data.table("I" = numeric(), "ID" = character(), "PG" = numeric(), 
                      "QG" = numeric(), "QT" = numeric(), "QB" = numeric(), 
                      "VS" = numeric(), "IREG" = numeric(), "MBASE" = numeric(), 
                      "ZR" = numeric(), "ZX" = numeric(), "RT" = character(), 
                      "XT" = numeric(), "GTAP" = numeric(), "STAT" = character(), 
                      "RMPCT" = numeric(), "PT" = numeric(), "PB" = character(), 
                      "O1" = numeric(), "O2" = numeric(), "O3" = numeric(), "O4" = numeric(), 
                      "F1" = numeric(), "F2" = numeric(), "F3" = numeric(), "F4" = numeric())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame)
dt.frame[, ID := gsub(" *' *", "", ID)]
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "BRANCH"
dt.frame <- data.table("I" = numeric(), "J" = numeric(), "CKT" = character(), 
                      "R" = numeric(), "X" = numeric(), "B" = numeric(), 
                      "RATEA" = numeric(), "RATEB" = numeric(), "RATEC" = numeric(), 
                      "GI" = numeric(), "BI" = numeric(), "GJ" = numeric(), "BJ" = numeric(),
                      "ST"= numeric(), "LEN" = numeric(), 
                      "O1" = numeric(), "O2" = numeric(), "O3" = numeric(), "O4" = numeric(), 
                      "F1" = numeric(), "F2" = numeric(), "F3" = numeric(), "F4" = numeric())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "TRANSFORMER" # (Multiline)
ind.start <- grep(pattern = "\\[", x = lst.inputs[["TRANSFORMER"]]) - 2
ind.end <- tail(c(ind.start - 1, length(lst.inputs[["TRANSFORMER"]])), -1)
if(max(ind.end-ind.start+1) != 4) stop("5th line in XF data")
dt.frame <- data.table("I" = numeric(), "J" = numeric(), "K" = numeric(), "CKT" = character(),
                       "CW" = numeric(), "CZ" = numeric(), "CM" = numeric(),
                       "MAG1" = numeric(), "MAG2" = numeric(), "NMETR" = numeric(),
                       "NAME"= numeric(), "STAT" = numeric(),
                       "O1" = numeric(), "F1" = numeric(), "O2" = numeric(), "F2" = numeric(),
                       "O3" = numeric(), "F3" = numeric(), "O4" = numeric(), "F4" = numeric())
dt.1 <- f.process1(lst.inputs[[chr.category]][ind.start], dt.frame, remove.last = 0)
dt.frame <- data.table("R1-2" = numeric(), "X1-2" = numeric(), "SBASE1-2" = numeric())
dt.2 <- f.process1(lst.inputs[[chr.category]][ind.start + 1], dt.frame, remove.last = 0)
dt.frame <- data.table("WINDV1" = numeric(), "NOMV1" = numeric(), "ANG1" = numeric(), 
                       "RATA1" = numeric(), "RATB1" = numeric(), "RATC1" = numeric(),
                       "COD1" = numeric(), "CONT1" = numeric(), "RMA1" = numeric(),
                       "RMI1"= numeric(), "VMA1" = numeric(), "VMI1" = numeric(),
                       "NTP1" = numeric(), "TAB1" = numeric(), "CR1" = numeric(), "CX1" = numeric())
dt.3 <- f.process1(lst.inputs[[chr.category]][ind.start + 2], dt.frame, remove.last = 1)
dt.frame <- data.table("WINDV2" = numeric(), "NOMV2" = numeric())
dt.4 <- f.process1(lst.inputs[[chr.category]][ind.start + 3], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- data.table(dt.1, dt.2, dt.3, dt.4)
rm(dt.1, dt.2, dt.3, dt.4)

chr.category <- "AREA"
dt.frame <- data.table("I" = numeric(), "ISW" = character(), "PDES" = numeric(), 
                      "PTOL" = numeric(), "ARNAME" = numeric())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "TWO-TERMINAL DC" # Up to 3 lines
dt.frame <- data.table("I" = numeric(), "MDC" = character(), "RDC" = numeric(), 
                       "SETVL" = numeric(), "VSCHD" = numeric(), "VCMOD" = numeric(), 
                       "RCOMP" = numeric(), "DELTI" = numeric(), "METER" = numeric(), 
                       "DCVMIN" = numeric(), "CCCITMX" = numeric(), "CCCACC" = numeric(), 
                       "IPR" = numeric(), "NBR" = numeric(), "ALFMX" = numeric(), 
                       "ALFMN" = numeric(), "RCR" = numeric(), "XCR" = numeric(), 
                       "EBASR" = numeric(), "TRR" = numeric(), "TAPR" = numeric(), 
                       "TMXR" = numeric(), "TMNR" = numeric(), "STPR" = numeric(), 
                       "ICR" = numeric(), "IFR" = numeric(), "ITR" = numeric(), 
                       "IDR" = numeric(), "XCAPR" = numeric(), "IPI" = numeric(), 
                       "NBI" = numeric(), "GAMMX" = numeric(), "GAMMN" = numeric(), 
                       "RCI" = numeric(), "XCI" = numeric(), "EBASI" = numeric(), 
                       "TRI" = numeric(), "TAPI" = numeric(), "TMXI" = numeric(), 
                       "TMNI" = numeric(), "STPI" = numeric(), "ICI" = numeric(), 
                       "IFI" = numeric(), "ITI" = numeric(), "IDI" = numeric(), "XCAPI" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "VSC DC LINE" # Up to 3 lines
dt.frame <- data.table("NAME" = numeric(), "MDC" = character(), "RDC" = numeric(), 
                       "O1" = numeric(), "F1" = numeric(), "O2" = numeric(), "F2" = numeric(), 
                       "O3" = numeric(), "F3" = numeric(), "O4" = numeric(), "F4" = numeric(), 
                       "IBUS" = numeric(), "TYPE" = numeric(), "MODE" = numeric(), 
                       "DOCET" = numeric(), "ACSET" = numeric(), "ALOSS" = numeric(), 
                       "BLOSS" = numeric(), "MINOSS" = numeric(), "SMAX" = numeric(), 
                       "IMAX" = numeric(), "PWF" = numeric(), "MAXQ" = numeric(), 
                       "MINQ" = numeric(), 
                       "IBUS" = numeric(), "TYPE" = numeric(), "MODE" = numeric(), 
                       "DOCET" = numeric(), "ACSET" = numeric(), "ALOSS" = numeric(), 
                       "BLOSS" = numeric(), "MINOSS" = numeric(), "SMAX" = numeric(), 
                       "IMAX" = numeric(), "PWF" = numeric(), "MAXQ" = numeric(), 
                       "MINQ" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "SWITCHED SHUNT"
dt.frame <- data.table("I" = numeric(), "MODSW" = character(), "VSWHI" = numeric(),
                      "VSWLO" = numeric(), "SWREM" = numeric(), "RMPCT" = numeric(),
                      "RMIDNT" = character(), "BINIT" = numeric(), 
                      "N1" = numeric(), "B1" = numeric(), "N2" = numeric(), "B2" = numeric(), 
                      "N3" = numeric(), "B3" = numeric(), "N4" = numeric(), "B4" = numeric(), 
                      "N5" = numeric(), "B5" = numeric(), "N6" = numeric(), "B6" = numeric(), 
                      "N7" = numeric(), "B7" = numeric(), "N8" = numeric(), "B8" = numeric())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 1)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "IMPEDANCE CORRECTION"
dt.frame <- data.table("I" = numeric(),
                       "T1" = numeric(), "F1" = numeric(), "T2" = numeric(), "F2" = numeric(), 
                       "T3" = numeric(), "F3" = numeric(), "T4" = numeric(), "F4" = numeric(), 
                       "T5" = numeric(), "F5" = numeric(), "T6" = numeric(), "F6" = numeric(), 
                       "T7" = numeric(), "F7" = numeric(), "T8" = numeric(), "F8" = numeric(), 
                       "T9" = numeric(), "F9" = numeric(), "T10" = numeric(), "F10" = numeric(), 
                       "T11" = numeric(), "F11" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "MULTI-TERMINAL DC" # Multi
dt.frame <- data.table("I" = numeric(), "NCONV" = character(), "NDCBS" = numeric(), 
                       "NDCLN" = numeric(), "MDC" = numeric(), "VCONC" = numeric(), 
                       "VCMOD" = numeric(), "VCONC" = numeric(), 
                       "IB" = numeric(), "N" = numeric(), "ANGMX" = numeric(), 
                       "ANGMN" = numeric(), "RC" = numeric(), "XC" = numeric(), 
                       "EBAS" = numeric(), "TR" = numeric(), "TAP" = numeric(), 
                       "TPMX" = numeric(), "TPMN" = numeric(), "TSTP" = numeric(), 
                       "STVL" = numeric(), "DCPF" = numeric(), "MARG" = numeric(), 
                       "CNVCOD" = numeric(), 
                       "IDC" = numeric(), "IB" = numeric(), "IA" = numeric(), 
                       "ZONE" = numeric(), "Name" = numeric(), "IDC2" = numeric(), 
                       "RGRND" = numeric(), "OWNER" = numeric(), 
                       "IDC" = numeric(), "JDC" = numeric(), "DCDKT" = numeric(), 
                       "RDC" = numeric(), "LDC" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "MULTI-SECTION LINE"
dt.frame <- data.table("I" = numeric(), "J" = character(), "ID" = numeric(),
                       "DUM1" = numeric(), "DUM2" = numeric(), "DUM3" = numeric(),
                       "DUM4" = numeric(), "DUM5" = numeric(), "DUM6" = numeric(),
                       "DUM7" = numeric(), "DUM8" = numeric(), "DUM9" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "ZONE"
dt.frame <- data.table("I" = numeric(), "ZONAME" = character())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 1)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "INTER-AREA TRANSFER"
dt.frame <- data.table("ARFROM" = numeric(), "ARTO" = character(), "TRID" = numeric(), "PTRAN" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 1)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "OWNER"
dt.frame <- data.table("I" = numeric(), "OWNAME" = character())
dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 0)
lst.outputs[[chr.category]] <- dt.frame

chr.category <- "FACTS DEVICE"
dt.frame <- data.table("N" = numeric(), "I" = character(), "J" = numeric(), 
                       "MODE" = numeric(), "PDES" = numeric(), "QDES" = numeric(), 
                       "VSET" = numeric(), "SHMAX" = numeric(), "TRMX" = numeric(), 
                       "VTMN" = numeric(), "VTMX" = numeric(), "VSMX" = numeric(), 
                       "IMX" = numeric(), "LINX" = numeric(), "RMPCT" = numeric(), 
                       "OWNER" = numeric(), "SET1" = numeric(), "SET2" = numeric(), 
                       "VSREF" = numeric())
# dt.frame <- f.process1(lst.inputs[[chr.category]], dt.frame, remove.last = 1)
lst.outputs[[chr.category]] <- dt.frame