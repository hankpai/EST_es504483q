# similar to version 1, now handline interpolated gw SC field
#     

rm(list=ls(all=TRUE))

library(spatstat)
library(rgdal)
require(xlsx)

param <- "SpCond"

# closest, avg, or idw, max, min
sc.method <- "closest"

special.q47 <- F
zero.neg.qg <- F


# for averaged and closest file, column is "gw_SC", for idw is "gw_SC_idw"
if (sc.method == "idw") { 
  gw.sc.name <- "gw_SC_idw"  
} else if (sc.method == "max") {
  gw.sc.name <- "max_SC"  
} else if (sc.method == "min") {
  gw.sc.name <- "min_SC"  
} else{
  # for avg and closest cases
  gw.sc.name <- "gw_SC"
}

# methods should be: lin, sens, siegel (repeated medians)
regression.method <- "lin"

out.colnames <- c("start_seg_riverm", 
                  "end_seg_riverm", 
                  "Longitude",
                  "Latitude",
                  "Q_ustream",
                  "Q_dstream", 
                  "conc_ustream",
                  "conc_dstream", 
                  "conc_gw",
                  "Q_g",
                  "se_Q_gw",
                  "conc_diff",
                  "tds_ustream_kgs",
                  "tds_dstream_kgs",
                  "tds_gw_kgs",
                  "tds_diff_kgs",
                  "reach_len",
                  "slope",
                  "num_points", 
                  "well_dist",
                  "gw_sc_coeffvar",
                  "date",
                  "daily_flow")

# converts uS/cm to mg/L, then finally to kg/s flux, multiply factor to 
# flow x SC concentration
mass.flux.conversion <- 0.65/1000

# spacing for river segments 
spacing <- 1000

# maximum river m seen in data
max.riverm <- 40000

# minimum river m seen in data
min.riverm <- 6000

out.dir1 <- "D:/hank/btsync/hank_work/synoptic/data/analysis/gw_flux-model/model_output-est_revision02/"
out.dir2 <- strftime(Sys.Date(), "%Y%m%d")

if (zero.neg.qg == T){
  out.dir3 <- paste(out.dir1, out.dir2, "-", sc.method, "-zeroNegQg/",
                    regression.method, "/", sep = "")  
} else{
  out.dir3 <- paste(out.dir1, out.dir2, "-", sc.method, "/", regression.method, 
                    "/", sep = "")
}

# locations of the river segmented by the spacing noted above
seg.start <- seq(max.riverm, min.riverm + spacing, by = -1 * spacing)
seg.end <- seq(max.riverm - spacing, min.riverm, by = -1 * spacing)
segment.id <- 1:length(seg.start)
segment.bounds <- cbind(segment.id, seg.start, seg.end)

segment.names <- c("segment_id", "start_seg", "end_seg")
segment.units <- c("num", "m", "m")
segment.headers <- rbind(segment.names, segment.units)

# final processing step
processing.step <- "stopsInletDepthFilters"

# ===== file definitions
# flow and date file
flow.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/daily_flows/daily_start_flows02.csv"

# run ids
run.ids <- c(1:17)

# river m column id
riverm.id = "river_m"

# flow id's
start.flow.id <- "start_cms"
daily.flow.col <- 3

# sensor dir
sensor.dirs1 <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"

# major canals file
large.structs.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/large_structures-gpsRDist.csv"

# gw Sc file, along with river segment info
if (sc.method == "closest") {
  gw.sc.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/20150101-gw_spcond/20150127-well_SC_to_river_segments-closest.csv"  
} else {
  gw.sc.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/20150101-gw_spcond/20150127-well_SC_to_river_segs-closest_n3-avg_idw.csv"
  #gw.sc.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/revision2_gw_interpSC/20150320-well_SC_to_river_segments-interp_avg.csv"

}

gw.dir <- gw.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/20150101-gw_spcond/"

gw.stats.fn <- "20150130-unique_well_SC_stats-buffer5000-n5.csv"
gw.stats.fullfn <- paste(gw.dir, gw.stats.fn, sep = "")

gw.stats.dat <- read.table(gw.stats.fullfn, sep = ",", as.is = T, header = T)


# ====== LESS-CHANGED USER VARS =====

# params for HL: Temp, SpCond, Depth, LDO%, LDO
hl.params <- c("Temp", "SpCond", "Depth", "LDO%", "LDO")
# params for s::can: NO3, Turbid, NO3-Neq, TOCeq, DOCeq
scan.params <- c("NO3", "Turbid", "NO3-Neq", "TOCeq", "DOCeq")

# identifying multiparamater sonde being used
if(length(grep(param, hl.params)) > 0){
  sonde <- "HL"
} else if(length(grep(param, scan.params)) > 0){
  sonde <- "scan"
} else{
  sonde <- NA
  stop("===> sensor parameter not found! <===")
}

# ====== COMMON FUNCTIONS =====
# for common code used more than once

# standard files for henry, 2 rows of headers
FileDat2Row <- function(fullfn, headers){
  dat <- read.table(fullfn, sep = ",", as.is = T, skip = 2)
  colnames(dat) <- headers[1, ]
  return(dat)
}

FileHeaders2Row <- function(fullfn){
  headers <- read.table(fullfn, sep = ",", as.is = T, nrows = 2)
  colnames(headers) <- headers[1, ]
  return(headers)
}

# part of medians calculations
GetLines <- function(index, xy, repeated){
  index.dat <- xy[index, ]
  x.index <- index.dat[1]
  y.index <- index.dat[2]
  
  if(repeated == F){
    other.id <- (index + 1):nrow(xy)
  } else{
    other.id <- which(!(1:nrow(xy) %in% index))
  }
  
  other.dat <- xy[other.id, ]
  
  if(length(other.dat) > 2){
    x.other <- other.dat[, 1]
    y.other <- other.dat[, 2]
  } else{
    x.other <- other.dat[1]
    y.other <- other.dat[2]
  }
  
  slopes <- (y.index - y.other)/(x.index - x.other)
  
  if(repeated == T){
    return.slopes <- median(slopes)
    intercepts <- ((x.index * y.other) - (x.other * y.index))/
      (x.index - x.other)
    return.intercepts <- median(intercepts)
  } else{
    return.slopes <- slopes
    return.intercepts <- rep(NA, length(slopes))
  }
  
  return.dat <- cbind(return.slopes, return.intercepts)
  
  return(return.dat)
}

# function for median slopes 
MblmHp <- function(formula, dataframe, repeated = T){
  if(missing(dataframe)){
    dataframe <- environment(formula)
  }
  
  term <- as.character(attr(terms(formula), "variables")[-1])
  x <- dataframe[[term[2]]]
  y <- dataframe[[term[1]]]
  
  
  if(length(term) > 2){
    stop("Only linear models are accepted")
  }
  
  xx <- sort(x)
  yy <- y[order(x)]
  n <- length(xx)
  
  if(repeated == T){
    loop.length <- n + 1
  } else{
    loop.length <- n
  }
  
  row.lines <- mapply(GetLines, 1:(loop.length - 1),
                      MoreArgs = list(xy = cbind(xx, yy), repeated = repeated),
                      SIMPLIFY = F)
  
  all.lines <- do.call(rbind, row.lines)
  
  slopes <- all.lines[, 1]
  intercepts <- all.lines[, 2]
  
  slope <- median(slopes)
  if(repeated == F){
    intercepts <- yy - slope * xx
  }
  intercept <- median(intercepts)
  
  # from original mblm pkg
  res <- list()
  res$coefficients <- c(intercept, slope)
  names(res$coefficients) <- c("(Intercept)", term[2])
  res$residuals <- y - slope*x - intercept
  names(res$residuals) <- as.character(1:length(res$residuals))
  res$fitted.values <- x*slope + intercept
  names(res$fitted.values) <- as.character(1:length(res$fitted.values))
  res$slopes <- slopes
  res$intercepts <- intercepts
  res$df.residual <- n - 2
  res$rank <- 2
  res$terms <- terms(formula)
  res$call <- match.call()
  res$model <- data.frame(y, x)
  res$assign <- c(0, 1)
  if(missing(dataframe)){
    res$effects <- lm(formula)$effects
    res$qr <- lm(formula)$qr
  }
  else {
    res$effects <- lm(formula, dataframe)$effects
    res$qr <- lm(formula, dataframe)$qr
  }
  res$effects[2] <- sqrt(sum((res$fitted - mean(res$fitted))^2))
  res$xlevels <- list()
  names(res$model) <- term
  attr(res$model, "terms") <- terms(formula)
  class(res) <- c("mblm", "lm")
  res
}


# ===== Reading in files =====
# large inlet canals
large.structs.dat <- read.table(large.structs.fullfn, sep = ",", as.is = T,
                                header = T)

# reading daily flow info
flow.headers <- FileHeaders2Row(flow.fullfn)
flow.dat <- FileDat2Row(flow.fullfn, flow.headers)

# getting specified flows
run.dat <- flow.dat[flow.dat[, 1] %in% run.ids, ]
start.flows <- run.dat[, start.flow.id]
daily.flows <- run.dat[, daily.flow.col]

# getting specified days and changing to searchable string
run.dates <- strptime(run.dat[, 2], format = flow.headers[2, 2])
run.strdates <- strftime(run.dates, format = "%Y%m%d")
plot.strdates <- strftime(run.dates, format = "%m-%d-%Y")
sheet.strdates <- strftime(run.dates, format = "%m%d%y")

# getting gw SC data
gw.dat1 <- read.table(gw.sc.fullfn, sep = ",", as.is = T, header = T)

riv.xy <- cbind(gw.dat1[, "river_seg_E"], gw.dat1[, "river_seg_N"])

riv.spatial.pts <- SpatialPoints(riv.xy, 
                                 proj4string = CRS("+proj=utm +zone=10 ellps=WGS84"))
riv.transform <- spTransform(riv.spatial.pts, CRS("+proj=longlat"))

riv.longlat <- as.data.frame(riv.transform)
colnames(riv.longlat) <- c("Longitude", "Latitude")

# directory for following
sensor.dirs2 <- paste(sensor.dirs1, run.strdates, "-synoptic/organized/", 
                      sep = "") 

# sensor filenames
sensor.fns <- paste(run.strdates, "-", processing.step, "_", sonde, ".csv", 
                    sep = "")
sensor.fullfns <- paste(sensor.dirs2, sensor.fns, sep = "")


if (zero.neg.qg == T) {
  xlsx.fullfn <- paste(out.dir3, out.dir2, "-mod_", regression.method, "_", 
                       sc.method, "-all-", "negQg0.xlsx", sep = "")
} else{
  xlsx.fullfn <- paste(out.dir3, out.dir2, "-mod_", regression.method, "_", 
                       sc.method, "-all.xlsx", sep = "")
}

# ===== main loops =====

ByReach <- function(index, sensor.dat, start.flow, count){
  start.riverm <- seg.start[index]
  end.riverm <- seg.end[index]
  
  gw.id <- which.min(abs(start.riverm - gw.dat1[, "start_riverm"]))
  conc.gw <- gw.dat1[gw.id, gw.sc.name]
  
  well.name <- gw.dat1[gw.id, "well_name"]
  
  #gw.stats.id <- which(gw.stats.dat[, "well_name"] == well.name)
  #if (length(gw.stats.id) > 0) {
  #  gw.stats.cv <- gw.stats.dat[gw.stats.id, "sc_coeffvar"]
  #} else {
  gw.stats.cv <- NA  
  #}
  
  
  well.dist <- gw.dat1[gw.id, "dist_to_start_seg_m"]
  
  riv.coord <- riv.longlat[gw.id, ]
  
  # this is the discretized reach length
  #reach.len <- abs(start.riverm - end.riverm)
  
  sensor.riverm <- sensor.dat[, riverm.id]
  
  reach.id <- which(sensor.riverm >= end.riverm & 
                    sensor.riverm <= start.riverm)
  
  if(length(reach.id) > 1){
    count <- iter + 1
    iter <<- count
    #browser()
    reach.dat <- sensor.dat[reach.id, ]
    
    start.river.m <- sensor.dat[reach.id[1], riverm.id]
    end.river.m <- sensor.dat[reach.id[length(reach.id)], riverm.id]
    
    #reach.len <- abs(start.river.m - end.river.m)
    # SECTION CHANGED... algorithm would be messing estimating reach length
    
    #if (count == 1) {
    #  reach.len <- abs(start.river.m - end.riverm)
    #} else if (index == length(seg.start)) {
    #  reach.len <- abs(start.riverm - end.river.m)
    #} else {
    reach.len <- abs(start.river.m - end.river.m)
    #}
    
    # data for this model is from observed data within descretized segment
    x <- reach.dat[, riverm.id]
    y <- reach.dat[, param]
    
    if (regression.method == "lin") {
      mod.dat <- lm(y~x)
    } else if (regression.method == "sens") {
      mod.dat <- MblmHp(y~x, repeated = F)
    } else if (regression.method == "siegel") {
      # siegel repeated median slope estimator method
      mod.dat <- MblmHp(y~x, repeated = T)
    }
    
    
    #lm.dat <- lm(y~x)
    
    
    # predicted line is for  descretized segment
    predict.x <- data.frame(x = c(start.riverm, end.riverm))
    
    
    predict.lmdat <- predict.lm(mod.dat, predict.x, se.fit = T)
    
    slope <- mod.dat[[1]][2]
    
    conc.ustream <- predict.lmdat$fit[1]
    conc.dstream <- predict.lmdat$fit[2]
    
    if(count == 1){
      Q.ustream <- start.flow
    } else{
      Q.ustream <- Q.dstream.prev
    }

    Q.gw <- (Q.ustream * (conc.ustream - conc.dstream)) / (conc.dstream - 
                                                             conc.gw)
    
    if (zero.neg.qg == T) {
      if (Q.gw < 0 ){
        Q.gw <- 0
      }
      
    }
    
    # mass estimates are now converted to TDS in mg/L
    tds.gw <- Q.gw * conc.gw * mass.flux.conversion
    
    Q.dstream <- Q.ustream + Q.gw
    
    
    tds.ustream <- Q.ustream * conc.ustream * mass.flux.conversion
    tds.dstream <- Q.dstream * conc.dstream * mass.flux.conversion
    
    Q.dstream.prev <<- Q.dstream
    
    if(count == 1){
      tds.diff <- NA
      conc.diff <- NA
    } else{
      tds.diff <- tds.ustream - tds.dstream.prev 
      conc.diff <- conc.ustream - conc.dstream.prev
    }
    
    conc.dstream.prev <<- conc.dstream
    tds.dstream.prev <<- tds.dstream
    
    # error calculation
    # derivative terms
    d.conc.ustream <- -Q.ustream/(conc.gw - conc.dstream)
    d.conc.gw <- Q.ustream*(conc.ustream - conc.dstream)/
      (conc.gw - conc.dstream)^2
    d.conc.dstream <- Q.ustream*(conc.gw - conc.ustream)/
      (conc.gw - conc.dstream)^2  
    
    
    # standard error terms
    # set to se.gw is set to zero for now as there is only one gw.sc value
    if(sc.method == "interp_avg"){
      se.gw <- gw.dat1[gw.id, "gw_SC_sd"]
    } else{
      se.gw <- 0
    }
    se.ustream <- predict.lmdat$se.fit[1]
    se.dstream <- predict.lmdat$se.fit[2]
    
    # combining
    se.Q.gw <- sqrt(d.conc.gw^2 * se.gw^2 + d.conc.ustream^2 * se.ustream^2 + 
                      d.conc.dstream^2 * se.dstream^2)
    
    return.vals <- c(start.riverm, 
                     end.riverm,
                     riv.coord,
                     Q.ustream, 
                     Q.dstream, 
                     conc.ustream, 
                     conc.dstream, 
                     conc.gw, 
                     Q.gw, 
                     se.Q.gw, 
                     conc.diff,
                     tds.ustream,
                     tds.dstream,
                     tds.gw,
                     tds.diff,
                     reach.len, 
                     slope,
                     length(reach.id),
                     well.dist,
                     gw.stats.cv)
  } else{
    return.vals <- c(start.riverm,
                     end.riverm,
                     riv.coord,
                     rep(NA, 16),
                     gw.stats.cv)
  }
  
  return(return.vals)                  
}




ByRun <- function(index){
  sensor.fullfn <- sensor.fullfns[index]
  sensor.headers <- FileHeaders2Row(sensor.fullfn)
  sensor.dat <- FileDat2Row(sensor.fullfn, sensor.headers)
  
  run.strdate <- run.strdates[index]
  plot.strdate <- plot.strdates[index]
  sheet.strdate <- sheet.strdates[index]
  
  
  start.flow <- start.flows[index]
  
  if (special.q47 == T & index == 1) {
    #to account for rating curve shift
    start.flow <- 4.33247753  
  }
  
  print(start.flow)
  
  daily.flow <- daily.flows[index]
  
  count <- 0
  iter <<- 0
  
  by.reach <- mapply(ByReach, 1:length(seg.start), 
                     MoreArgs = list(sensor.dat = sensor.dat, 
                                     start.flow = start.flow,
                                     count = count),
                     SIMPLIFY = F)
  
  gw.estimates <- do.call(rbind, by.reach)
  
  date.col <- rep(plot.strdate, nrow(gw.estimates))
  flow.col <- rep(daily.flow, nrow(gw.estimates))
  
  out.dat <- cbind(gw.estimates, date.col, flow.col) 
  
  colnames(out.dat) <- out.colnames
  
  daily.flow.str <- sprintf("%04.f", daily.flow)
  
  if (zero.neg.qg == T) {
    out.fn <- paste("Q", daily.flow.str, "-", run.strdate, 
                    "-model_", regression.method, "_", sc.method, "-negQg0.csv", 
                    sep = "")  
    
  } else{
    out.fn <- paste("Q", daily.flow.str, "-", run.strdate, 
                    "-model_", regression.method, "_", sc.method, ".csv" , 
                    sep = "")  
  }
  
  out.fullfn <- paste(out.dir3, out.fn, sep = "") 
  

  #sheet.name <- paste("Q", daily.flow.str, "-", sheet.strdate, sep = "")
  sheet.name <- paste("Q", daily.flow.str, sep = "")
  
  
  if (index == 1) {
    #wb <- loadWorkbook(xlsx.fullfn, create = T)
    append.val = F
  } else{
    #wb <- loadWorkbook(xlsx.fullfn, create = F)
    append.val = T
  }
  
  #createSheet(wb, name = sheet.name)
  #writeWorksheet(wb, out.dat, sheet = sheet.name, startRow = 1, startCol = 1)
  #saveWorkbook(wb)
  
  write.xlsx(out.dat, xlsx.fullfn, sheetName = sheet.name, col.names = T, 
              row.names = F, append = append.val)
  
  write.table(out.dat, out.fullfn, sep = ",", quote = F, row.names = F, 
              col.names = T)
  

  
  
}

by.run <- mapply(ByRun, 1:length(sensor.fullfns), SIMPLIFY = F)
