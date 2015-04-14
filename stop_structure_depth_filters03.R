# filter for stop spots
# two ways to implement:
# 1. easy is to take stops and apply a upstream & downstream distance buffer
#     This will be implemented as plotted speeds did not appear to give
#     anticipated algorithm
# 2. harder one is to find stops from deceleration to acceleration transitions
# 3. filter out after last major structure
# editted: 5/8/2014
# changed for btsync directory and adding logfile information

rm(list=ls(all=TRUE))

# output filtered file, probably always true
write.file = T

# filter distances in meters
upstream.dist.filter <- 600
downstream.dist.filter <- 600

# depth standard deviations filter
depth.col.ids <- c("Dep25", "Depth")
sd.num <- 2

# sensors to be filtered
sensors <- c("HL", "scan")

# battery loading zones
stop.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/transition_stops-gpsRDist.csv"

# major canals
major.inputs.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/large_structures-gpsRDist.csv"

stops.dat <- read.table(stop.fullfn, sep = ",", as.is = T, header = T)
major.inputs.dat <- read.table(major.inputs.fullfn, sep = ",", as.is = T,
                               header = T)


# filter for last input
last.input.riverm <- major.inputs.dat[which.min(major.inputs.dat[, "river_m"]), 
                                      "river_m"]

# flow days
days.fn <- "D:/hank/btsync/hank_work/synoptic/data/misc/run_dates.csv"
days <- read.table(days.fn, sep = ",", as.is = T, skip = 2)
days.titles <- read.table(days.fn, sep = ",", as.is = T, nrow = 1)
colnames(days) <- days.titles
date.strs <- as.character(days[, "run_dates"])

# file directory variables
rest.dir <- "-synoptic/organized/"
initial.dir <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"
sensor.dirs <- paste(initial.dir, date.strs, rest.dir, sep = "")

# logfile location
log.dir <- "D:/hank/btsync/hank_work/synoptic/data/processing_logfiles/"
today.date.str <- strftime(Sys.Date(), "%Y%m%d")
log.fn.suffix <- "_stopDepthFilter_logfile.txt"
log.fn <- paste(today.date.str, log.fn.suffix, sep = "")
log.fullfn <- paste(log.dir, log.fn, sep = "")

# logfile function
Logfile <- function(log.str, append.val){
  print(log.str)
  cat(log.str, file = log.fullfn, append = append.val, fill = T)
}

StopsFilter <- function(index, sensor.dat){
  stop.riverm <- stops.dat[index, "river_m"]
  
  longitudinal.dists <- sensor.dat[, "river_m"]
  
  # filter for stops, recall upstream has higher river distance
  filter.ids <- which(longitudinal.dists <= stop.riverm + upstream.dist.filter & 
                      longitudinal.dists >= stop.riverm - downstream.dist.filter)
  
  
  return(filter.ids)
                        
 


}


BySensor <- function(index, date.str, sensor.dir){
  # changed to python
  sensor <- sensors[index]
  
  Logfile(paste("----------", sensor, "----------"), append.val = T)
  
  sensor.fn <- paste(date.str, "-gpsRDist_", sensor, ".csv", sep = "")
  sensor.fullfn <- paste(sensor.dir, sensor.fn, sep = "")
    
  # output variables
  out.fn <- paste(date.str, "-stopsInletDepthFilters_", sensor, ".csv", sep = "")
  out.fullfn <- paste(sensor.dir, out.fn, sep = "")
  
  Logfile(paste("opening:",  sensor.fullfn), T)
  
  
  
  # reading in data
  if(file.exists(sensor.fullfn)){
    sensor.test <- read.table(sensor.fullfn, sep = ",", as.is = T)
    if(nrow(sensor.test) > 2){
    
      sensor.headers <- read.table(sensor.fullfn, sep = ",", as.is = T, 
                                   nrows = 2, strip.white = T)
      sensor.dat <- read.table(sensor.fullfn, sep = ",", as.is = T, skip = 2)
      colnames(sensor.dat) <- sensor.headers[1, ]
      colnames(sensor.headers) <- sensor.headers[1, ]  
     
      
      # identifying row ids of stop filters
      stop.filtered.ids <- mapply(StopsFilter, 1:nrow(stops.dat), 
                                  MoreArgs = list(sensor.dat = sensor.dat), 
                                  SIMPLIFY = F)
                                  
      #browser()
      
      stop.filtered.ids.vector <- do.call(c, stop.filtered.ids)
      
      stops.pts.filtered <- length(stop.filtered.ids.vector)
     
      stops.good.rows <- !(1:nrow(sensor.dat) %in% stop.filtered.ids.vector)
      
      # resulting data frame
      stops.filtered.dat <- sensor.dat[stops.good.rows, ]
      #browser()
      
      # depth filter
      depth.col.find <- which(depth.col.ids %in% colnames(stops.filtered.dat))
      
      if(length(depth.col.find) > 0){
        depth.col.id <- depth.col.ids[depth.col.find]
        depth.dat <- stops.filtered.dat[, depth.col.id]
        depth.mean <- mean(depth.dat, na.rm = T)
        depth.sd.filter <- sd.num * sd(depth.dat, na.rm = T)
        depth.upper <- depth.mean + depth.sd.filter
        depth.lower <- depth.mean - depth.sd.filter
        
        depth.filtered.ids <- c(which(depth.dat < depth.lower),  
                                which(depth.dat > depth.upper))
        
        depth.pts.filtered <- length(depth.filtered.ids)
        if(depth.pts.filtered > 0){
          #browser()
          
          depth.good.rows <- !(1:nrow(stops.filtered.dat) %in% 
                               depth.filtered.ids)
        } else{
          depth.good.rows <- 1:nrow(stops.filtered.dat)
        }
          
        
        depth.filtered.dat <- stops.filtered.dat[depth.good.rows, ]
      } else{
        depth.filtered.dat <- stops.filtered.dat
        depth.pts.filtered <- 0
        depth.sd.filter <- NA
        depth.mean <- NA 
      
      }
      
      
      
      
      
      
      # filtering after last inlet
      last.inlet.filter.dat <- depth.filtered.dat[depth.filtered.dat[, "river_m"] > last.input.riverm, ] 
      
    
      # output
      if(write.file == T){
        
        Logfile(paste("points filtered for being ", upstream.dist.filter,
                " m upstream from stop and ", downstream.dist.filter, 
                " m downstream from stop: ", stops.pts.filtered, "\n", 
                sep = ""), append.val = T)
                
        Logfile(paste("points filtered for being ", sd.num, 
                " stdevs from mean depth: ", depth.pts.filtered, "\n", 
                sep = ""), append.val = T)
        Logfile(paste("mean depth: ", depth.mean, ", filter range from depth: ", 
                depth.sd.filter, sep = ""), append.val = T)
        
        
        write.table(sensor.headers, file = out.fullfn, append = F, quote = F, 
                    sep = ",", row.names = F, col.names = F)
        write.table(last.inlet.filter.dat, file = out.fullfn, append = T, 
                    quote = F, sep = ",", row.names = F, col.names = F)
      }
    }
  }
}

# flowpath
#fp.utm <- c(694491,4140047)
#fp.ppp <- ppp(fp.utm[1], fp.utm[2], window = dat_window)
#project.fp <- project2segment(fp.ppp, map.psp)
#segment.len <- lengths.psp(map.psp)
#river.m <- mapply(RiverCalc, 1, MoreArgs = list(segment.len = segment.len, 
#                    project.dat = project.fp))



ByDay <- function(index){

  if(index == 1){
    append.val <- F
  } else{
    append.val <- T
  }
  Logfile(paste("==========", date.strs[index], "=========="), append.val)
  
   
  sensor.mapply <- mapply(BySensor, 1:length(sensors), 
                          MoreArgs = list(date.str = date.strs[index], 
                          sensor.dir = sensor.dirs[index]))
}


project.loop = mapply(ByDay, 1:length(date.strs))




