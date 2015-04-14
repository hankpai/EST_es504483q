# projects synoptic data onto river vector
# editted: 4/29/2014
# changed for btsync directory and adding logfile information

rm(list=ls(all=TRUE))

library(spatstat)

write.file <- T

days.fn <- "D:/hank/btsync/hank_work/synoptic/data/misc/run_dates.csv"
days <- read.table(days.fn, sep = ",", as.is = T, skip = 2)
days.titles <- read.table(days.fn, sep = ",", as.is = T, nrow = 1)
colnames(days) <- days.titles
date.strs <- as.character(days[, "run_dates"])

#date_strs = c("20120217","20120323")

# enter date: YYYYMMDD
#date_strs = c("20100331",
#              "20100809",
#              "20110303",
#              "20110407",
#              "20110421",
#              "20110506",
#              "20110607",
#              "20110623",
#              "20110728",
#              "20110811",
#              "20110907",
#              "20110920")

proj.filter <- 50
             
sensors <- c("HL", "scan", "scanfp", "Fixed")

# define UTM window
x.range <- c(679000, 710050)
y.range <- c(4135000, 4147000)
dat.window <- owin(x.range, y.range)

# file directory variables
rest.dir <- "-synoptic/organized/"
initial.dir <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"
sensor.dirs <- paste(initial.dir, date.strs, rest.dir, sep = "")
 
map.dir <- "D:/hank/btsync/hank_work/synoptic/data/maps/gEarth/"
map.fn <- "merced_riv_utm-synoptic.csv"
map.fullfn <- paste(map.dir, map.fn, sep = "")

# logfile location
log.dir <- "D:/hank/btsync/hank_work/synoptic/data/processing_logfiles/"
today.date.str <- strftime(Sys.Date(), "%Y%m%d")
log.fn.suffix <- "_river-mile_logfile.txt"
log.fn <- paste(today.date.str, log.fn.suffix, sep = "")
log.fullfn <- paste(log.dir, log.fn, sep = "")


# logfile function
Logfile <- function(log.str, append.val){
  print(log.str)
  cat(log.str, file = log.fullfn, append = append.val, fill = T)
}
                                                   
map.dat <- read.table(map.fullfn, sep = ",", as.is = T, header = T)

# psp object, 2-d line segment pattern
# x0, y0 => first endpoint of segment
# x1, y1 => second endpoint of segment
x0 <- map.dat[1:(nrow(map.dat) - 1), "Easting"]
y0 <- map.dat[1:(nrow(map.dat) - 1), "Northing"]
x1 <- map.dat[2:nrow(map.dat), "Easting"]
y1 <- map.dat[2:nrow(map.dat), "Northing"]
  
map.psp <- psp(x0, y0, x1, y1, window = dat.window)

# calculates river meter by summing all the segments preceding and adding the fraction of the segment
# river meter is distance from the mouth of the Merced River
RiverCalc <- function(index, segment.len, project.dat){
  #return_dat = sum(segment_len[1:(project_dat$mapXY[index] - 1)]) + (1 - project_dat$tp[index]) * segment_len[project_dat$mapXY[index]]
  
  return.dat <- sum(segment.len[1:(project.dat$mapXY[index] - 1)]) + (project.dat$tp[index]) * segment.len[project.dat$mapXY[index]]
  return(return.dat)
}

BySensor <- function(index, date.str, sensor.dir){
  # changed to python
  sensor <- sensors[index]
  
  Logfile(paste("----------", sensor, "----------"), append.val = T)
  
  sensor.fn <- paste(date.str, "-pycombined_gps", sensor, ".csv", sep = "")
  sensor.fullfn <- paste(sensor.dir, sensor.fn, sep = "")
    
  # output variables
  out.fn <- paste(date.str, "-gpsRDist_", sensor, ".csv", sep = "")
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
     
      # ppp object, 2-d point pattern
      sensor.ppp <- ppp(sensor.dat[, "Easting"], sensor.dat[, "Northing"], 
                        window = dat.window)
      
      # projecting dat
      project.dat <- project2segment(sensor.ppp, map.psp)
      
      # length of each line segment
      segment.len <- lengths.psp(map.psp)
      #print(segment.len)
    
      river.m <- mapply(RiverCalc, 1:nrow(sensor.dat), 
                        MoreArgs = list(segment.len = segment.len, 
                        project.dat = project.dat))
      
      # convert to mile
      river.mile <- 0.000621371192 * river.m
      
      # saves projected distance, filter later?
      proj.dist <- project.dat$d
    
      # output
      if(write.file == T){
        dat.temp <- cbind(sensor.dat, river.mile, river.m, proj.dist)
        new.dat <- dat.temp[dat.temp[, ncol(dat.temp)] < proj.filter, ]
               
        pts.filtered <- nrow(dat.temp) - nrow(new.dat)
        Logfile(paste("points filtered for being ", proj.filter,
                " m away from shapefile line: ", pts.filtered, "\n", 
                sep = ""), append.val = T)
        
        
        temp.headers <- cbind(c("river_mile", "mi"), c("river_m", "m"), 
                              c("proj_dist", "m"))
        new.headers <- cbind(sensor.headers, temp.headers)
        write.table(new.headers, file = out.fullfn, append = F, quote = F, 
                    sep = ",", row.names = F, col.names = F)
        write.table(new.dat, file = out.fullfn, append = T, quote = F, 
                    sep = ",", row.names = F, col.names = F)
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





 
 
 