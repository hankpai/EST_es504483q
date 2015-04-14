# getting cdec CRS for upstream flow and MST for downstream flow comparisons
# Note, also grabbed stage

# takes in account lag time

rm(list=ls(all=TRUE))

own.rating <- F

sensors <- c("flow")
stations <- "MST"
sensor.val.col <- c(4, 4)

date.process.str <- strftime(Sys.Date(), format = "%Y%m%d")

# processing identifier
processing.step <- "stopsInletDepthFilters"
sonde <- "HL"

if (own.rating == T) {
  date.of.cdec.dl <- "20150313"  
} else{
  date.of.cdec.dl <- "2014-06-18"
}

# auxilary functions
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

# still vars, but change less often
cdec.dir <- "D:/hank/btsync/hank_work/synoptic/data/stationary/cdec/"

# time window for plot
start.date.str <- "2010-03-01"
end.date.str <- "2012-04-01"

start.date <- strptime(start.date.str, format = "%Y-%m-%d")
end.date <- strptime(end.date.str, format = "%Y-%m-%d")


# auxilary functions
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

# still vars, but change less often
cdec.dir <- "D:/hank/btsync/hank_work/synoptic/data/stationary/cdec/"
stationary.dirs <- paste(cdec.dir, sensors, "/", sep = "")

# flow data mainly to get date information for synoptic run info
#flow.dir <- "D:/hank/btsync/hank_work/synoptic/data/misc/daily_flows/"
#flow.fn <- "daily_flows-by_flow03.csv"
#flow.fullfn <- paste(flow.dir, flow.fn, sep = "")


# flow and date file
flow.fullfn <- "D:/hank/btsync/hank_work/synoptic/data/misc/daily_flows/daily_start_flows03.csv"

flow.headers <- FileHeaders2Row(flow.fullfn)
flow.dat <- FileDat2Row(flow.fullfn, flow.headers)

# columns
# 1: run id
# 2: day
# 3: flow cfs
# 4: flow cms
# 5: cdec start time
# 6: start cfs
# 7: start cms
# 8: sensor start time


flow.dates <- strptime(flow.dat[, 2], format = flow.headers[2, 2])
flow.dates.str <- strftime(flow.dates, format = "%Y%m%d")

# model files, one row of headers; interested in last rows
model.labs <- c("avg", "max", "min")

model.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/summary_gw_inflow/"
model.fns <- paste("Q_dstream_", model.labs, "_Q_gw-cms.csv", sep = "")
model.fullfns <- paste(model.dir, model.fns, sep = "")

# good model runs to analyze, flows in cfs
gw.cfs.runs <- c(47, 127, 159, 226, 231, 232, 261, 405, 455, 487, 574, 887,
                 1115, 1639, 2995, 3613, 5296)

#gw.cfs.runs <- c(127, 159, 226, 231, 405, 455, 487, 574, 1115, 
#                 1639, 2995, 3613, 5296)


# getting flow dates that correspond with model
flow.dates.with.mod.id <- flow.dat[, 3] %in% gw.cfs.runs

flow.dates.with.mod.str <- flow.dates.str[flow.dates.with.mod.id]

run.dates <- strptime(flow.dat[flow.dates.with.mod.id, 2],
                      format = flow.headers[2, 2])

run.dates.str <- strftime(run.dates, "%m/%d/%Y")

# basing synoptic run info from hydrolab data
hl.fns <- paste(flow.dates.with.mod.str, "-", processing.step, "_", sonde, 
                ".csv", sep = "")

synoptic.sub.dirs <- paste(flow.dates.with.mod.str, "-synoptic/organized/", 
                           sep = "")
synoptic.home.dir <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"
synoptic.dirs <- paste(synoptic.home.dir, synoptic.sub.dirs, sep = "")




synoptic.fullfns <- paste(synoptic.dirs, hl.fns, sep = "")


if (own.rating == T) {
  sensor.fns <- paste(date.of.cdec.dl, "-", stations, "-ownRating_", sensors, 
                      ".csv", sep = "")
  sensor.fullfns <- paste(stationary.dirs, sensor.fns, sep = "") 
} else{
  sensor.fns <- paste(date.of.cdec.dl, "_", stations, "_", sensors, ".csv",
                      sep = "")
  sensor.fullfns <- paste(stationary.dirs, sensor.fns, sep = "")
}

# adds delineations for runs
GetByRun <- function(index, sensor.vals, sensor.times, sensor.dat){
  # read in file
  synoptic.fullfn <- synoptic.fullfns[index]
  synoptic.headers <- read.table(synoptic.fullfn, as.is = T, sep = ",",
                                 nrows = 2)
  synoptic.dat <- read.table(synoptic.fullfn, as.is = T, sep = ",", skip = 2)
  colnames(synoptic.headers) <- synoptic.headers[1, ]
  colnames(synoptic.dat) <- synoptic.headers[1, ]
  
  run.date <- run.dates[index]
  
  run.date.str <- run.dates.str[index]
  
  
  # grab start and end times
  syn.gps.times <- strptime(synoptic.dat[, 2], format = synoptic.headers[2, 2], 
                            tz = "America/Los_Angeles")
  #syn.gps.times <- strptime(synoptic.dat[, 2], format = synoptic.headers[2, 2], 
  #                          tz = "Pacific/Pitcairn")
                            
  syn.gps.start <- syn.gps.times[1] + 60*60*24
  syn.gps.end1 <- syn.gps.times[length(syn.gps.times)]
  syn.gps.end <- syn.gps.end1 + 60*60*24
  
  cdec.dates <- strftime(sensor.times, format = "%m/%d/%Y")
  
  new.run.date.start <- run.date + 60*60*24
  new.run.date.end <- run.date + 60*60*(24+24)
  
  day.id <- which(cdec.dates == run.date.str)
  
  day.id <- which(sensor.times >= new.run.date.start & 
                    sensor.times <= new.run.date.end)
  
  
  #syn.gps.dates[day.id] 
  
  #browser()
  
  # gets cdec values for start and end
  cdec.period.id <- which(sensor.times >= syn.gps.start & 
                            sensor.times <= syn.gps.end)
  
  syn.gps.start.str <- strftime(syn.gps.start, "%Y-%m-%d %H:%M:%S")
  syn.gps.end.str <- strftime(syn.gps.end, "%Y-%m-%d %H:%M:%S")
  
  if(length(cdec.period.id) > 0){
    sensor.period.times <- sensor.times[cdec.period.id]
    cdec.period.dat <- sensor.vals[cdec.period.id]
    
    sensor.times.within <- sensor.times[cdec.period.id]
    
    cdec.closest.end.id <- which.min(abs(sensor.times.within - syn.gps.end))
    cdec.closest.end <- cdec.period.dat[cdec.closest.end.id]
    
    cdec.closest.start.id <- which.min(abs(sensor.times.within - syn.gps.start))
    cdec.closest.start <- cdec.period.dat[cdec.closest.start.id]
    
    cdec.avg <- mean(cdec.period.dat, na.rm = T)
    
    # hopefully max/min's aren't too wacky
    cdec.min <- min(cdec.period.dat, na.rm = T)
    cdec.max <- max(cdec.period.dat, na.rm = T) 
    
    
    
    closest.cdec.start.time <- sensor.period.times[cdec.closest.start.id]
    closest.cdec.end.time <- sensor.period.times[cdec.closest.end.id]
    
    closest.cdec.start.time.str <- strftime(closest.cdec.start.time, 
                                            "%Y-%m-%d %H:%M:%S")
    closest.cdec.end.time.str <- strftime(closest.cdec.end.time,
                                          "%Y-%m-%d %H:%M:%S")
    
    return.dat1 <- c(syn.gps.start.str, 
                    syn.gps.end.str, 
                    closest.cdec.start.time.str, 
                    closest.cdec.end.time.str, 
                    cdec.closest.start, 
                    cdec.closest.end, 
                    cdec.avg, 
                    cdec.min, 
                    cdec.max)   
    
  } else{
    return.dat1 <- c(syn.gps.start.str, syn.gps.end.str, 
                    NA, NA, NA, NA, NA, NA, NA)
    
  }
  
  
  if (length(day.id) > 0) {
    cdec.day.dat <- sensor.vals[day.id]  
    day.avg <- mean(cdec.day.dat, na.rm = T)
    day.min <- min(cdec.day.dat, na.rm = T)
    day.max <- max(cdec.day.dat, na.rm = T)
    
    #browser()
    
    return.dat <- c(return.dat1, day.avg, day.min, day.max)
  } else{
    return.dat <- c(return.dat1, NA, NA, NA)  
    
  }
  
  
  return(return.dat)
}


# sensors are referring to cdec sensors
CdecGet <- function(index){
  # start choosing file
  
  if (own.rating == T) {
    sensor.dat <- read.table(sensor.fullfns[index], sep = ",", as.is = T, 
                             header = T)
  } else{
    sensor.headers <- FileHeaders2Row(sensor.fullfns[index])
    sensor.dat <- FileDat2Row(sensor.fullfns[index], sensor.headers)
    
    #browser()
  }
  
  # good val filter
  sensor.filter.id <- which(sensor.dat[, 6] == T)
  
  # additional filter for stage at MST
  if(stations[index] == "MST"){  #& sensors[index] == "stage"){
    bad.stage.id1 <- which(sensor.dat[, 6] == F)
    bad.stage.id2 <- which(sensor.dat[, 3] == 99.99)
    bad.stage.id3 <- which(sensor.dat[, 3] < 10)
    
    all.bad.stage.ids <- c(bad.stage.id1, bad.stage.id2, bad.stage.id3)
    
    
    sensor.filter.id <- !(1:nrow(sensor.dat) %in% all.bad.stage.ids)
  }
  
  sensor.dat <- sensor.dat[sensor.filter.id, ]
  
  sensor.times <- strptime(sensor.dat[, 1], format = "%Y-%m-%d_%H:%M:%S",
                           tz = "Pacific/Pitcairn")
  sensor.vals <- sensor.dat[, sensor.val.col[index]]
  

  
  period.index <- which(sensor.times >= start.date & sensor.times <= end.date)
  
  sensor.dat.period <- sensor.dat[period.index, ]
  sensor.times.period <- sensor.times[period.index]
  sensor.vals.period <- sensor.vals[period.index]
  
  run.dat <- mapply(GetByRun, 1:length(synoptic.fullfns), 
                    MoreArgs = list(sensor.vals = sensor.vals.period, 
                                    sensor.times = sensor.times.period,
                                    sensor.dat = sensor.dat.period), 
                    SIMPLIFY = F)
  
  run.dat.organized <- do.call(rbind, run.dat)  
  
  colnames(run.dat.organized) <- c("syn_start_time", 
                                   "syn_end_time",
                                   "cdec_start_time",
                                   "cdec_end_time", 
                                   "closest_start",
                                   "closest_end", 
                                   "period_mean",
                                   "period_min",
                                   "period_max",
                                   "day_mean",
                                   "day_min",
                                   "day_max")  
  
  return(run.dat.organized)
  
}

cdec.dat <- mapply(CdecGet, 1:length(stations), SIMPLIFY = F)

out.dat1 <- do.call(rbind, cdec.dat)
out.dat2 <- cbind(run.dates.str, gw.cfs.runs, out.dat1)

processing.date <- strftime(Sys.Date(), "%Y%m%d")
out.fn <- paste(processing.date, "-", sensors, "_", stations, "_24hrlag.csv", sep = "")

out.dir <- "D:/hank/btsync/hank_work/synoptic/data/misc/station_closest_time_data/"

out.fullfn <- paste(out.dir, out.fn, sep = "")

write.table(out.dat2, out.fullfn, col.names = T, row.names = F, quote = F, 
            sep = ",")

