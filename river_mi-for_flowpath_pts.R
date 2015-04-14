# projects synoptic data onto river vector

rm(list=ls(all=TRUE))

library(spatstat)

write_file = T

#days_fn = "C:/Documents and Settings/hank/My Documents/My Dropbox/synoptic/data/misc/run_dates.csv"
#days = read.table(days_fn, sep = ",", as.is = T, skip = 2)
#days_titles = read.table(days_fn, sep = ",", as.is = T, nrow = 1)
#colnames(days) = days_titles
#date_strs = as.character(days[, "run_dates"])

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

proj_filter = 1000
             
#sensors = c("HL", "scan", "scanfp", "Fixed")
sensors = c("cdec_stations")


# define UTM window
x_range = c(679300, 709450)
y_range = c(4135100, 4146850)
dat_window = owin(x_range, y_range)

# location variables
rest_dir = "-synoptic/organized/"

#initial_dir = "C:/Documents and Settings/hank/My Documents/My Dropbox/synoptic/data/sensor/"
#sensor_dirs = paste(initial_dir, date_strs, rest_dir, sep = "")

sensor_dirs <- "D:/hank/btsync/hank_work/synoptic/data/misc/important_spatial_features/"
 
#map_dir = "C:/Documents and Settings/hank/My Documents/My Dropbox/synoptic/data/misc/merced river shapefile/"

map_dir = "D:/hank/btsync/hank_work/synoptic/data/maps/gEarth/"
map_fn = "merced_riv_utm-synoptic.csv"
map_fullfn = paste(map_dir, map_fn, sep = "")

map_dat = read.table(map_fullfn, sep = ",", as.is = T, header = T)

# psp object, 2-d line segment pattern
# x0, y0 => first endpoint of segment
# x1, y1 => second endpoint of segment
x0 = map_dat[1:(nrow(map_dat) - 1), "Easting"]
y0 = map_dat[1:(nrow(map_dat) - 1), "Northing"]
x1 = map_dat[2:nrow(map_dat), "Easting"]
y1 = map_dat[2:nrow(map_dat), "Northing"]
  
map_psp = psp(x0, y0, x1, y1, window = dat_window)

# calculates river meter by summing all the segments preceding and adding the fraction of the segment
# river meter is distance from the mouth of the Merced River
river_calc = function(index, segment_len, project_dat){
  #return_dat = sum(segment_len[1:(project_dat$mapXY[index] - 1)]) + (1 - project_dat$tp[index]) * segment_len[project_dat$mapXY[index]]
  
  return_dat = sum(segment_len[1:(project_dat$mapXY[index] - 1)]) + (project_dat$tp[index]) * segment_len[project_dat$mapXY[index]]
  return(return_dat)
}

#by_sensor = function(index, date_str, sensor_dir){
by_sensor = function(index, sensor_dir){
  # changed to python
  sensor = sensors[index]
  #sensor_fn = paste(date_str, "-pycombined_gps", sensor, ".csv", sep = "")
  sensor_fn <- paste(sensor, ".csv", sep = "")
  sensor_fullfn = paste(sensor_dir, sensor_fn, sep = "")
    
  # output variables
  #out_fn = paste(date_str, "-gpsRDist_", sensor, ".csv", sep = "")
  out_fn <- paste(sensor, "-gpsRDist.csv", sep = "")
  out_fullfn = paste(sensor_dir, out_fn, sep = "")
  print(out_fullfn)

  
  
  # reading in data
  if(file.exists(sensor_fullfn)){
    sensor_test = read.table(sensor_fullfn, sep = ",", as.is = T)
    if(nrow(sensor_test) > 2){
    
      sensor_headers = read.table(sensor_fullfn, sep = ",", as.is = T, nrows = 1, strip.white = T)
      sensor_dat = read.table(sensor_fullfn, sep = ",", as.is = T, skip = 1)
      colnames(sensor_dat) = sensor_headers[1, ]
      colnames(sensor_headers) = sensor_headers[1, ]  
      
      # ppp object, 2-d point pattern
      sensor_ppp = ppp(sensor_dat[, "Easting"], sensor_dat[, "Northing"], window = dat_window)
      
      # projecting dat
      project_dat = project2segment(sensor_ppp, map_psp)
      
      # length of each line segment
      segment_len = lengths.psp(map_psp)
    
      river_m = mapply(river_calc, 1:nrow(sensor_dat), MoreArgs = list(segment_len = segment_len, project_dat = project_dat))
      
      # convert to mile
      river_mile = 0.000621371192 * river_m
      
      # saves projected distance, filter later?
      proj_dist = project_dat$d
    
      # output
      if(write_file == T){
        dat_temp = cbind(sensor_dat, river_mile, river_m, proj_dist)
        new_dat = dat_temp[dat_temp[, ncol(dat_temp)] < proj_filter, ]
        
        pts_filtered = nrow(dat_temp) - nrow(new_dat)
        #out_msg = paste("pts filtered for ", date_str, ": ", pts_filtered, sep = "")
        #print(out_msg)
        #browser()
        
        #temp_headers = cbind(c("river_mile", "mi"), c("river_m", "m"), c("proj_dist", "m"))
        temp_headers <- c("river_mile", "river_m", "proj_dist")
        new_headers = c(sensor_headers, temp_headers)
        write.table(new_headers, file = out_fullfn, append = F, quote = F, sep = ",", row.names = F, col.names = F)
        write.table(new_dat, file = out_fullfn, append = T, quote = F, sep = ",", row.names = F, col.names = F)
        
      }
    }
  }
}

# flowpath
fp_utm = c(694491,4140047)
fp_ppp = ppp(fp_utm[1], fp_utm[2], window = dat_window)
project_fp = project2segment(fp_ppp, map_psp)
segment_len = lengths.psp(map_psp)
river_m = mapply(river_calc, 1, MoreArgs = list(segment_len = segment_len, project_dat = project_fp))



#by_day = function(index){
#sensor_mapply = mapply(by_sensor, 1:length(sensors), MoreArgs = list(date_str = date_strs[index], sensor_dir = sensor_dirs[index]))
sensor_mapply = mapply(by_sensor, 1:length(sensors), MoreArgs = list(sensor_dir = sensor_dirs))

#}


#project_loop = mapply(by_day, 1:length(date_strs))





 
 
 