rm(list=ls(all=TRUE))

gw.sc.dir <- "D:/hank/btsync/hank_work/synoptic/data/groundwater/20150101-gw_spcond/"

current.day <- "1/30/2015"

gw.sc.fn <- "20150130-well_SC-buffer5000_all.csv"
gw.sc.fullfn <- paste(gw.sc.dir, gw.sc.fn, sep = "")

gw.buffer <- 5000

min.pts <- 5

yr.buffer <- strptime("1/1/1950", "%m/%d/%Y")


gw.sc.dat.all <- read.table(gw.sc.fullfn, header = T, sep = ",", as.is = T)

gw.sc.reach <- gw.sc.dat.all[gw.sc.dat.all[, "river_m"] > 2000 & 
                             gw.sc.dat.all[, "river_m"] < 40000, ] 

gw.sc.dist <- gw.sc.reach[gw.sc.reach[, "projected_dist_m"] < gw.buffer, ]

well.dates <- strptime(gw.sc.dat.all[, "DATE"], "%m/%d/%Y")

gw.sc.dat <- gw.sc.dist[well.dates >= yr.buffer, ]  

unique.well.names <- unique(gw.sc.dat[, "WELL.NAME"])




ByUniqueWell <- function(index){
  unique.well.name <- unique.well.names[index]
  
  unique.well.id <- which(gw.sc.dat[, "WELL.NAME"] == unique.well.name)
  
  unique.well.dat <- gw.sc.dat[unique.well.id, ]
  unique.well.sc <- gw.sc.dat[unique.well.id, "RESULT"]
  

  num.pts <- nrow(unique.well.dat)
  well.dist.to.riv <- unique.well.dat[1, "projected_dist_m"]
  well.riv.m <- unique.well.dat[1, "river_m"]
  
  

  
  
  
  
  if (num.pts >= min.pts) {
    sd.sc <- sd(unique.well.sc)
    mean.sc <- mean(unique.well.sc)
    cv.sc <- sd.sc/mean.sc
    
    return.dat1 <- c(sd.sc, mean.sc, cv.sc)
    
  } else{
    return.dat1 <- c(NA, NA, NA)  
    
  }
  
  return.dat <- c(unique.well.name, return.dat1, well.dist.to.riv, well.riv.m, 
                  num.pts)
  
  return(return.dat)
  
}




by.unique.well <- mapply(ByUniqueWell, 1:length(unique.well.names), 
                         SIMPLIFY = F)

out.dat <- do.call(rbind, by.unique.well)

colnames(out.dat) <- c("well_name", "sc_stdev", "sc_mean", "sc_coeffvar", 
                       "proj_dist_m", "river_m", "num_pts")



date.process.str <- strftime(Sys.Date(), "%Y%m%d")

out.fn <- paste(date.process.str, "-unique_well_SC_stats-buffer", gw.buffer, 
                "-n", min.pts, ".csv", sep = "")

out.fullfn <- paste(gw.sc.dir, out.fn, sep = "")

write.table(out.dat, out.fullfn, sep = ",", row.names = F, col.names = T)

