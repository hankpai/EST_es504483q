
rm(list=ls(all=TRUE))

library(rgdal)

work.dir <- "D:/hank/btsync/hank_work/synoptic/data/maps/gEarth/"
longlat.fullfn <- paste(work.dir, "merced_riv-synoptic.txt", sep = "")
out.fullfn <- paste(work.dir, "merced_riv_utm-synoptic.csv", sep = "")

longlat.dat <- read.table(longlat.fullfn, sep = ",", as.is = T, header = F)

xy <- as.matrix(longlat.dat[, 1:2])

utm <- project(xy, "+proj=utm +zone=10 ellps=WGS84")

colnames(utm) <- c("Easting", "Northing")

write.table(utm, file = out.fullfn, sep = ",", quote = F, row.names = F,
            col.names = T)
