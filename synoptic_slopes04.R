# finds slopes: endpt, linear, sens, and siegel slopes
# Sens and Siegel are calculated using a modified version of the mlbm package
# (modified for speed)
# last edit: 2014-05-20
# by: henry pai

rm(list=ls(all=TRUE))

# ===== USER VARIABLES =====

# sensor parameter
y.name <- "SpCond"
x.name <- "river_m"

# maximum river m seen in data
max.riverm <- 40000

# minimum river m seen in data
min.riverm <- 6000

# spacing interval for calculating slopes (m)
spacing <- 1000

# used to be gpsRDist
mid.step.id <- "stopsInletDepthFilters"

# string for dates
days.fn <- "D:/hank/btsync/hank_work/synoptic/data/misc/run_dates.csv"

days <- read.table(days.fn, sep = ",", as.is = T, skip = 2)
days.titles <- read.table(days.fn, sep = ",", as.is = T, nrow = 1)
colnames(days) = days.titles
date.strs <- as.character(days[days[, "analyze"] == "x", "run_dates"])

              
trend.types <- c("lin", "endpt", "sens")

slope.units <- paste(param, "/m", sep = "")

lin.names <- c("lin_slope", "lin_bound1", "lin_bound2", "lin_corr", "lin_p")
lin.units <- c(rep(slope.units, 3), "NA", "NA")
lin.headers <- rbind(lin.names, lin.units)

endpt.names <- c("endpt_slope")
endpt.units <- c(slope.units)
endpt.headers <- rbind(endpt.names, endpt.units)

sens.names <- c("sens_slope",
                "sens_bound1",
                "sens_bound2",
                "sens_rank1",
                "sens_rank2",
                "sens_num_slopes",
                "sens_var_S",
                "sens_MK_S",
                "sens_total_ties",
                "sens_q",
                "sens_n",
                "sens_pkg_slope",
                "sens_pkg_intercept",
                "sens_pkg_slope_bound1",
                "sens_pkg_slope_bound2",
                "sens_pkg_intercept_bound1",
                "sens_pkg_intercept_bound2",
                "siegel_slope",
                "siegel_intercept",
                "siegel_slope_bound1",
                "siegel_slope_bound2",
                "siegel_intercept_bound1",
                "siegel_intercept_bound2",
                "num_pts")

sens.units <- c(slope.units,
                slope.units,
                slope.units,
                "num",
                "num",
                "num",
                "num",
                "num",
                "num",
                "num",
                "num",
                slope.units,
                param,
                slope.units,
                slope.units,
                param,
                param,
                slope.units,
                param,
                slope.units,
                slope.units,
                param,
                param,
                "num")

sens.headers <- rbind(sens.names, sens.units)

# "independent" parameter, often unit of time/distance
denom <- "river_m"

# for 95% confidence interval
z.val <- 1.96



# locations of the river segmented by the spacing noted above
seg.start <- seq(max.riverm, min.riverm + spacing, by = -1 * spacing)
seg.end <- seq(max.riverm - spacing, min.riverm, by = -1 * spacing)
segment.id <- 1:length(seg.start)
segment.bounds <- cbind(segment.id, seg.start, seg.end)

segment.names <- c("segment_id", "start_seg", "end_seg")
segment.units <- c("num", "m", "m")
segment.headers <- rbind(segment.names, segment.units)

# correlation factors for linear regressions, will filter when plotting
# cor_filter = 0.0

# logfile location
log.dir <- "D:/hank/btsync/hank_work/synoptic/data/processing_logfiles/"
today.date.str <- strftime(Sys.Date(), "%Y%m%d")
log.fn.suffix <- "_synopticSlopes_logfile.txt"
log.fn <- paste(today.date.str, log.fn.suffix, sep = "")
log.fullfn <- paste(log.dir, log.fn, sep = "")

# logfile function
Logfile <- function(log.str, append.val = T){
  print(log.str)
  cat(log.str, file = log.fullfn, append = append.val, fill = T)
}

# ===== FILE VARIABLES =====



# synoptic file directories & names
syn.dir1 <- "D:/hank/btsync/hank_work/synoptic/data/sensor/"
syn.dir2 <- paste(syn.dir1, date.strs, "-synoptic/organized/", sep = "")

hl.fns <- paste(date.strs, "-", mid.step.id, "_HL.csv", sep = "")
hl.fullfns <- paste(syn.dir2, hl.fns, sep = "")

scan.fns <- paste(date.strs, "-", mid.step.id, "_scan.csv", sep = "")
scan.fullfns <- paste(syn.dir2, scan.fns, sep = "")

# output file directories & names
spacing.str <- sprintf("%04.f", spacing) 
out.dir <- paste("D:/hank/btsync/hank_work/synoptic/data/analysis/slopes/segments_", 
                  mid.step.id, "/", sep = "")

# ===== PROCESSING =====

# ===== SEN'S

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









# estimate median slope from Sen's method, more info: 
# http://www.webapps.cee.vt.edu/ewr/environmental/teach/smprimer/sen/sen.html
GetSlope <- function(index, segment.dat){
  data.one <- segment.dat[index, ]
  other.data <- segment.dat[(index + 1):nrow(segment.dat), ]

  # negative indicates increase as one travels downstream (because denominator 
  # is always negative)
  slopes <- (other.data[, param] - data.one[, param])/
            (other.data[, denom] - data.one[, denom])
  return(slopes)
}

# calculates the summation term in Sen's method variance
VarianceSummation <- function(index, segment.dat, tie.vals){
  t.p <- length(which(segment.dat[, param] == tie.vals[index]))
  sum.term <- t.p * (t.p - 1) * (2*t.p + 5)

  return(sum.term)
}

FindSens <- function(segment.dat, num.pts){
  if(num.pts < 10){
    sens.dat <- rep(NA, 23)
  } else{
    # calculates row of slopes according to sen's method
    row.slopes <- mapply(GetSlope, 1:(nrow(segment.dat) - 1), 
                         MoreArgs = list(segment.dat = segment.dat))
    all.slopes <- do.call(c, row.slopes)

    # finding Mann-Kendall S
    pos <- length(which(all.slopes > 0))
    neg <- length(which(all.slopes < 0))
    mk.S <- pos - neg
    
    # finds number of ties
    tie.id <- duplicated(segment.dat[, param])
    tie.values <- unique(segment.dat[tie.id, param])
    q.val <- length(tie.values)
    
    # outputs ties
    total.ties <- length(which(segment.dat[, param] %in% tie.values == T))
    
    # gets summation term or assigns 0 for no ties
    if(length(tie.values) != 0){
      var.sum.terms <- mapply(VarianceSummation, 1:q.val, 
                              MoreArgs = list(segment.dat = segment.dat, 
                              tie.vals = tie.values))
      total.sum <- sum(var.sum.terms)
    } else{
      total.sum <- 0
    }

    # same n as in equation, number of data points
    n <- length(segment.dat[, param])

    variance.S <- (1/18) * (n * (n - 1) * (2 * n + 5) - total.sum)

    conf.int <- z.val * sqrt(variance.S)

    num.slopes <- length(all.slopes)
    M1 <- (num.slopes - conf.int)/2
    M2 <- (num.slopes + conf.int)/2

    rank1 <- floor(M1)
    rank2 <- ceiling(M2 + 1)

    ordered.slopes <- sort(all.slopes)

    bound1.slope <- ordered.slopes[rank1]
    bound2.slope <- ordered.slopes[rank2]

    median.slope <- median(all.slopes, na.rm = T)
    
    # test packages   
    y <- segment.dat[, 2]
    x <- segment.dat[, 1]
    sens.mod <- MblmHp(y ~ x, repeated = F)
    
    
    siegel.mod <- MblmHp(y ~ x)
    
    sens.test.slope <- sens.mod$coefficients[2]
    sens.test.intercept <- sens.mod$coefficients[1]
    
    
    siegel.test.slope <- siegel.mod$coefficients[2]
    siegel.test.intercept <- siegel.mod$coefficients[1]
    
    # 4 different error stats    
    confint.sens <- confint(sens.mod)
    confint.sens.intercept <- confint.sens[1, ]
    confint.sens.slope <- confint.sens[2, ]
    
    
    confint.siegel <- confint(siegel.mod)
    confint.siegel.intercept <- confint.siegel[1, ]
    confint.siegel.slope <- confint.siegel[2, ]
    
    print(confint.siegel)
    
    
    
    sens.dat <- c(median.slope, 
                  bound1.slope, 
                  bound2.slope, 
                  rank1, 
                  rank2, 
                  num.slopes, 
                  variance.S, 
                  mk.S, 
                  total.ties, 
                  q.val, 
                  n,
                  sens.test.slope,          # +2
                  sens.test.intercept,
                  confint.sens.slope,       # +4
                  confint.sens.intercept,
                  siegel.test.slope,        # +2
                  siegel.test.intercept,
                  confint.siegel.slope,     # +4
                  confint.siegel.intercept)
  }
  
  sens.dat <- c(sens.dat, num.pts)
  
  return(sens.dat)
}

# ===== LINEAR
# returns p-value, from: 
# http://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression 

Lmp <- function (modelobject) {
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    f <- summary(modelobject)$fstatistic
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    attributes(p) <- NULL
    return(p)
}


FindLin <- function(segment.dat, num.pts){
  # returns slopes, 95% confidence intervals, and correlation (R)
  
  print(paste("pts", num.pts))

  # needs at least 2 pts for regressions
  if(num.pts < 2){
    lin.dat <- rep(NA, 4)
  } else{
    # linear regression
    lin.mod <- lm(segment.dat[, param] ~ segment.dat[, denom])
    lin.slope <- lin.mod$coefficients[2]

    p.val <- Lmp(lin.mod)
    
    
    
    #glm.mod <- glm(segment.dat[, param] ~ segment.dat[, denom])
    #browser()
    
    # statistical info
    lin.conf <- confint(lin.mod)[2, ]
    stats.lin <- summary(lin.mod, correlation = T)
    
    #browser()
    #cor.lin <- sqrt(stats_lin$r.squared)
    
    cor.lin <- cor(segment.dat[, denom], segment.dat[, param])

    if(is.na(cor.lin) == F){
      lin.dat <- c(lin.slope, lin.conf, cor.lin, p.val)
    } else{
      lin.dat <- c(lin.slope, lin.conf, NA, p.val)
    }
  }
  return(lin.dat)
}

# ===== ENDPT
FindEndpt <- function(segment.dat, num.pts){
  # needs at least 2 pts for regressions
  if(num.pts < 2){
    endpt.slope <- NA
  } else{
    # endpt regression
    endpt.slope <- (segment.dat[num.pts, param] - segment.dat[1, param])/
                   (segment.dat[num.pts, denom] - segment.dat[1, denom])
  }
  return(endpt.slope)
}

# ===== LOOP by Trend Type
ByTrend <- function(index, segment.dat, num.pts){
  if(trend.types[index] == "endpt"){
    slope.dat <- FindEndpt(segment.dat, num.pts)
  } else if(trend.types[index] == "lin"){
    slope.dat <- FindLin(segment.dat, num.pts)
  } else if(trend.types[index] == "sens"){
    slope.dat <- FindSens(segment.dat, num.pts)
  }
    
  return(slope.dat)
}

# ===== LOOP by Segments
BySegment <- function(index, sensor.dat){
  start.dist <- segment.bounds[index, 2]
  end.dist <- segment.bounds[index, 3]

  segment.dat <- sensor.dat[sensor.dat[, denom] >= end.dist & 
                            sensor.dat[, denom] <= start.dist, ]
                            
  num.pts <- nrow(segment.dat)
  
  slope.dat <- mapply(ByTrend, 1:length(trend.types), 
                      MoreArgs = list(segment.dat, num.pts))
  
  out.msg <- paste("segment loop", index)
  print(out.msg)
  
  #browser()
  
  return(slope.dat)
}

# ===== LOOP by Day
ByDay <- function(index){
  if(index == 1){
    append.val <- F
  } else{
    append.val <- T
  }
  Logfile(paste("==========", date.strs[index], "=========="), append.val)

  # read in data
  hl.headers <- read.table(hl.fullfns[index], sep = ",", nrows = 2, as.is = T)
  hl.dat <- read.table(hl.fullfns[index], sep = ",", skip = 2, as.is = T)
  colnames(hl.headers) <- hl.headers[1, ]
  colnames(hl.dat) <- hl.headers[1, ]

  scan.headers <- read.table(scan.fullfns[index], sep = ",", nrows = 2, 
                             as.is = T)
  scan.dat <- read.table(scan.fullfns[index], sep = ",", skip = 2, as.is = T)
  colnames(scan.headers) <- scan.headers[1, ]
  colnames(scan.dat) <- scan.headers[1, ]

  # find parameter
  hl.find <- length(which(colnames(hl.dat) == param))
  scan.find <- length(which(colnames(scan.dat) == param))

  # assign if it's in the hydrolab file
  if(hl.find != 0 & scan.find == 0){
    col.id <- which(colnames(hl.dat) == param)
    dist.id <- which(colnames(hl.dat) == denom)

    # error- no distance data found
    if(length(dist.id) == 0){
      out.msg <- paste(denom, " column not found", sep = "")
      Logfile(out.msg)
      break
    }

    # records easting/northing, distance, and sensor parameter data
    col.dat <- hl.dat[, c(dist.id, col.id)]
  } else if(scan.find != 0 & hl.find == 0){
  # assign if it's in the s:can file
    col.id <- which(colnames(scan.dat) == param)
    dist.id <- which(colnames(scan.dat) == denom)

    if(length(dist.id) == 0){
      out.msg <- paste(denom, " column not found", sep = "")
      Logfile(out.msg)
      break
    }
    col.dat <- scan.dat[, c(dist.id, col.id)]
  } else{
    out.msg <- paste(param, " column not found", sep = "")
    Logfile(out.msg)
    break
  }

  # finds slopes by trend method
  trend.dat <- mapply(BySegment, 1:nrow(segment.bounds), 
                      MoreArgs = list(sensor.dat = col.dat), SIMPLIFY = T)
                      
 
  # organizing for output
  for(j in 1:length(trend.types)){
    trend.type <- trend.types[j]
    trend.id <- which(trend.types == trend.type)
    
    if(trend.type == "lin"){
      lin.dat <- data.frame(do.call(rbind, trend.dat[trend.id, ]))
    } else if(trend.type == "endpt"){
      endpt.dat <- data.frame(do.call(rbind, trend.dat[trend.id, ]))
    } else if(trend.type == "sens"){
      sens.dat <- data.frame(do.call(rbind, trend.dat[trend.id, ]))
    }
  }
  
   
  headers <- cbind(segment.headers, endpt.headers, lin.headers, sens.headers)
  organized.dat <- cbind(segment.bounds, endpt.dat, lin.dat, sens.dat)
  
  fn <- paste(date.strs[index], "-slopes_seg", spacing.str, "_", param, ".csv", 
              sep = "")
  fullfn <- paste(out.dir, fn, sep = "")
  
  write.table(headers, fullfn, sep = ",", append = F, quote = F, row.names = F, 
              col.names = F)
  write.table(organized.dat, fullfn, sep = ",", append = T, quote = F, 
              row.names = F, col.names = F)     
}

dat <- mapply(ByDay, 1:length(date.strs), SIMPLIFY = F)




