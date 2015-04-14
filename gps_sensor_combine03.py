# rewrite of 20110607_synoptic.R
# Henry Pai
# last major edit: 4/29/2014
# revision(s)
# - added log files for points filtered etc.

import time
from datetime import datetime, timedelta
from numpy import *
import string
import csv

# ===== user vars =====

sensors = ["HL", "scan", "scanfp"]

generic_tString = "%Y/%m/%d_%H:%M:%S"

# days of use
# no for 3/3/2011 (handheld), 5/20/2011 (bad run), 6/7/2011 (handheld)
#day = ["20110303", "20110920"]
#days = ["20100331", "20100809", "20110303", "20110407", "20110421", "20110506", "20110607", "20110623", "20110728", "20110811", "20110907", "20110920"]

# this can be adjusted
speed_filter = 0

pos_tFormat = ["%d/%m/%Y %H:%M:%S.%f", "%d/%m/%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S.%f", "%Y/%m/%d_%H:%M:%S"]

# log file definition
today = datetime.today()
today_str = datetime.strftime(today, "%Y%m%d")

log_dir = "D:/hank/btsync/hank_work/synoptic/data/processing_logfiles/"
log_fn_suffix = "_gps-sensor-combine_logfile.txt"
log_fullfn = log_dir + today_str + log_fn_suffix

# ====== functions =====
def LogOutput(out_str):
    print out_str
    log_fHandler.write(out_str + "\n")
    log_fHandler.flush()

# ====== main =====


days_fn = "D:/hank/btsync/hank_work/synoptic/data/misc/run_dates.csv"
days_fHandler = open(days_fn, "rU")
days = []


for k, line in enumerate(days_fHandler.readlines()):
    line_noEndline = line.replace("\n", "")
    split = line_noEndline.split(",")

    if k > 1:
        days.append(str(split[0]))
days_fHandler.close()

#days = []
#days = ["20120217", "20120323"]

log_fHandler = open(log_fullfn, "w")



for j in range(len(days)):
    day = days[j]

    day_datetime = datetime.strptime(day, "%Y%m%d").date()

    LogOutput("========== " + day + " ==========")
        
    #work_dir = "C:/Documents and Settings/hank/My Documents/My Dropbox/synoptic/data/sensor/" + day + "-synoptic/organized/"
    #work_dir = "C:/Documents and Settings/hank/My Documents/Dropbox/synoptic/" + day + "-synoptic/organized/"

    # windows 7 file directory setup
    #D:\hank\btsync\hank_work\synoptic\data\sensor\20100331-synoptic
    work_dir = "D:/hank/btsync/hank_work/synoptic/data/sensor/" + day + "-synoptic/organized/"

    file_prefix = day + "-synoptic_"
    file_postfix = ".csv"
    
    # ----- read in gps file -----
    pos_file = work_dir + file_prefix + "gps" + file_postfix
    pos_fHandler = open(pos_file, "rU")
    pos_dat = []
    pos_time = []
    pos_decTime = []
    pos_strTime = []
    pos_east = []
    pos_north = []
    fix_mark = []
    speed = []

    zero_diff = 0

    LogOutput("opening: " + pos_file)
    
    for i, line in enumerate(pos_fHandler.readlines()):
        line_noEndline = line.replace("\n", "")
        
        # removes endline character
        split = line_noEndline.split(",")

        # gets fixed mark column number
        if i == 0:
            east_col = split.index("Easting")
            north_col = split.index("Northing")
            try:
                fix_col = split.index("Fix Mark")
                
            except:
                fix_col = False
                LogOutput('no fix mark column')
                
        pos_dat.append(split[1:3])

        if i <=1:
            fix_mark.append(split[1:3])
        elif i > 1:
            # records easting and northing for speed
            pos_east.append(float(split[east_col]))
            pos_north.append(float(split[north_col]))

            # reading in time
            for j in range(len(pos_tFormat)):
                # accounting for several time formats
                try:
                    day_temp = datetime.strptime(str(split[0]), pos_tFormat[j]).date()
                    if day_temp == day_datetime:
                        time_temp = datetime.strptime(str(split[0]), pos_tFormat[j])
                except:
                    pass

            try:
                time_temp
            except:
                time_temp = False

            if time_temp != False:
                pos_decTime.append(time.mktime(time_temp.timetuple()) + time_temp.microsecond/1000000.0)

                # stored for strftime later
                pos_time.append(time_temp)

                # unnecessary, but can check
                pos_strTime.append(str(split[0]))

                # stores fix mark points
                if fix_col != False:
                    if split[len(split) - 1] != "0":
                        fix_mark.append(split[1:3])
            else:
                LogOutput('no time format worked')

            if i > 2:
                # calculating speed
                dist_temp = pow(pow(pos_east[i-2] - pos_east[i-3], 2) + pow(pos_north[i-2] - pos_north[i-3], 2), 0.5)
                time_diff_secs = pos_decTime[i-2] - pos_decTime[i-3]
                if time_diff_secs == 0:
                    zero_diff = zero_diff + 1
                    speed_temp = 0
                else:
                    speed_temp = dist_temp/time_diff_secs
                speed.append(speed_temp)
                

    pos_fHandler.close()

    
    # storing times as array so numpy/vector math can be applied
    pos_timeArray = array(pos_decTime)
    ave_speed = average(speed)

    

    LogOutput("average speed: " + str(ave_speed))
    LogOutput("number of zero time differences: " + str(zero_diff))

    # output for fix marks
    if fix_col != False:
        fixOut_fullfn = work_dir + day + "-pycombined_gpsFixed.csv"
        fixOut_fHandler = open(fixOut_fullfn, "w")

        for i in range(len(fix_mark)):
            temp = fix_mark[i][0] + "," + fix_mark[i][1] + "\n"
            fixOut_fHandler.write(temp)
            fixOut_fHandler.flush()
            
        fixOut_fHandler.close()

        
          

    for k in range(len(sensors)):
        # ----- read in sensor file -----
        sensor = sensors[k]
        LogOutput("---------- " + sensor + " ----------")
        out_fullfn = work_dir + day + "-pycombined_gps" + sensor + ".csv"
        
        sensor_file = work_dir + file_prefix + sensor + file_postfix
        sensor_fHandler = open(sensor_file, "rU")
        sensor_dat = []
        sensor_time = []
        sensor_decTime = []
        sensor_strTime = []
        interval_time = []

        LogOutput("opening: " + sensor_file)

        filter_count = 0

        for i, line in enumerate(sensor_fHandler.readlines()):
            split = line.split(",")

            # finding filter columns
            if i == 0:
                # column to start registering data
                if sensor == "HL":
                    sensor_colStart = split.index("Temp")
                elif sensor == "scan":
                    sensor_colStart = split.index("Turbid")
                elif sensor == "scanfp":
                    sensor_colStart = split.index("Status_0")
                
                if sensor == "HL":
                    filter_col = split.index("SpCond")
                elif sensor == "scan":
                    no3_col = split.index("NO3-Neq")
                    filter_col = split.index("Turbid")
            
                # storing line of data
                sensor_dat.append(split[sensor_colStart:len(split)])
            elif i == 1:
                # time for hydrolab is split into 2 columns; 1 for scan
                if sensor_colStart == 2:
                    sensor_tFormat = str(split[0]) + "_" + str(split[1])  
                #elif (sensor == "scan") | (sensor == "scanfp"):
                elif sensor_colStart == 1:
                    sensor_tFormat = str(split[0])

                sensor_dat.append(split[sensor_colStart:len(split)])
            
            elif i > 1:

                if sensor_colStart == 2:
                    time_temp = datetime.strptime(str(split[0]) + "_" + str(split[1]), sensor_tFormat)
                elif sensor_colStart == 1:
                    time_temp = datetime.strptime(str(split[0]), sensor_tFormat)
                
                # filters for hydrolab, scan, none for scanfp... probably can functionalize this part
                if sensor == "HL":
                    #time_temp = datetime.strptime(str(split[0]) + "_" + str(split[1]), sensor_tFormat)
                    
                    if split[filter_col] != "0":                
                        if sensor_colStart == 2:
                            sensor_strTime.append(str(split[0]) + "_" + str(split[1]))
                        elif sensor_colStart == 1:
                            sensor_strTime.append(str(split[0]))
                        
                        sensor_dat.append(split[sensor_colStart:len(split)])
                        sensor_decTime.append(time.mktime(time_temp.timetuple()) + time_temp.microsecond/1000000.0)
                        sensor_time.append(time_temp)
                    else:
                        filter_count = filter_count + 1
                elif sensor == "scan":
                    #time_temp = datetime.strptime(str(split[0]), sensor_tFormat)
                    
                    if (float(split[filter_col]) < 10) and (not math.isnan(float(split[filter_col]))):                
                        # calculating nitrate
                        no3_conc = float(split[no3_col]) * 4.43
                        
                        if sensor_colStart == 2:
                            sensor_strTime.append(str(split[0]) + "_" + str(split[1]))
                        elif sensor_colStart == 1:
                            sensor_strTime.append(str(split[0]))

                        # adding list to list
                        temp = []
                        temp.append(str(no3_conc))
                        temp.extend(split[sensor_colStart:len(split)])
                        sensor_dat.append(temp)

                        sensor_decTime.append(time.mktime(time_temp.timetuple()) + time_temp.microsecond/1000000.0)
                        sensor_time.append(time_temp)
                    else:
                        filter_count = filter_count + 1
                elif sensor == "scanfp":
                    #time_temp = datetime.strptime(str(split[0]), sensor_tFormat)
                    if sensor_colStart == 2:
                        sensor_strTime.append(str(split[0]) + "_" + str(split[1]))
                    elif sensor_colStart == 1:
                        sensor_strTime.append(str(split[0]))
                    sensor_dat.append(split[sensor_colStart:len(split)])
                    sensor_decTime.append(time.mktime(time_temp.timetuple()) + time_temp.microsecond/1000000.0)
                    sensor_time.append(time_temp)

                
                interval_time.append(time.mktime(time_temp.timetuple()) + time_temp.microsecond/1000000.0)

                # 20th to prevent weird gaps in beginning   
                if i == 20:
                    sensor_interval = (interval_time[len(interval_time)-1] - interval_time[len(interval_time)-2])/2.
                    LogOutput("sensor interval: " + str(sensor_interval))
                
        sensor_fHandler.close()

        
        LogOutput("original number of sensor measurements: " + str(i - 1))
        LogOutput("points filtered points for " + sensor + ": " + str(filter_count))

        # ----- finding minimum time difference & output -----

        out_fHandler = open(out_fullfn, "w")

        pos_timeArray = array(pos_decTime)

        speed_count = 0
        time_count = 0
        final_count = 0

        for i in range(len(sensor_decTime)):
            #if ((i + 1)%50) == 0:    
            #    print str(i + 1) + " out of " + str(len(sensor_decTime))

            time_diff = abs(sensor_decTime[i] - pos_timeArray)
            min_id = time_diff.argmin()

            if i == 0:
                # titles line... functionalize?
                titles = []
                sensor_timeTitle = sensor + "_time"
                pos_timeTitle = "gps_time"

                titles.append(sensor_timeTitle)
                titles.append(pos_timeTitle)
                titles.extend(pos_dat[0])
                titles.append("speed")

                # nitrate title, user added column
                if sensor == "scan":
                    titles.append("NO3")
                
                titles.extend(sensor_dat[0])

                titles_line = string.join(titles, ",")

                out_fHandler.write(titles_line)
                out_fHandler.flush()

                # units line
                units = []
                units.append(generic_tString)
                units.append(generic_tString)
                units.extend(pos_dat[1])
                units.append("m/s")

                if sensor == "scan":
                    units.append("mg/l")
                    
                units.extend(sensor_dat[1])

                units_line = string.join(units, ",")
                
                out_fHandler.write(units_line)
                out_fHandler.flush()

            # filters if closest time points are far apart
            if time_diff.min() < sensor_interval:

                # filters slow speeds... not sure how many that is yet
                if i > 0:
                    if speed[min_id - 1] >= speed_filter:
                        dat = []

                        sensor_newStrTime = time.strftime(generic_tString, sensor_time[i].timetuple())
                        pos_newStrTime = time.strftime(generic_tString, pos_time[min_id].timetuple())

                        dat.append(sensor_newStrTime)
                        dat.append(pos_newStrTime)


                        dat.extend(pos_dat[min_id+2])
                        # only line of difference, adds speed column
                        dat.append(str(speed[min_id - 1]))
                        
                        dat.extend(sensor_dat[i+2])

                        dat_line = string.join(dat, ",")

                        out_fHandler.write(dat_line)
                        out_fHandler.flush()
                        final_count = final_count + 1
                    else:
                        speed_count = speed_count + 1
                else:
                    # probably redundant section
                    dat = []

                    sensor_newStrTime = time.strftime(generic_tString, sensor_time[i].timetuple())
                    pos_newStrTime = time.strftime(generic_tString, pos_time[min_id].timetuple())

                    dat.append(sensor_newStrTime)
                    dat.append(pos_newStrTime)
                    dat.extend(pos_dat[min_id+2])

                    dat.append("NA")
                        
                    dat.extend(sensor_dat[i+2])

                    dat_line = string.join(dat, ",")

                    out_fHandler.write(dat_line)
                    out_fHandler.flush()
                    final_count + 1
            else:
                time_count = time_count + 1
                    
        out_fHandler.close()
        LogOutput("points filtered due to speed: " + str(speed_count))
        LogOutput("points filtered due to time mismatch: " + str(time_count))
        LogOutput("total remaining points: " + str(final_count))
    LogOutput(" ")
        

log_fHandler.close()

