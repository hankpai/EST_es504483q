README 

Authors: henry pai, Sandra R. Villamizar, Thomas C. Harmon

Overview:
For data assimilation and Groundwater-Surface water (GW-SW) modeling along Merced River DOI: 10.1021/es504483q

More details:
- Code & data repository source: https://eng.ucmerced.edu/snsjho/files/San_Joaquin/Papers/SynopticGW_SW?sort_on=id&sort_dir=asc

- Code for most text outputs are presented.  Code for figures are available on request (hpai[at]ucmerced[dot]edu).  Disclaimer, there is some redundancy within the code, and they should be run as independent scripts.  User variables at the beginning of the scripts will likely need to be adjusted as well.

- Data sets
  1. Sensor collected longitudinal river data
  2. Downloaded SW flow and stage data from gauging stations near Cressy and Stevinson (CRS & MST)
  3. Downloaded county GW specific conductivity (SC) data from CA GAMA (California GW ambient monitoring and assessment)
  4. Traced river with Google Earth (kmz/kml) -> export to text file w/ spatial coordinates
  5. Mapped locations of large inlets and battery transitions

- Assimilation & preparation summary
  1. Code assigning/assimilating spatial (UTM) coordinates to water quality data by closest timestamp
     - gps_sensor_combine<n>.py
  2. Code converting river spatial data (longlat -> UTM) and projecting to linearized river dimension
     - latlong_to_utm.R
     - river_mi-for_flowpath_pts<n>.R (or close variation)     
  3. Code projecting assimilated synoptic sensor data to linearized river dimension
     - river_mile<n>.R
  4. Code projecting spatial coordinates to linear river distance (for traced and mapped data)
     - river_mi-for_flowpath_pts<n>.R (or close variation)
  5. Code filtering major stop and last inlet locations (600 m buffer up-/downstream from locations)
     - stop_structure_depth_filters<n>.R
  6. Code adjusting river resolution for UTM location and linearized dimension data
     - Dense resolution (0.5 m): merced_riv-river_meter_calc<n>.R
     - Lower resolution (100 m): merced_riv-sparse_spacing_localization<n>.R
  7. GIS interpolating GW SC surface and output
     - Sandra (co-author)
  8. Code assigning GW SC (from specific well or interpolated surface) to linearized river locations
     - assign_well_to_riv-sparse_spacing<n>.R

- Analysis & model summary
  1. Estimating travel times for station differntial gauging
     - Sandra (co-author)
  2. Code calculating coefficient of variation for wells with >= 5 pts to show relative changes in GW SC data
     - gw_sc-coef_var01.R
  3. Code adjusting downstream 
     - station_dat_get-around_synoptic_times<n>.R
  4. Code estimating slopes at different discretizations for linearized, longitudinal river locations
     - synoptic.slopes<n>.R
  5. Code discretizing GW-SW mixing model to estimate GW inflow, loading
     - gw_flux_calcs-equidistant<n>.R
  6. Spreadsheet analyzing model results
     - within Excel aggregating differential gauging data and summed model outputs
  





 
  