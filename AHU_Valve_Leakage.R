# Megan K. McHugh, Masters in Engineering (Sustainable Systems) Thesis Research, UT Austin 2018
# Intelligent Environments Lab & UT Facilities Services Energy Management Optimization
# Fault Detection of Air Handling Unit Chilled Water and Steam Valve Leakage
# Part 1: AHU Valve Leakage Calculations

#################### DEFINE FUNCTION ####################
# campus_building is the abbreviation (e.g. "RLM") of a building on UT Austin main campus. Returns ALL data sets for unrecognized inputs.
# building_ahu is the name of an air handling unit (e.g. "AHU09") within a main campus building. Returns ALL data sets for unrecognized inputs.
# valve_type is either chilled water ("CHW" | "chw") or steam ("STM" | "stm") for each air handling unit. Returns BOTH chw & stm data sets for unrecognized inputs.
# Only accommodates proper entries for all three fields else complete data sets are returned instead.

AHU_Valve_Leakage <- function(campus_building = "ABC", building_ahu = "AHU00", valve_type = "both", scrape_Siemens = c("yes", "no"), scrape_Andover = c("yes", "no")){
  # defaults to returning all data sets
  
  #################### LOAD LIBRARIES ####################
  library(gmailr)
  library(data.table)
  library(lubridate)
  library(plyr)
  library(dplyr)
  library(zoo)
  library(openxlsx)
  library(tibble)
  
  #################### DOWNLOAD TREND REPORTS VIA GMAIL ####################
  #library(gmailr)
  
  if(grepl(pattern = "yes|y", scrape_Siemens, ignore.case = T)) {
    
    # create new directory to store trend data reports if one doesn't already exist
    folder_path <- "./raw_data/Siemens_raw/"
    ifelse(!dir.exists(folder_path), dir.create(folder_path), FALSE)
    
    # download secret file.json when setting up google API for gmail
    # otherwise use key and secret to authorize
    gmail_auth(scope = "full", secret_file = './inputs/client_secret_CENSORED.apps.googleusercontent.com.json')
    
    # get a list of messages with the label "AHU Valve Testing Trend Reports" from energymanagementoptimization@gmail.com
    # check https://developers.google.com/gmail/api/v1/reference/users/labels/list to determine the correct label ID (not the label name as listed in gmail)
    my_messages <- messages(label_ids = "Label_1", include_spam_trash = NULL, user_id = "me")
    
    # fix the double nested list to be a single vector with a list of message IDs
    msg_unnest <- my_messages[[1]]$messages
    
    # save all attachments from all email messages in list
    msg_id_list <- lapply(msg_unnest, function(x) save_attachments(message(x$id), path = folder_path, user_id = "me"))
    
    # list of imported file names
    files <- list.files(pattern = "*.csv", path = folder_path)
    
    #################### IMPORT TREND REPORTS ####################
    #library(data.table)
    
    # read dataframes
    read_data <- function(p){
      df <- fread(p, skip = "<>Date", nrows = length(readLines(p)), fill = T, stringsAsFactors = F)
      return(df)
    }
    
    # read Siemens trend point headers
    read_headers <- function(p, d){
      nd <- ncol(d)-2
      headers <- fread(p, skip = "Point_1", header = F, nrows = nd, fill = T, select = c(1,2,4), col.names = c('Point.ID','Trend.Name','Trend.Interval'), stringsAsFactors = F)
      return(headers)
    }
    
    p <- paste0(folder_path, files) # create paths
    df.list <- lapply(p, read_data) # list of dataframes
    vars.list <- mapply(FUN = read_headers, p = p, d = df.list, SIMPLIFY = F) # list of headers
    
    #################### CLEAN & ORGANIZE TREND DATA ####################
    #library(lubridate)
    #library(plyr)
    #library(dplyr)
    #library(zoo)
    
    renames <- lapply(vars.list, "[[", "Trend.Name") # list of trend names
    
    # function to make to rename trend cols, remove footer, and format date and time columns
    headers_to_varnames <- function(r, d){
      # rename data frame column headers to variable names by point ID
      colnames(d)[1:2] <- c('Date','Time') # rename date and time cols
      colnames(d)[3:ncol(d)] <- r # rename point ID cols with var names
      d <- d[1:nrow(d)-1,] # remove last row
      d$Date.Time <- paste(d$Date, d$Time)
      d$Date.Time <- mdy_hms(d$Date.Time, tz = "America/Chicago")
      d <- setcolorder(d, c(ncol(d),1:(ncol(d)-1)))
      d <- d[,-c(2,3)]
      return(d)
    }
    
    # formatted dataframes
    df.list1 <- suppressWarnings(mapply(FUN = headers_to_varnames, r = renames, d = df.list, SIMPLIFY = F))
    
    # subset time intervals to every 15 minutes
    df.list.sub <- lapply(df.list1, subset, (minute(ymd_hms(Date.Time)) == 00 | minute(ymd_hms(Date.Time)) == 15 
                                             | minute(ymd_hms(Date.Time)) == 30 | minute(ymd_hms(Date.Time)) == 45) 
                          & second(ymd_hms(Date.Time)) == 00)
    
    # name dataframes and determine building names
    df.list.sub <- setNames(df.list.sub, files) # set df list names
    split.names <- strsplit(names(df.list.sub), split = "_") # seperate parts of names by underscore
    bldg.names <- lapply(split.names, function(x) ifelse(x[1] == "BMC",
                                                         paste0(x[1],"_",x[2]),
                                                         x[1]))
    bldg.names <- unique(bldg.names)
    
    #combine dataframes from the same building
    df.list.bind <- list()
    for(j in 1:length(bldg.names)){
      bind.names <- ls(df.list.sub, pattern = bldg.names[j])
      df.list.bind[[j]] <- rbind.fill(df.list.sub[bind.names])
    }
    df.list.bind <- setNames(df.list.bind, bldg.names) #set df list names
    
    # remove duplicated date/time rows
    df.list.unique <- lapply(df.list.bind, function(x) distinct(x, x$Date.Time, .keep_all = TRUE))
    df.list.unique <- lapply(df.list.unique, function(x) subset(x, select = c(1:(ncol(x)-2)))) #remove the two added columns from distinct function
    
    # merge separate BMC data frames
    df.list.unique[['BMC']] <- merge(df.list.unique[['BMC_AHU']], df.list.unique[['BMC_FCU']])
    df.list.unique[['BMC_AHU']] <- NULL; df.list.unique[['BMC_FCU']] <- NULL # remove extra DFs
    df_list <- df.list.unique
    
    ## determine names of air handling units per building for each data frame
    
    # list to store full split names
    split.ahu.names <- lapply(df_list, 
                              function(x) ifelse(grepl(names(x[2:ncol(x)]), pattern =  "^DPI"),
                                                 strsplit(names(x[2:ncol(x)]), split = "[_]"), # DPI is only one separated by underscore
                                                 strsplit(names(x[2:ncol(x)]), split = "[.]"))) # seperate parts of names by period
    
    # list to store ahu names for each building
    ahu.names <- lapply(split.ahu.names, function(x) lapply(x, function(y) gsub("[-]", "", y[2])))
    
    # list to store variable names
    ahu.var.names <- lapply(split.ahu.names, function(x) lapply(x, function(y) ifelse(length(y) == 4,
                                                                                      paste0(y[3], y[4]),
                                                                                      ifelse(grepl(y[3], pattern = "[-]"),
                                                                                             gsub("[-]", "", y[3]),
                                                                                             y[3]))))
    
    # remove non-AHU names from the RLM dataset
    ahu.names[['RLM']][[1]] <- NULL ; ahu.names[['RLM']][[1]] <- NULL
    ahu.var.names[['RLM']][[1]] <- NULL ; ahu.var.names[['RLM']][[1]] <- NULL
    
    # global outside air temperature and relative humidity to use for each ahu analysis (stored in RLM dataframe by trend logs)
    global.OA <- data.frame(c(df_list[["RLM"]]['Date.Time'],
                              df_list[["RLM"]]['GLOBAL.OAT'],
                              df_list[["RLM"]]['GLOBAL.OAH']), 
                            stringsAsFactors = F)
    df_list[["RLM"]][c(2:3)] <- NULL
    
    global.OA$GLOBAL.OAT <- as.numeric(global.OA$GLOBAL.OAT)
    global.OA$GLOBAL.OAH <- as.numeric(global.OA$GLOBAL.OAH)
    global.OA[1,2] <- global.OA[2,2] ; global.OA[1,3] <- global.OA[2,3]
    
    # 4 hrs na.approx gap fill
    na_gap <- 60/15 * 4
    global.OA[2:3] <- lapply(global.OA[2:3], function(x) na.approx(x, na.rm = F, maxgap = na_gap))
    
    names(global.OA)[2:3] <- c("Global.OAT", "Global.OAH")
    
    saveRDS(global.OA, file = "./supplements/global_OA.rds")
    
    # initialize list of data frames for calculations with Date.Time column for each building
    dfl1 <- lapply(df_list, function(x) list(left_join(as.data.frame(x[1]), 
                                                       global.OA, 
                                                       by = c('Date.Time'))))
    dfl1 <- lapply(dfl1, setNames, "Global.OA")
    
    # rename data frame columns to ahu names
    for(i in 1:length(df_list)){
      names(ahu.var.names[[i]]) <- ahu.var.names[[i]]
      colnames(df_list[[i]])[2:ncol(df_list[[i]])] <- names(ahu.var.names[[i]])
      names(ahu.names[[i]]) <- ahu.names[[i]]
    }
    
    # split data frames by ahu
    unique.ahu <- lapply(ahu.names, function(x) unique(x))
    unique.ahu <- lapply(unique.ahu, function(x) setNames(x, paste0(x)))
    
    dfl.split <- lapply(df_list, function(x) x[-1])
    
    split_by_ahu <- function(d, a, u, v){
      df <- list()
      for (i in 1:length(u)) {
        df[[i]] <- d[names(a) == names(u)[i]]
        names(df[[i]]) <- v[names(a) == names(u)[i]]
      }
      names(df) <- names(u)
      return(df)
    }
    
    dfl2 <- mapply(split_by_ahu, d = dfl.split, a = ahu.names, u = unique.ahu, v = ahu.var.names, SIMPLIFY = F)
    
    # convert values from character to numeric
    na_convert <- function(d){
      # linear interpolation to fill NAs
      df <- na.fill(na.approx(as.numeric(d), na.rm = F, maxgap = na_gap), fill = "extend")
      return(df)
    }
    dfl3 <- suppressWarnings(lapply(dfl2, function(y) lapply(y, function(x) lapply(x, na_convert))))
    
    # dataframe with all parts
    merge_dfls <- function(d, l){
      df <- list()
      for (i in 1:length(d)) {
        df[[i]] <- cbind(l[[1]], d[[i]])
      }
      names(df) <- names(d)
      return(df)
    }
    
    Siemens.dfl <- mapply(merge_dfls, l = dfl1, d = dfl3, SIMPLIFY = F)
    
    saveRDS(Siemens.dfl, "./inputs/Siemens_dfl.rds")

  } else {
    
    # load previous Siemens dataframes
    Siemens.dfl <- readRDS("./inputs/Siemens_dfl.rds")

  }

  #################### CREATE PSYCHROMETRIC FUNCTIONS ####################
  #function to calculate barometric/total pressure based on altitude (default 0 m)
  #eq. 3 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  bar_press <- function(altitude = 150){
    return(101.325*(1-2.25577E-05*altitude)**5.2559) #kPa
  }
  
  #function to calculate saturated water vapor pressure
  #eq. 5 & eq. 6 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  sat_w_press <- function(dry.bulb.temp){
    #coefficients to find sat pressure over water and ice
    C <- c(-5674.5359, 6.3925247, -0.009677843, 6.2215701E-07, 2.07478E-09, -9.4840240E-13, 4.1635019, -5800.2206, 1.3914993, -4.8640239E-02, 4.1764768E-05, -1.4452093E-08, 6.5459673)
    #if less than 273.15K then calc for water, else calc for ice
    sat.w.press <- ifelse((dry.bulb.temp+273.15) < 273.15, exp(C[1]/(dry.bulb.temp+273.15)+C[2]+C[3]*(dry.bulb.temp+273.15)+C[4]*(dry.bulb.temp+273.15)**2+C[5]*(dry.bulb.temp+273.15)**3+C[6]*(dry.bulb.temp+273.15)**4+C[7]*log((dry.bulb.temp+273.15))),
                          exp(C[8]/(dry.bulb.temp+273.15)+C[9]+C[10]*(dry.bulb.temp+273.15)+C[11]*(dry.bulb.temp+273.15)**2+C[12]*(dry.bulb.temp+273.15)**3+C[13]*log((dry.bulb.temp+273.15))))
    return(sat.w.press/1000) #kPa
  }
  
  #function to calculate the partial water vapor pressure
  #eq. 36 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  par_w_press <- function(humidity.ratio, altitude = 150){
    return(bar_press(altitude)*humidity.ratio/(0.62198+humidity.ratio))
  }
  
  #function to calculate saturated humidity ratio
  #eq. 23 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  sat_hum_ratio <- function(dry.bulb.temp, altitude = 150){
    return(0.62198 * sat_w_press(dry.bulb.temp) / (bar_press(altitude) - sat_w_press(dry.bulb.temp))) #kg/kg
  }
  
  #function to calculate enthalpy given dry bulb temp and humidity ratio
  #eq. 32 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  enthalpy <- function(dry.bulb.temp, humidity.ratio){
    Enthalpy <- 1.006*dry.bulb.temp+humidity.ratio*(2501+1.805*dry.bulb.temp) #kJ/kg
    return(Enthalpy/2.326+7.686) #BTU/lb (conversion similar to C->F in methodology)
  }
  
  #function to calculate humidity ratio given the dry bulb temp, atm pressure, and relative humidity
  #eq. 25 & eq. 12 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  humidity_ratio <- function(relative.humidity, dry.bulb.temp, altitude = 150){
    relative.humidity <- relative.humidity/100
    W.s <- sat_hum_ratio(dry.bulb.temp, altitude)
    P <- bar_press(altitude)
    P.ws <- sat_w_press(dry.bulb.temp)
    W <- (-relative.humidity)*W.s*(P-P.ws)/(relative.humidity*P.ws-P)
    return(W) #lb/lb or kg/kg
  }
  
  #function to calculate the relative humidity
  #eq. 24 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  relative_humidity <- function(dry.bulb.temp, humidity.ratio, altitude = 150){
    relative.humidity <- par_w_press(humidity.ratio, altitude)/sat_w_press(dry.bulb.temp)*100
    relative.humidity <- ifelse(relative.humidity > 100, 100, relative.humidity)
    return(relative.humidity) #percent, %
  }
  
  #function to calculate the specific volume
  #eq. 24 - ASHRAE Fundamentals Handbook 2002, Psychrometrics
  #altitude, 150 meters set as default for Austin, TX
  specific_volume <- function(dry.bulb.temp, humidity.ratio, altitude = 150){
    specific.volume <- 0.2871*(dry.bulb.temp+273.15)*(1+1.6078*humidity.ratio)/bar_press(altitude) #m3/kg
    return(specific.volume*16.0185) #ft3/lb
  }
  
  #################### PREPARATION FOR CALCULATIONS ####################
  #ADP constants
  adp.temp <- 54.001 #deg F
  adp.rh <- 100 #percent, %
  adp.hum.ratio <- 0.00886067854779551
  
  #cost rate constants, $
  chw.fully.burdened <- 0.181
  chw.fuel.only <- 0.051204
  stm.fully.burdened <- 0.023
  stm.fuel.only <- 0.005197206
  
  #other constants
  annual.plant.efficiency <- 0.8470 #84.70%
  time.increment <- 15 #minutes
  const.vol.fan <- 100.00 #100%
  
  # remove CAFE MUAs from dfl
  #df_l.calcs[["EER"]][["CAFE"]] <- NULL #remove CAFE MUAs from dfl
  
  ##### Import Additional Dataframes #####
  
  # import master CFM data and index dataframe
  ut_cfm <- readRDS(file = "./Shiny-FDD/RDS/ut_cfm_and_occupancy.rds")
  
  # import Andover dataframes
  if(grepl(pattern = "yes|y", scrape_Andover, ignore.case = T)){
    source("./supplements/Andover_variable_renames.R")
    Andover_variable_renames()
    Andover.dfl <- readRDS("./inputs/Andover_dfl.rds")
  } else {
    Andover.dfl <- readRDS("./inputs/Andover_dfl.rds")
  }
  
  # combine with Siemens dfl
  dfl5 <- c(Siemens.dfl, Andover.dfl)
  saveRDS(dfl5, "./Shiny-FDD/RDS/BAS_formatted_data.rds")
  
  # import csv with 2016 historical average temperatures by date, typical meterological year data for Austin, TX
  Austin_TX_TMY_2016 <- read.csv("./inputs/Austin_TX_TMY_2016.csv")
  Austin_TX_TMY_2016$Date <- as.POSIXct(Austin_TX_TMY_2016$Date, format = "%Y-%m-%d") # convert date to POSIX object
  
  ##### Feature Conversion Functions #####
  
  # mixed air temp and %OAT
  mixed_air_temp <- function(x, l, OA) {
    if ("MAT" %in% names(x)) {
      x[["MAT"]]
    } else if ("CDMAT" %in% names(x)) {
      x[["CDMAT"]]
    } else if ("HDMAT" %in% names(x)) {
      x[["HDMAT"]]
    } else if ("RAT" %in% names(x)) {
      x[["Global.OAT"]] * OA + x[["RAT"]] * (1 - OA)
    } else {
      x[["Global.OAT"]] * OA + rep(75, length.out = l) * (1 - OA)
    }
  }
  
  # mixed air RH and %OAT
  mixed_air_RH <- function(x, l, OA) {
    if ("MAH" %in% names(x)) {
      x[["MAH"]]
    } else if ("RAH" %in% names(x)) {
      x[["Global.OAH"]] * OA + x[["RAH"]] * (1 - OA)
    } else {
      x[["Global.OAH"]] * OA + rep(50, length.out = l) * (1 - OA)
    }
  }
  
  # fan speeds by chw/stm or constant volume
  CD_fan_speed <- function(x, l) {
    if ("CDF" %in% names(x)) {
      x[["CDF"]]
    } else if ("SAF" %in% names(x)) {
      x[["SAF"]]
    } else if ("VFD" %in% names(x)) {
      x[["VFD"]]
    } else if ("CD_SVD" %in% names(x)) {
      x[["CD_SVD"]]
    } else if ("SVD" %in% names(x)) {
      x[["SVD"]]
    } else if ("SVFD" %in% names(x)) {
      x[["SVFD"]]
    } else if ("SVDFO" %in% names(x)) {
      x[["SVDFO"]]
    } else {
      return(rep(as.numeric(const.vol.fan), length.out = l))
    }
  }
  
  HD_fan_speed <- function(x, l) {
    if ("HDF" %in% names(x)) {
      x[["HDF"]]
    } else if ("SAF" %in% names(x)) {
      x[["SAF"]]
    } else if ("VFD" %in% names(x)) {
      x[["VFD"]]
    } else if ("HD_SVD" %in% names(x)) {
      x[["HD_SVD"]]
    } else if ("SVD" %in% names(x)) {
      x[["SVD"]]
    } else if ("SVFD" %in% names(x)) {
      x[["SVFD"]]
    } else if ("SVDFO" %in% names(x)) {
      x[["SVDFO"]]
    } else {
      return(rep(as.numeric(const.vol.fan), length.out = l))
    }
  }

  # supply air temps
  CD_supply_air <- function(x, l) {
    if ("CDT" %in% names(x)) {
      x[["CDT"]]
    } else if ("CDSAT" %in% names(x)) {
      x[["CDSAT"]]
    } else if ("CD" %in% names(x)) {
      x[["CD"]]
    } else if ("SAT" %in% names(x)) {
      x[["SAT"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  HD_supply_air <- function(x, l) {
    if ("HDT" %in% names(x)) {
      x[["HDT"]]
    } else if ("HDSAT" %in% names(x)) {
      x[["HDSAT"]]
    } else if ("HD" %in% names(x)) {
      x[["HD"]]
    } else if ("SAT" %in% names(x)) {
      x[["SAT"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  # set point temps
  CD_set_point <- function(x, l) {
    if ("CDS" %in% names(x)) {
      x[["CDS"]]
    } else if ("CDSAS" %in% names(x)) {
      x[["CDSAS"]]
    } else if ("SAS" %in% names(x)) {
      x[["SAS"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  HD_set_point <- function(x, l) {
    if ("HDS" %in% names(x)) {
      x[["HDS"]]
    } else if ("HDSAS" %in% names(x)) {
      x[["HDSAS"]]
    } else if ("SAS" %in% names(x)) {
      x[["SAS"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  # valve commands
  CD_valve_control <- function(x, l) {
    if ("CDV" %in% names(x)) {
      x[["CDV"]]
    } else if ("CDVFB" %in% names(x)) {
      x[["CDVFB"]]
    } else if ("CCV" %in% names(x)) {
      x[["CCV"]]
    } else if ("CHWO" %in% names(x)) {
      x[["CHWO"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  HD_valve_control <- function(x, l) {
    if ("HDV" %in% names(x)) {
      x[["HDV"]]
    } else if ("HDVFB" %in% names(x)) {
      x[["HDVFB"]]
    } else if ("HCV" %in% names(x)) {
      x[["HCV"]]
    } else {
      return(rep(as.numeric(NA), length.out = l))
    }
  }

  ##### Percent Outside Air #####
  
  # filter out rows where all are NA besides date/time and global OAT/OAH
  dflOA <- lapply(dfl5, function(y) lapply(y, function(x) {
    if(ncol(x) <= 4) {
      x <- x[complete.cases(x),]
    } else {
      x <- x[rowSums(is.na(x[,4:ncol(x)])) != (ncol(x)-3),]
    }
    return(x)
  }))
  
  # remove 0 row dataframes
  dfl6 <- lapply(dflOA, function(x) x[sapply(x, nrow)>0])
  
  # building names
  b.names <- mapply(function(x,y) rep_len(y, length.out = length(x)), x = dfl6, y = names(dfl6), SIMPLIFY = F)
  
  # AHU names
  a.names <- lapply(dfl6, names)
  
  # nrow (length) for each AHU dataframe
  dfl.lengths <- lapply(dfl6, function(x) lapply(x, function(y) nrow(y)))
  
  # OA per AHU
  pct_OA <- function(df, b) {
    OA <- list()
    for (i in 1:length(df)) {
      a <- names(df)[i]
      OA[[i]] <- ut_cfm$Percent.OA[(ut_cfm$Building %in% b | ut_cfm$Alt.Abbrev %in% b) & ut_cfm$AHU %in% a][!is.na(ut_cfm$Percent.OA[(ut_cfm$Building %in% b | ut_cfm$Alt.Abbrev %in% b) & ut_cfm$AHU %in% a])] / 100
    }
    return(OA)
  }
  
  pctOAs <- mapply(function(df, b) pct_OA(df = df, b = b), df = dfl6, b = b.names, SIMPLIFY = F)
  
  # add AHU names to list and make length equal to dfl6 nrow
  pctOAs <- mapply(function(dfl, pctOAs, df.len){
    names(pctOAs) <- names(dfl)
    for (i in 1:length(pctOAs)) {
      pctOAs[[i]] <- rep_len(pctOAs[[i]], length.out = df.len[[i]])
    }
    return(pctOAs)
  }, dfl = dfl6, pctOAs = pctOAs, df.len = dfl.lengths)
  
  #################### CALCULATIONS FOR AHU LEAKAGE ####################  
  
  ##### Feature reassignment #####
  dfl7 <- mapply(function(x,l,OA,a){
    df <- list()
    for (i in 1:length(x)) {
      df[[i]] <- as.data.frame(matrix(nrow = nrow(x[[i]]), ncol = 11, dimnames = list(NULL, 
                            c("Date.Time","Mixed.Air.Temp","Mixed.Air.RH","CD.Fan.Ctl","HD.Fan.Ctl","CD.SA.Temp","HD.SA.Temp","CD.SP.Temp","HD.SP.Temp","CHW.Vlv.Ctl","STM.Vlv.Ctl"))), stringsAsFactors = F)
      df[[i]]$Date.Time <- x[[i]]$Date.Time
      df[[i]]$Mixed.Air.Temp <- mixed_air_temp(x = x[[i]], l = l[[i]], OA = OA[[i]])
      df[[i]]$Mixed.Air.RH <- mixed_air_RH(x = x[[i]], l = l[[i]], OA = OA[[i]])
      df[[i]]$CD.Fan.Ctl <- CD_fan_speed(x = x[[i]], l = l[[i]])
      df[[i]]$HD.Fan.Ctl <- HD_fan_speed(x = x[[i]], l = l[[i]])
      df[[i]]$CD.SA.Temp <- CD_supply_air(x = x[[i]], l = l[[i]])
      df[[i]]$HD.SA.Temp <- HD_supply_air(x = x[[i]], l = l[[i]])
      df[[i]]$CD.SP.Temp <- CD_set_point(x = x[[i]], l = l[[i]])
      df[[i]]$HD.SP.Temp <- HD_set_point(x = x[[i]], l = l[[i]])
      df[[i]]$CHW.Vlv.Ctl <- CD_valve_control(x = x[[i]], l = l[[i]])
      df[[i]]$STM.Vlv.Ctl <- HD_valve_control(x = x[[i]], l = l[[i]])
    }
    names(df) <- a
    return(df)
  }, x = dfl6, l = dfl.lengths, OA = pctOAs, a = a.names, SIMPLIFY = F)
  
  ##### Flag incorrect sensor/controller values #####
  dflF <- mapply(function(x){
    for (i in 1:length(x)) {
      
      x[[i]]$CD.SA.Temp.Flag[x[[i]]$CD.SA.Temp > 130 | x[[i]]$CD.SA.Temp < 35] <- 1
      x[[i]]$CD.SA.Temp.Flag[!(x[[i]]$CD.SA.Temp > 130 | x[[i]]$CD.SA.Temp < 35)] <- 0

      x[[i]]$HD.SA.Temp.Flag[x[[i]]$HD.SA.Temp > 150 | x[[i]]$HD.SA.Temp < 55] <- 1
      x[[i]]$HD.SA.Temp.Flag[!(x[[i]]$HD.SA.Temp > 150 | x[[i]]$HD.SA.Temp < 55)] <- 0
      
    }
    return(x)
  }, x = dfl7, SIMPLIFY = F)
  
  ##### Convert all temperatures to celsius from fahrenheit #####
  dfl8 <- mapply(function(x,a){
    df <- list()
    for (i in 1:length(x)) {
      df[[i]] <- as.data.frame(matrix(nrow = nrow(x[[i]]), ncol = 6, dimnames = list(NULL, 
                     c("Date.Time", "Mixed.Air.TempC", "CD.SA.TempC", "HD.SA.TempC", "CD.SP.TempC", "HD.SP.TempC"))), stringsAsFactors = F)
      df[[i]]$Date.Time <- x[[i]]$Date.Time
      df[[i]]$Mixed.Air.TempC <- (as.numeric(x[[i]]$Mixed.Air.Temp)-32)*5/9
      
      x[[i]]$CD.SA.Temp[x[[i]]$CD.SA.Temp > 130 | x[[i]]$CD.SA.Temp < 35] <- NA
      df[[i]]$CD.SA.TempC <- (as.numeric(x[[i]]$CD.SA.Temp)-32)*5/9
      
      x[[i]]$HD.SA.Temp[x[[i]]$HD.SA.Temp > 150 | x[[i]]$HD.SA.Temp < 55] <- NA
      df[[i]]$HD.SA.TempC <- (as.numeric(x[[i]]$HD.SA.Temp)-32)*5/9
      
      df[[i]]$CD.SP.TempC <- (as.numeric(x[[i]]$CD.SP.Temp)-32)*5/9
      df[[i]]$HD.SP.TempC <- (as.numeric(x[[i]]$HD.SP.Temp)-32)*5/9
      df[[i]] <- merge(x[[i]], df[[i]], by = "Date.Time")
    }
    names(df) <- a
    return(df)
  }, x = dfl7, a = a.names, SIMPLIFY = F)
  
  #### Apply psychrometric functions #####
  dfl9 <- mapply(function(x,a,b) {
    df <- list()
    for (i in 1:length(x)) {
      df[[i]] <- as.data.frame(matrix(nrow = nrow(x[[i]]), ncol = 7, dimnames = list(NULL, 
                 c("Date.Time", "Mixed.Air.Hum.Ratio", "Mixed.Air.Enthalpy", "CD.CFM", "CD.CFM.act", "HD.CFM", "HD.CFM.act"))), stringsAsFactors = F)
      df[[i]]$Date.Time <- x[[i]]$Date.Time
      df[[i]]$Mixed.Air.Hum.Ratio <- humidity_ratio(dry.bulb.temp = x[[i]]$Mixed.Air.TempC, relative.humidity = x[[i]]$Mixed.Air.RH)
      df[[i]]$Mixed.Air.Enthalpy <- enthalpy(dry.bulb.temp = x[[i]]$Mixed.Air.TempC, humidity.ratio = df[[i]]$Mixed.Air.Hum.Ratio)
      
      df[[i]]$CD.CFM <- rep_len(as.numeric(ifelse(is.na(ut_cfm$CFM.CD[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]]), 
                                                  ut_cfm$CFM.TOT[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]], 
                                                  ut_cfm$CFM.CD[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]])), 
                                length.out = length(x[[i]]$CD.Fan.Ctl))
      df[[i]]$CD.CFM.act <- as.numeric(x[[i]]$CD.Fan.Ctl)/100 * df[[i]]$CD.CFM
      
      df[[i]]$HD.CFM <- rep_len(as.numeric(ifelse(is.na(ut_cfm$CFM.HD[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]]), 
                                                  ut_cfm$CFM.TOT[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]], 
                                                  ut_cfm$CFM.HD[(ut_cfm$Building %in% b[i] | ut_cfm$Alt.Abbrev %in% b[i]) & ut_cfm$AHU %in% a[i]])), 
                                length.out = length(x[[i]]$HD.Fan.Ctl))
      df[[i]]$HD.CFM.act <- as.numeric(x[[i]]$HD.Fan.Ctl)/100 * df[[i]]$HD.CFM
      df[[i]] <- merge(x[[i]], df[[i]], by = "Date.Time")
    }
    names(df) <- a
    return(df)
  }, x = dfl8, a = a.names, b = b.names, SIMPLIFY = F)
  
  ##### AHU valve leakage calculations #####
  dflC <- mapply(function(x,a) {
    df <- list()
    for (i in 1:length(x)) {
      df[[i]] <- as.data.frame(matrix(nrow = nrow(x[[i]]), ncol = 11, dimnames = list(NULL, 
                 c("Date.Time", "CHW.SA.RH", "CHW.SA.Hum.Ratio", "CHW.SA.Enthalpy", "CHW.SP.Enthalpy", "CHW.QcoilSP.BTU", "CHW.QcoilSP.ton_hr",
                   "CHW.Vlv.Leakage", "STM.QcoilSP.BTU", "STM.QcoilSP.lbs", "STM.Vlv.Leakage"))), stringsAsFactors = F)
      df[[i]]$Date.Time <- x[[i]]$Date.Time
      
      # chilled water
      df[[i]]$CHW.SA.RH <- ifelse(relative_humidity(dry.bulb.temp = x[[i]]$CD.SA.TempC, 
                                                    humidity.ratio = x[[i]]$Mixed.Air.Hum.Ratio) <= 100, 
                                  relative_humidity(dry.bulb.temp = x[[i]]$CD.SA.TempC, 
                                                    humidity.ratio = x[[i]]$Mixed.Air.Hum.Ratio), 
                                  100)
      df[[i]]$CHW.SA.Hum.Ratio <- humidity_ratio(relative.humidity = df[[i]]$CHW.SA.RH, 
                                                 dry.bulb.temp = x[[i]]$CD.SA.TempC)
      df[[i]]$CHW.SA.Enthalpy <- enthalpy(dry.bulb.temp = x[[i]]$CD.SA.TempC, 
                                              humidity.ratio = df[[i]]$CHW.SA.Hum.Ratio)
      df[[i]]$CHW.SP.Enthalpy <- enthalpy(dry.bulb.temp = x[[i]]$CD.SP.TempC, 
                                          humidity.ratio = df[[i]]$CHW.SA.Hum.Ratio)
      
      CHW.QcoilSP.BTU <- x[[i]]$CD.CFM.act * 
        (df[[i]]$CHW.SP.Enthalpy - df[[i]]$CHW.SA.Enthalpy) / 
        specific_volume(dry.bulb.temp = x[[i]]$Mixed.Air.TempC,
                        humidity.ratio = x[[i]]$Mixed.Air.Hum.Ratio) * time.increment
      df[[i]]$CHW.QcoilSP.BTU <- ifelse(CHW.QcoilSP.BTU >= 0, CHW.QcoilSP.BTU, 0)
      df[[i]]$CHW.QcoilSP.ton_hr <- df[[i]]$CHW.QcoilSP.BTU/12000
      df[[i]]$CHW.Vlv.Leakage <- ifelse(!is.na(x[[i]]$CHW.Vlv.Ctl) & x[[i]]$CHW.Vlv.Ctl == 0 & 
                                          !is.na(df[[i]]$CHW.QcoilSP.ton_hr) & df[[i]]$CHW.QcoilSP.ton_hr >= 0, 
                                        df[[i]]$CHW.QcoilSP.ton_hr, 
                                        0)
      
      # steam
      STM.QcoilSP.BTU <- 1.08 * as.numeric(x[[i]]$HD.CFM.act) * 
        (as.numeric(x[[i]]$HD.SA.Temp) - pmax(as.numeric(x[[i]]$HD.SP.Temp), as.numeric(x[[i]]$Mixed.Air.Temp), na.rm = T)) * 
        time.increment/60
      df[[i]]$STM.QcoilSP.BTU <- ifelse(STM.QcoilSP.BTU >= 0, STM.QcoilSP.BTU, 0)
      df[[i]]$STM.QcoilSP.lbs <- df[[i]]$STM.QcoilSP.BTU/1.218/1000
      df[[i]]$STM.Vlv.Leakage <- ifelse((as.numeric(x[[i]]$HD.SA.Temp) - as.numeric(x[[i]]$HD.SP.Temp)) >= 10,
                                        0,
                                        ifelse(!is.na(x[[i]]$STM.Vlv.Ctl) & 
                                                 x[[i]]$STM.Vlv.Ctl==0 & 
                                                 !is.na(df[[i]]$STM.QcoilSP.lbs) & 
                                                 df[[i]]$STM.QcoilSP.lbs >= 0, 
                                               df[[i]]$STM.QcoilSP.lbs, 0))
      
      df[[i]] <- merge(x[[i]], df[[i]], by = "Date.Time")
    }
    names(df) <- a
    return(df)
  }, x = dfl9, a = a.names, SIMPLIFY = F)
  
  saveRDS(dflC, "./Shiny-FDD/RDS/dflC.rds")
  
  ##### Leakage summary calculations #####
  dflS <- mapply(function(x,a) {

    df <- list()

    for (i in 1:length(x)) {
      
      x[[i]][,2:ncol(x[[i]])] <- lapply(x[[i]][,2:ncol(x[[i]])], as.numeric)
      
      df[[i]] <- as.data.frame(matrix(nrow = 1, ncol = 13, dimnames = list(NULL, 
                 c("Period.Days", "Total.CHW.Vlv.ton_hr", "CHW.ton.hr_day", "CHW.Vlv.Closed.Percent", "CHW.Vlv.Closed.Avg.Leakage", "CHW.Vlv.Closed.Max.TempF", "CHW.Vlv.Closed.days_yr",
                   "Total.STM.Vlv.lbs", "STM.lbs_day", "STM.Vlv.Closed.Percent", "STM.Vlv.Closed.Avg.Leakage", "STM.Vlv.Closed.Min.TempF", "STM.Vlv.Closed.days_yr"))), stringsAsFactors = F)
      
      df[[i]]$Period.Days <- length(unique(lubridate::date(x[[i]]$Date.Time)))
        # time_length((max(x[[i]]$Date.Time) - min(x[[i]]$Date.Time)), unit = "days")
      
      ## CHW calcs
      
      # total chw leakage
      df[[i]]$Total.CHW.Vlv.ton_hr <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Leakage) | x[[i]]$CHW.Vlv.Leakage == 0),
                                             sum(x[[i]]$CHW.Vlv.Leakage),
                                             as.numeric(NA))
      
      # chw leakage per day
      df[[i]]$CHW.ton.hr_day <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Leakage) | x[[i]]$CHW.Vlv.Leakage == 0),
                                       df[[i]]$Total.CHW.Vlv.ton_hr / df[[i]]$Period.Days, # total chw leakage
                                       as.numeric(NA))
      
      # count of times valve is closed over total valve trend counts
      df[[i]]$CHW.Vlv.Closed.Percent <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Ctl) | x[[i]]$CHW.Vlv.Ctl == 0) & min(x[[i]]$CHW.Vlv.Ctl, na.rm = T) <= 0,
                                               length(x[[i]]$CHW.Vlv.Ctl[x[[i]]$CHW.Vlv.Ctl == 0]) / length(x[[i]]$CHW.Vlv.Ctl) * 100,
                                               as.numeric(NA))
      
      # average leakage ton-hrs with valve closed
      df[[i]]$CHW.Vlv.Closed.Avg.Leakage <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Ctl) | x[[i]]$CHW.Vlv.Ctl == 0) & min(x[[i]]$CHW.Vlv.Ctl, na.rm = T) <= 0,
                                                   sum(x[[i]]$CHW.Vlv.Leakage[x[[i]]$CHW.Vlv.Leakage > 0]) / length(x[[i]]$CHW.Vlv.Leakage[x[[i]]$CHW.Vlv.Leakage > 0]),
                                                   as.numeric(NA))
      
      # max temperature when chw valve is closed
      df[[i]]$CHW.Vlv.Closed.Max.TempF <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Ctl) | x[[i]]$CHW.Vlv.Ctl == 0) & min(x[[i]]$CHW.Vlv.Ctl, na.rm = T) <= 0,
                                                 max(x[[i]]$Mixed.Air.Temp[x[[i]]$CHW.Vlv.Ctl == 0], na.rm = T),
                                                 as.numeric(NA))
      
      # days per year valve closed based on Austin, TX typical metereological year
      df[[i]]$CHW.Vlv.Closed.days_yr <- ifelse(!all(is.na(x[[i]]$CHW.Vlv.Ctl) | x[[i]]$CHW.Vlv.Ctl == 0) & min(x[[i]]$CHW.Vlv.Ctl, na.rm = T) <= 0,
                                               length(Austin_TX_TMY_2016$Date[Austin_TX_TMY_2016$Average.Daily.Drybulb.Temp <= df[[i]]$CHW.Vlv.Closed.Max.TempF]),
                                               as.numeric(NA))

      ## STM calcs
      # total stm leakage
      df[[i]]$Total.STM.Vlv.lbs <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Leakage) | x[[i]]$STM.Vlv.Leakage == 0),
                                          sum(x[[i]]$STM.Vlv.Leakage),
                                          as.numeric(NA))
      
      # stm leakage per day
      df[[i]]$STM.lbs_day <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Leakage) | x[[i]]$STM.Vlv.Leakage == 0),
                                    df[[i]]$Total.STM.Vlv.lbs / df[[i]]$Period.Days,
                                    as.numeric(NA))
        
      
      # count of times valve is closed over total valve trend counts
      df[[i]]$STM.Vlv.Closed.Percent <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Ctl) | x[[i]]$STM.Vlv.Ctl == 0) & min(x[[i]]$STM.Vlv.Ctl, na.rm = T) <= 0,
                                               length(x[[i]]$STM.Vlv.Ctl[x[[i]]$STM.Vlv.Ctl == 0]) / length(x[[i]]$STM.Vlv.Ctl) * 100,
                                               as.numeric(NA))
      
      
      # average leakage lbs with valve closed
      df[[i]]$STM.Vlv.Closed.Avg.Leakage <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Ctl) | x[[i]]$STM.Vlv.Ctl == 0) & min(x[[i]]$STM.Vlv.Ctl, na.rm = T) <= 0,
                                                   sum(x[[i]]$STM.Vlv.Leakage[x[[i]]$STM.Vlv.Leakage > 0], na.rm = T) / length(x[[i]]$STM.Vlv.Leakage[x[[i]]$STM.Vlv.Leakage > 0]),
                                                   as.numeric(NA))
      
      
      # min temperature when stm valve is closed
      
      df[[i]]$STM.Vlv.Closed.Min.TempF <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Ctl) | x[[i]]$STM.Vlv.Ctl == 0) & min(x[[i]]$STM.Vlv.Ctl, na.rm = T) <= 0,
                                                 min(x[[i]]$Mixed.Air.Temp[x[[i]]$STM.Vlv.Ctl == 0], na.rm = T),
                                                 as.numeric(NA))
      
      
      # days per year valve closed based on Austin, TX typical metereological year
      df[[i]]$STM.Vlv.Closed.days_yr <- ifelse(!all(is.na(x[[i]]$STM.Vlv.Ctl) | x[[i]]$STM.Vlv.Ctl == 0) & min(x[[i]]$STM.Vlv.Ctl, na.rm = T) <= 0,
                                               length(Austin_TX_TMY_2016$Date[Austin_TX_TMY_2016$Average.Daily.Drybulb.Temp >= df[[i]]$STM.Vlv.Closed.Min.TempF]),
                                               as.numeric(NA))
    }
    
    names(df) <- a
    return(df)
    
  }, x = dflC, a = a.names, SIMPLIFY = F)
  
  #################### FUNCTION OUTPUTS ####################
  
  # prepare dataframes for output summary
  dflOut <- mapply(function(x,a) {
    
    df <- list()
    
    for (i in 1:length(x)) {
      
      df[[i]] <- as.data.frame(matrix(nrow = 1, ncol = 10, dimnames = list(NULL, 
                 c("CHW.ton.hr_year","CHW.Leakage.Daily.Cost.Fully.Burdened", "CHW.Leakage.Daily.Cost.Fuel.Only", "CHW.Leakage.Annual.Cost.Fully.Burdened","CHW.Leakage.Annual.Cost.Fuel.Only",
                   "STM.lbs_year","STM.Leakage.Daily.Cost.Fully.Burdened", "STM.Leakage.Daily.Cost.Fuel.Only", "STM.Leakage.Annual.Cost.Fully.Burdened","STM.Leakage.Annual.Cost.Fuel.Only"))), stringsAsFactors = F)
      
      # CHW
      df[[i]]$CHW.ton.hr_year <- ifelse(is.numeric(x[[i]]$CHW.Vlv.Closed.days_yr) & is.numeric(x[[i]]$CHW.ton.hr_day),
                                        x[[i]]$CHW.ton.hr_day * x[[i]]$CHW.Vlv.Closed.days_yr,
                                        0)
      
      df[[i]]$CHW.Leakage.Daily.Cost.Fully.Burdened <- ifelse(is.numeric(x[[i]]$CHW.Vlv.Closed.days_yr) & is.numeric(x[[i]]$CHW.ton.hr_day),
                                                              x[[i]]$CHW.ton.hr_day * chw.fully.burdened,
                                                              0)
      
      df[[i]]$CHW.Leakage.Daily.Cost.Fuel.Only <- ifelse(is.numeric(x[[i]]$CHW.Vlv.Closed.days_yr) & is.numeric(x[[i]]$CHW.ton.hr_day),
                                                         x[[i]]$CHW.ton.hr_day * chw.fuel.only,
                                                         0)
      
      df[[i]]$CHW.Leakage.Annual.Cost.Fully.Burdened <- ifelse(is.numeric(x[[i]]$CHW.Vlv.Closed.days_yr) & is.numeric(x[[i]]$CHW.ton.hr_day),
                                                               df[[i]]$CHW.Leakage.Daily.Cost.Fully.Burdened * x[[i]]$CHW.Vlv.Closed.days_yr,
                                                               0)
      
      df[[i]]$CHW.Leakage.Annual.Cost.Fuel.Only <- ifelse(is.numeric(x[[i]]$CHW.Vlv.Closed.days_yr) & is.numeric(x[[i]]$CHW.ton.hr_day),
                                                          df[[i]]$CHW.Leakage.Daily.Cost.Fuel.Only * x[[i]]$CHW.Vlv.Closed.days_yr,
                                                          0)
      

      # STM
      df[[i]]$STM.lbs_year <- ifelse(is.numeric(x[[i]]$STM.Vlv.Closed.days_yr) & is.numeric(x[[i]]$STM.lbs_day),
                                        x[[i]]$STM.lbs_day * x[[i]]$STM.Vlv.Closed.days_yr,
                                        0)
      
      df[[i]]$STM.Leakage.Daily.Cost.Fully.Burdened <- ifelse(is.numeric(x[[i]]$STM.Vlv.Closed.days_yr) & is.numeric(x[[i]]$STM.lbs_day),
                                                              x[[i]]$STM.lbs_day * stm.fully.burdened,
                                                              0)
      
      df[[i]]$STM.Leakage.Daily.Cost.Fuel.Only <- ifelse(is.numeric(x[[i]]$STM.Vlv.Closed.days_yr) & is.numeric(x[[i]]$STM.lbs_day),
                                                         x[[i]]$STM.lbs_day * stm.fuel.only,
                                                         0)
      
      df[[i]]$STM.Leakage.Annual.Cost.Fully.Burdened <- ifelse(is.numeric(x[[i]]$STM.Vlv.Closed.days_yr) & is.numeric(x[[i]]$STM.lbs_day),
                                                               df[[i]]$STM.Leakage.Daily.Cost.Fully.Burdened * x[[i]]$STM.Vlv.Closed.days_yr,
                                                               0)
      
      df[[i]]$STM.Leakage.Annual.Cost.Fuel.Only <- ifelse(is.numeric(x[[i]]$STM.Vlv.Closed.days_yr) & is.numeric(x[[i]]$STM.lbs_day),
                                                          df[[i]]$STM.Leakage.Daily.Cost.Fuel.Only * x[[i]]$STM.Vlv.Closed.days_yr,
                                                          0)
      
      df[[i]] <- cbind(x[[i]], df[[i]])

    }
    
    names(df) <- a
    return(df)
    
  }, x = dflS, a = a.names, SIMPLIFY = F)
  
  ##### DF and OUT dataframes #####
  
  ## collapse into single dataframe per building
    # CHW
    full.chw.df <- mapply(function(x,a,b) {
      x <- rbindlist(x)
      x$Building <- b
      x$AHU <- a
      x <- x[,c(24:25,1:3,14,4:7,15:18)]
      return(x)
      }, x = dflOut, a = a.names, b = b.names, SIMPLIFY = F)
    
    # STM
    full.stm.df <- mapply(function(x,a,b) {
      x <- rbindlist(x)
      x$Building <- b
      x$AHU <- a
      x <- x[,c(24:25,1,8:9,19,10:13,20:23)]
      return(x)
    }, x = dflOut, a = a.names, b = b.names, SIMPLIFY = F)
  
  ## collapse into single dataframe total
    # CHW
    chw.df <- rbindlist(full.chw.df)
    chw.df <- chw.df[complete.cases(chw.df),]
    chw.df <- as.data.frame(chw.df)
    for (i in c(4:14)) {
      chw.df[,i] <- round(chw.df[,i], digits = 2)
    }

    # STM
    stm.df <- rbindlist(full.stm.df)
    stm.df <- stm.df[complete.cases(stm.df),]
    stm.df <- as.data.frame(stm.df)
    for (i in c(4:14)) {
      stm.df[,i] <- round(stm.df[,i], digits = 2)
    }

    
  ## output dataframes per building per AHU
  full.chw.out <- lapply(dflC, function(y) lapply(y, function(x) x <- x[,c(1,4,20,2,3,17,18,6,23:25,8,26:28,10,29)]))
  full.stm.out <- lapply(dflC, function(y) lapply(y, function(x) x <- x[,c(1,5,22,2,3,7,9,30,31,11,32)]))
  
  # round output numbers
  full.chw.out <- lapply(full.chw.out, function(y) lapply(y, function(x) {
    for (i in c(14,15,16,17,7,13,11)) {
      x[,i] <- round(as.numeric(x[,i]), digits = 2)
    }
    for (i in c(2,3,4,5,8,9,12)) {
      x[,i] <- round(as.numeric(x[,i]), digits = 1)
    }
    for (i in c(6,10)) {
      x[,i] <- round(as.numeric(x[,i]), digits = 7)
    }    
    return(x)
    }))
  
  full.stm.out <- lapply(full.stm.out, function(y) lapply(y, function(x) {
    for (i in c(8,9,11)) {
      x[,i] <- round(as.numeric(x[,i]), digits = 2)
    }
    for (i in c(2,3,4,5,6,7,10)) {
      x[,i] <- round(as.numeric(x[,i]), digits = 1)
    }
    return(x)
  }))
    
  ##### Excel Outputs #####
    
  # folder for intermediate leakage outputs
  ifelse(!dir.exists("./outputs/AHU_Valve_Leakage"), dir.create("./outputs/AHU_Valve_Leakage"), FALSE)
  
  # createStyle for openxlsx conditional formatting
  warnStyle <- createStyle(bgFill = "#6ddb97")
  leakStyle <- createStyle(bgFill = "#d9d9d9")
  databarStyle <- createStyle(halign = "left", valign = "center", numFmt = "COMMA")
  colorscaleStyle <- createStyle(halign = "center", valign = "center")
  
  ##### Excel functions for calculations dataframes #####
  
  ### all AHUs for one building
  
  chw_xlsx_bldg.no.ahu <- function(dfl, a, b) {
    
    sName <- paste0(b,".",a)
    addWorksheet(calcs_wb, sheetName = sName)
    chw.dt <- as.data.frame(dfl)
    writeDataTable(calcs_wb, sheet = sName, chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
    for (x in c(2,4,5,8,9,10,12,15)) {
      conditionalFormatting(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
      addStyle(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
    }
    for (x in c(3,6,7,11,13,14,16,17)) {
      suppressWarnings(conditionalFormat(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
    }
    conditionalFormatting(calcs_wb, sName, cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
    conditionalFormatting(calcs_wb, sName, cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
    addStyle(calcs_wb, sheet = sName, cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
    addStyle(calcs_wb, sheet = sName, cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
    setColWidths(calcs_wb, sName, cols = 1, widths = 17.57)
    
    wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_CHW_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
  }
  
  stm_xlsx_bldg.no.ahu <- function(dfl, a, b) {
    sName <- paste0(b,".",a)
    addWorksheet(calcs_wb, sheetName = sName)
    stm.dt <- as.data.frame(dfl)
    writeDataTable(calcs_wb, sheet = sName, stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
    for (x in c(2,4,5,6,7,9)) {
      conditionalFormatting(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
      addStyle(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
    }
    for (x in c(3,8,10,11)) {
      suppressWarnings(conditionalFormat(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
    }
    conditionalFormatting(calcs_wb, sName, cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
    conditionalFormatting(calcs_wb, sName, cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
    addStyle(calcs_wb, sheet = sName, cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
    addStyle(calcs_wb, sheet = sName, cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
    setColWidths(calcs_wb, sName, cols = 1, widths = 17.57)
    wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_STM_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
  }
  
  both_xlsx_bldg.no.ahu <- function(dfl.stm,dfl.chw,a,b) {
    
    sName.chw <- paste0(b,".",a,".CHW")
    addWorksheet(calcs_wb, sheetName = sName.chw)
    chw.dt <- as.data.frame(dfl.chw)
    writeDataTable(calcs_wb, sheet = sName.chw, chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
    for (x in c(2,4,5,8,9,10,12,15)) {
      conditionalFormatting(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
      addStyle(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
    }
    for (x in c(3,6,7,11,13,14,16,17)) {
      suppressWarnings(conditionalFormat(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
    }
    conditionalFormatting(calcs_wb, sName.chw, cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
    conditionalFormatting(calcs_wb, sName.chw, cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
    addStyle(calcs_wb, sheet = sName.chw, cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
    addStyle(calcs_wb, sheet = sName.chw, cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
    setColWidths(calcs_wb, sName.chw, cols = 1, widths = 17.57)
    
    sName.stm <- paste0(b,".",a,".STM")
    addWorksheet(calcs_wb, sheetName = sName.stm)
    stm.dt <- as.data.frame(dfl.stm)
    writeDataTable(calcs_wb, sheet = sName.stm, stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
    for (x in c(2,4,5,6,7,9)) {
      conditionalFormatting(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
      addStyle(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
    }
    for (x in c(3,8,10,11)) {
      suppressWarnings(conditionalFormat(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
    }
    conditionalFormatting(calcs_wb, sName.stm, cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
    conditionalFormatting(calcs_wb, sName.stm, cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
    addStyle(calcs_wb, sheet = sName.stm, cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
    addStyle(calcs_wb, sheet = sName.stm, cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
    setColWidths(calcs_wb, sName.stm, cols = 1, widths = 17.57)
    
    wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
    
  }
  
  ### all buildings
  
  chw_xlsx_all.bldgs <- function(dfl, a, b) {
    
    calcs_wb <- createWorkbook(creator = "Megan McHugh")
    dirCheck <- paste0("./outputs/AHU_Valve_Leakage/Building_Leakage_Calcs/",b[[1]])
    ifelse(!dir.exists(dirCheck), dir.create(dirCheck), FALSE)

    for (i in 1:length(dfl)) {
      
      sName <- paste0(b[[i]],".",a[[i]])
      addWorksheet(calcs_wb, sheetName = sName)
      
      chw.dt <- as.data.frame(dfl[[i]])
      writeDataTable(calcs_wb, sheet = sName, chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
      
      for (x in c(2,4,5,8,9,10,12,15)) {
        conditionalFormatting(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
        addStyle(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
      }
      for (x in c(3,6,7,11,13,14,16,17)) {
        suppressWarnings(conditionalFormat(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
      }
      conditionalFormatting(calcs_wb, sName, cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
      conditionalFormatting(calcs_wb, sName, cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
      addStyle(calcs_wb, sheet = sName, cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
      addStyle(calcs_wb, sheet = sName, cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
      
      setColWidths(calcs_wb, sName, cols = 1, widths = 17.57)
    }
    
    wb.filename <- paste0(dirCheck,"/",b[[1]],"_CHW_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
    
  }
  
  stm_xlsx_all.bldgs <- function(dfl, a, b) {
    
    calcs_wb <- createWorkbook(creator = "Megan McHugh")
    dirCheck <- paste0("./outputs/AHU_Valve_Leakage/Building_Leakage_Calcs/",b[[1]])
    ifelse(!dir.exists(dirCheck), dir.create(dirCheck), FALSE)
    
    for (i in 1:length(dfl)) {
      
      sName <- paste0(b[[i]],".",a[[i]])
      addWorksheet(calcs_wb, sheetName = sName)
      
      stm.dt <- as.data.frame(dfl[[i]])
      writeDataTable(calcs_wb, sheet = sName, stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
      
      for (x in c(2,4,5,6,7,9)) {
        conditionalFormatting(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
        addStyle(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
      }
      for (x in c(3,8,10,11)) {
        suppressWarnings(conditionalFormat(calcs_wb, sheet = sName, cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
      }
      conditionalFormatting(calcs_wb, sName, cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
      conditionalFormatting(calcs_wb, sName, cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
      addStyle(calcs_wb, sheet = sName, cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
      addStyle(calcs_wb, sheet = sName, cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
      
      setColWidths(calcs_wb, sName, cols = 1, widths = 17.57)
    }
    
    wb.filename <- paste0(dirCheck,"/",b[[1]],"_STM_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
    
  }
  
  both_xlsx_all.bldgs <- function(dfl.stm,dfl.chw,a,b) {
    
    calcs_wb <- createWorkbook(creator = "Megan McHugh")
    dirCheck <- paste0("./outputs/AHU_Valve_Leakage/Building_Leakage_Calcs/",b[[1]])
    ifelse(!dir.exists(dirCheck), dir.create(dirCheck), FALSE)
    
    for (i in 1:length(dfl.chw)) {
      
      sName.chw <- paste0(b[[i]],".",a[[i]],".CHW")
      addWorksheet(calcs_wb, sheetName = sName.chw)
      
      chw.dt <- as.data.frame(dfl.chw[[i]])
      writeDataTable(calcs_wb, sheet = sName.chw, chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
      
      for (x in c(2,4,5,8,9,10,12,15)) {
        conditionalFormatting(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
        addStyle(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
      }
      for (x in c(3,6,7,11,13,14,16,17)) {
        suppressWarnings(conditionalFormat(calcs_wb, sheet = sName.chw, cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
      }
      conditionalFormatting(calcs_wb, sName.chw, cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
      conditionalFormatting(calcs_wb, sName.chw, cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
      addStyle(calcs_wb, sheet = sName.chw, cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
      addStyle(calcs_wb, sheet = sName.chw, cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
      
      setColWidths(calcs_wb, sName.chw, cols = 1, widths = 17.57)
    }
    
    for (i in 1:length(dfl.stm)) {
      
      sName.stm <- paste0(b[[i]],".",a[[i]],".STM")
      addWorksheet(calcs_wb, sheetName = sName.stm)
      
      stm.dt <- as.data.frame(dfl.stm[[i]])
      writeDataTable(calcs_wb, sheet = sName.stm, stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
      
      for (x in c(2,4,5,6,7,9)) {
        conditionalFormatting(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
        addStyle(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
      }
      for (x in c(3,8,10,11)) {
        suppressWarnings(conditionalFormat(calcs_wb, sheet = sName.stm, cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
      }
      conditionalFormatting(calcs_wb, sName.stm, cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
      conditionalFormatting(calcs_wb, sName.stm, cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
      addStyle(calcs_wb, sheet = sName.stm, cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
      addStyle(calcs_wb, sheet = sName.stm, cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
      
      setColWidths(calcs_wb, sName.stm, cols = 1, widths = 17.57)
    }
    
    wb.filename <- paste0(dirCheck,"/",b[[1]],"_AHU_Valve_Leakage_Calculations.xlsx")
    saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
    
  }
  
  bldg.dirCheck <- paste0("./outputs/AHU_Valve_Leakage/Building_Leakage_Calcs/")
  ifelse(!dir.exists(bldg.dirCheck), dir.create(bldg.dirCheck), FALSE)
  
  ##### Apply Excel Functions #####
  
  if (campus_building %in% names(b.names)) {
    if (building_ahu %in% a.names[[campus_building]]) { # return specific worksheet for that building and AHU
      if (grepl(valve_type, pattern = "chw", ignore.case = T) & building_ahu %in% names(full.chw.out[[campus_building]])) {
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        addWorksheet(calcs_wb, sheetName = "CHW")
        chw.dt <- as.data.frame(full.chw.out[[campus_building]][[building_ahu]])
        writeDataTable(calcs_wb, sheet = "CHW", chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
        for (x in c(2,4,5,8,9,10,12,15)) {
          conditionalFormatting(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
          addStyle(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
        }
        for (x in c(3,6,7,11,13,14,16,17)) {
          suppressWarnings(conditionalFormat(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
        }
        conditionalFormatting(calcs_wb, "CHW", cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
        conditionalFormatting(calcs_wb, "CHW", cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
        addStyle(calcs_wb, sheet = "CHW", cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
        addStyle(calcs_wb, sheet = "CHW", cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
        setColWidths(calcs_wb, "CHW", cols = 1, widths = 17.57)
        wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_",building_ahu,"_CHW_Valve_Leakage_Calculations.xlsx")
        saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
        
      } else if (grepl(valve_type, pattern = "stm", ignore.case = T) & building_ahu %in% names(full.stm.out[[campus_building]])) {
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        addWorksheet(calcs_wb, sheetName = "STM")
        stm.dt <- as.data.frame(full.stm.out[[campus_building]][[building_ahu]])
        writeDataTable(calcs_wb, sheet = "STM", stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
        for (x in c(2,4,5,6,7,9)) {
          conditionalFormatting(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
          addStyle(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
        }
        for (x in c(3,8,10,11)) {
          suppressWarnings(conditionalFormat(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
        }
        conditionalFormatting(calcs_wb, "STM", cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
        conditionalFormatting(calcs_wb, "STM", cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
        addStyle(calcs_wb, sheet = "STM", cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
        addStyle(calcs_wb, sheet = "STM", cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
        setColWidths(calcs_wb, "STM", cols = 1, widths = 17.57)
        wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_",building_ahu,"_STM_Valve_Leakage_Calculations.xlsx")
        saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
        
      } else if (building_ahu %in% names(full.chw.out[[campus_building]]) & building_ahu %in% names(full.stm.out[[campus_building]])) { # return CHW and STM dfs for that building and AHU
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        
        addWorksheet(calcs_wb, sheetName = "CHW")
        chw.dt <- as.data.frame(full.chw.out[[campus_building]][[building_ahu]])
        writeDataTable(calcs_wb, sheet = "CHW", chw.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", bandedRows = F)
        for (x in c(2,4,5,8,9,10,12,15)) {
          conditionalFormatting(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
          addStyle(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, style = colorscaleStyle)
        }
        for (x in c(3,6,7,11,13,14,16,17)) {
          suppressWarnings(conditionalFormat(calcs_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.dt)+1, type = "databar"))
        }
        conditionalFormatting(calcs_wb, "CHW", cols = 16, rows = 1:nrow(chw.dt)+1, rule = "==0", style = warnStyle)
        conditionalFormatting(calcs_wb, "CHW", cols = 17, rows = 1:nrow(chw.dt)+1, rule = "!=0", style = leakStyle)
        addStyle(calcs_wb, sheet = "CHW", cols = 16, rows = 1:nrow(chw.dt)+1, style = databarStyle)
        addStyle(calcs_wb, sheet = "CHW", cols = 17, rows = 1:nrow(chw.dt)+1, style = databarStyle)
        setColWidths(calcs_wb, "CHW", cols = 1, widths = 17.57)
        
        addWorksheet(calcs_wb, sheetName = "STM")
        stm.dt <- as.data.frame(full.stm.out[[campus_building]][[building_ahu]])
        writeDataTable(calcs_wb, sheet = "STM", stm.dt, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", bandedRows = F)
        for (x in c(2,4,5,6,7,9)) {
          conditionalFormatting(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, type = 'colorscale', style = c('#638EC6','white','#ff5050'))
          addStyle(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, style = colorscaleStyle)
        }
        for (x in c(3,8,10,11)) {
          suppressWarnings(conditionalFormat(calcs_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.dt)+1, type = "databar", rule = "#ff5050"))
        }
        conditionalFormatting(calcs_wb, "STM", cols = 10, rows = 1:nrow(stm.dt)+1, rule = "==0", style = warnStyle)
        conditionalFormatting(calcs_wb, "STM", cols = 11, rows = 1:nrow(stm.dt)+1, rule = "!=0", style = leakStyle)
        addStyle(calcs_wb, sheet = "STM", cols = 10, rows = 1:nrow(stm.dt)+1, style = databarStyle)
        addStyle(calcs_wb, sheet = "STM", cols = 11, rows = 1:nrow(stm.dt)+1, style = databarStyle)
        setColWidths(calcs_wb, "STM", cols = 1, widths = 17.57)
        
        wb.filename <- paste0("./outputs/AHU_Valve_Leakage/",campus_building,"_",building_ahu,"_Valve_Leakage_Calculations.xlsx")
        saveWorkbook(calcs_wb, file = wb.filename, overwrite = TRUE)
        
      }
    } else { # return all AHU dataframes for that building
      if (grepl(valve_type, pattern = "chw", ignore.case = T)) { # return CHW dfs for all AHUs
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        mapply(function(dfl,a,b) chw_xlsx_bldg.no.ahu(dfl,a,b), dfl = full.chw.out[[campus_building]], a = a.names[[campus_building]], b = b.names[[campus_building]], SIMPLIFY = F)

      } else if (grepl(valve_type, pattern = "stm", ignore.case = T)) { # return STM dfs for all AHUs
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        mapply(function(dfl,a,b) stm_xlsx_bldg.no.ahu(dfl,a,b), dfl = full.stm.out[[campus_building]], a = a.names[[campus_building]], b = b.names[[campus_building]], SIMPLIFY = F)
        
      } else { # return workbooks for all AHUs
        
        calcs_wb <- createWorkbook(creator = "Megan McHugh")
        mapply(function(dfl.stm,dfl.chw,a,b) both_xlsx_bldg.no.ahu(dfl.stm,dfl.chw,a,b), dfl.chw = full.chw.out[[campus_building]], dfl.stm = full.stm.out[[campus_building]], a = a.names[[campus_building]], b = b.names[[campus_building]], SIMPLIFY = F)
        
      }
    }
  } else { # return workbooks for all buildings and all AHUs
    if (grepl(valve_type, pattern = "chw", ignore.case = T)) { # return CHW dfs for all buildings and all AHUs
      if (grepl(scrape_Andover, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T)) { # only replace Andover if new reports have been scraped
        
        Andover.chw.out <- full.chw.out[names(full.chw.out) %in% names(Andover.dfl)]
        mapply(function(dfl,a,b) chw_xlsx_all.bldgs(dfl,a,b), dfl = Andover.chw.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) { # only replace Siemens if new reports have been scraped
        
        Siemens.chw.out <- full.chw.out[names(full.chw.out) %in% names(Siemens.dfl)]
        mapply(function(dfl,a,b) chw_xlsx_all.bldgs(dfl,a,b), dfl = Siemens.chw.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) {
        
        mapply(function(dfl,a,b) chw_xlsx_all.bldgs(dfl,a,b), dfl = full.chw.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      }
    } else if (grepl(valve_type, pattern = "stm", ignore.case = T)) { # return STM dfs for all buildings and all AHUs
      if (grepl(scrape_Andover, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T)) { # only replace Andover if new reports have been scraped
        
        Andover.stm.out <- full.stm.out[names(full.stm.out) %in% names(Andover.dfl)]
        mapply(function(dfl,a,b) stm_xlsx_all.bldgs(dfl,a,b), dfl = Andover.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) { # only replace Siemens if new reports have been scraped
        
        Siemens.stm.out <- full.stm.out[names(full.stm.out) %in% names(Siemens.dfl)]
        mapply(function(dfl,a,b) stm_xlsx_all.bldgs(dfl,a,b), dfl = Siemens.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) {
        
        mapply(function(dfl,a,b) stm_xlsx_all.bldgs(dfl,a,b), dfl = full.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      }
    } else { # return workbooks for all buildings and all AHUs
      if (grepl(scrape_Andover, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T)) { # only replace Andover if new reports have been scraped
        
        Andover.chw.out <- full.chw.out[names(full.chw.out) %in% names(Andover.dfl)]
        Andover.stm.out <- full.stm.out[names(full.stm.out) %in% names(Andover.dfl)]
        mapply(function(dfl.stm,dfl.chw,a,b) both_xlsx_all.bldgs(dfl.stm,dfl.chw,a,b), dfl.chw = Andover.chw.out, dfl.stm = Andover.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & !grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) { # only replace Siemens if new reports have been scraped
        
        Siemens.chw.out <- full.chw.out[names(full.chw.out) %in% names(Siemens.dfl)]
        Siemens.stm.out <- full.stm.out[names(full.stm.out) %in% names(Siemens.dfl)]
        mapply(function(dfl.stm,dfl.chw,a,b) both_xlsx_all.bldgs(dfl.stm,dfl.chw,a,b), dfl.chw = Siemens.chw.out, dfl.stm = Siemens.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
        
      } else if (grepl(scrape_Siemens, pattern = "yes|y", ignore.case = T) & grepl(scrape_Andover, pattern = "yes|y", ignore.case = T)) {
       
        mapply(function(dfl.stm,dfl.chw,a,b) both_xlsx_all.bldgs(dfl.stm,dfl.chw,a,b), dfl.chw = full.chw.out, dfl.stm = full.stm.out, a = a.names, b = b.names, SIMPLIFY = F)
         
      } else {
        
        print("Check './outputs/AHU_Valve_Leakage/Building_Leakage_Calcs/' for most recently updated output calculation files.")
        
      }
    }
  }

  ##### Summary Output #####
  
  # create excel summary workbook
  summary_wb <- createWorkbook(creator = "Megan McHugh")

  # summary workbook add worksheets
  addWorksheet(summary_wb, sheetName = "CHW")
  addWorksheet(summary_wb, sheetName = "STM")
  
  # summary workbook write data
  writeDataTable(summary_wb, "CHW", chw.df, rowNames = F, colNames = T, tableStyle = "TableStyleMedium16", firstColumn = F)
  writeDataTable(summary_wb, "STM", stm.df, rowNames = F, colNames = T, tableStyle = "TableStyleMedium17", firstColumn = F)
  
  # summary workbook add databars
  for (x in c(4:ncol(chw.df))) {
    suppressWarnings(conditionalFormat(summary_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.df)+1, type = "databar"))
    addStyle(summary_wb, sheet = "CHW", cols = x, rows = 1:nrow(chw.df)+1, style = databarStyle)
  }
  for (x in c(4:ncol(stm.df))) {
    suppressWarnings(conditionalFormat(summary_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.df)+1, type = "databar", rule = "#ff5050"))
    addStyle(summary_wb, sheet = "STM", cols = x, rows = 1:nrow(stm.df)+1, style = databarStyle)
  }
  
  # left align col 3
  addStyle(summary_wb, sheet = "CHW", cols = 3, rows = 1:nrow(chw.df)+1, style = databarStyle)
  addStyle(summary_wb, sheet = "STM", cols = 3, rows = 1:nrow(stm.df)+1, style = databarStyle)
  
  # summary workbook col widths
  setColWidths(summary_wb, "CHW", cols = 3:ncol(chw.df), widths = 15)
  setColWidths(summary_wb, "STM", cols = 3:ncol(stm.df), widths = 15)
  
  # summary workbook save
  summary.wb.name <- paste0("./outputs/AHU_Valve_Leakage/Combined_AHU_Valve_Leakage_Summary_", Sys.Date(), ".xlsx")
  saveWorkbook(summary_wb, file = summary.wb.name, overwrite = TRUE)

  # write csv backups
  chw.summary.name <- paste0("./outputs/AHU_Valve_Leakage/CHW_Valve_Leakage_Summary_", Sys.Date(), ".csv")
  stm.summary.name <- paste0("./outputs/AHU_Valve_Leakage/STM_Valve_Leakage_Summary_", Sys.Date(), ".csv")
  write.csv(chw.df, file = chw.summary.name, row.names = F)
  write.csv(stm.df, file = stm.summary.name, row.names = F)
  
  # save R objects
  saveRDS(chw.df, file = "./Shiny-FDD/RDS/chw_leak_sum.rds")
  saveRDS(full.chw.out, file = "./Shiny-FDD/RDS/chw_detailed.rds")
  saveRDS(stm.df, file = "./Shiny-FDD/RDS/stm_leak_sum.rds")
  saveRDS(full.stm.out, file = "./Shiny-FDD/RDS/stm_detailed.rds")
  saveRDS(dflS, file = "./supplements/ahu_vlv_leak_sum.rds")
  
} #################### END OF FUNCTION ####################
