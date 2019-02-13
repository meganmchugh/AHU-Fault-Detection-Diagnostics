# Megan K. McHugh, Masters in Engineering (Sustainable Systems) Thesis Research, UT Austin 2018
# Intelligent Environments Lab & UT Facilities Services Energy Management Optimization
# Fault Detection of Air Handling Unit Chilled Water and Steam Valve Leakage
# Part 3: APAR Fault Detection

APAR_Fault_Detection <- function(scrape_Andover = c("yes", "no"), scrape_Siemens = c("yes", "no"), heatmap_display = "all", app_request = FALSE){

  #################### LIBRARIES ####################
  library(BBmisc)
  library(lubridate)
  library(stringr)
  library(mefa)
  library(data.table)
  library(lattice)
  
  #################### IMPORT BAS DATAFRAMES ####################
  if(grepl(pattern = "yes|y", scrape_Andover, ignore.case = T)){
    source("./supplements/Andover_variable_renames.R")
    Andover_variable_renames()
    Andover.dfl <- readRDS("./inputs/Andover_dfl.rds")
  } else {
    Andover.dfl <- readRDS("./inputs/Andover_dfl.rds")
  }
  
  if(grepl(pattern = "yes|y", scrape_Siemens, ignore.case = T)){
    source("./AHU_Valve_Leakage.R")
    AHU_Valve_Leakage(scrape_Siemens = "yes", scrape_Andover = "no")
    Siemens.dfl <- readRDS("./inputs/Siemens_dfl.rds")
  } else {
    Siemens.dfl <- readRDS("./inputs/Siemens_dfl.rds")
  }
  
  #################### SET AHU PARAMETERS ####################
  Ep_hc <- 0.005 # heating coil signal threshold
  Ep_cc <- 0.05 # cooling coil signal threshold
  Ep_d <- 0.155 # mixing box damper control signal threshold
  Ep_f <- 0.3 # air fraction threshold
  Ep_h <- 2.33 # humidify signal threshold
  Q_oa_min <- 0.15 # minimum outdoor air fraction
  Ep_t <- 1.0 # temperature threshold
  Del.T_min <- 5.56 # minimum change in temperature
  Del.T_sf <- 1.11 # change in temperature across the supply fan
  Del.T_rf <- 1.0 # change in temperature across the return fan
  T_co <- 96.8 # outdoor air changeover temperature for going from modes 3 to 4
  U_cc_min <- 0.0 # minimum cooling coil signal
  U_cc_max <- 1.0 # maximum cooling coil signal
  U_hc_min <- 0.0 # minimum heating coil signal
  U_hc_max <- 1.0 # maximum heating coil signal
  U_d_min <- 0.0 # minimum mixing box damper
  U_d_max <- 1.0 # maximum mixing box damper
  U_h_min <- 0.0 # minimum humidity command
  U_h_max <- 1.0 # maximum humidity command
  U_r_min <- 0.0 # minimum recovery command
  U_r_max <- 1.0 # maximum recovery command
  Ep_r <- 1.0 # recovery signal threshold
  Ep_T_max <- 100 # maximum allowable temperature difference
  H_ra.s_min <- 20.0 # minimum return air humidity set point
  H_ra.s_max <- 60.0 # maximum return air humidity set point
  Del.cc_max <- 1.0 # amount of sign changes per hour of cooling coil control signal
  Del.hc_max <- 1.0 # amount of sign changes per hour of heating coil control signal
  Del.h_max <- 1.0 # amount of sign changes per hour of humidifier control signal
  T_set_max <- 76 # maximum air temperature set point
  T_set_min <- 60 # minimum air temperature set point
  MT_max <- 4 # number of mode transitions per hour
  Occ_min <- 0 # minimum occupancy value
  Occ_max <- 1 # maximum occupancy value
  U_od_min <- 0.0 # minimum outdoor air damper value
  U_od_max <- 1.0 # maximum outdoor air damper value
  Ep_U_od <- 0.005 # outdoor air damper threshold
  sensitivity <- 1.0 # threshold value modifier
  
  #################### MERGE DATAFRAMES ####################
  
  dfl5 <- c(Andover.dfl, Siemens.dfl)
  saveRDS(names(dfl5),"./supplements/analysis_building_names.rds")
  
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
  dfl <- lapply(dflOA, function(x) x[sapply(x, nrow)>0])
  
  # building names
  b.names <- mapply(function(x,y) rep_len(y, length.out = length(x)), x = dfl, y = names(dfl), SIMPLIFY = F)
  saveRDS(b.names)
  
  # AHU names
  a.names <- lapply(dfl, names)
  
  # nrow (length) for each AHU dataframe
  dfl.lengths <- lapply(dfl, function(x) lapply(x, function(y) nrow(y)))
  
  #################### SET BUILDING METADATA ####################
  
  ##### Percent Outside Air #####
  ut_cfm <- readRDS("./Shiny-FDD/RDS/ut_cfm_and_occupancy.rds")
  
  # OA per AHU
  pct_OA <- function(df, b) {
    OA <- list()
    for (i in 1:length(df)) {
      a <- names(df)[i]
      OA[[i]] <- ut_cfm$Percent.OA[(ut_cfm$Building %in% b | ut_cfm$Alt.Abbrev %in% b) & ut_cfm$AHU %in% a][!is.na(ut_cfm$Percent.OA[(ut_cfm$Building %in% b | ut_cfm$Alt.Abbrev %in% b) & ut_cfm$AHU %in% a])] / 100
    }
    return(OA)
  }
  
  pctOAs <- mapply(function(df, b) pct_OA(df = df, b = b), df = dfl, b = b.names, SIMPLIFY = F)
  
  # add AHU names to list
  pctOAs <- mapply(function(dfl, pctOAs, df.len){
    names(pctOAs) <- names(dfl)
    for (i in 1:length(pctOAs)) {
      pctOAs[[i]] <- rep_len(pctOAs[[i]], length.out = df.len[[i]])
    }  
    return(pctOAs)
  }, dfl = dfl, pctOAs = pctOAs, df.len = dfl.lengths)
  
  # remove factors and leave only numeric columns 
  dfl <- lapply(dfl, function(x) lapply(x, function(df) df[, sapply(df, class) != "factor"]))
  
  ut_bldg_index <- readRDS("./Shiny-FDD/RDS/ut_bldg_index.rds")
  APAR.dfl <- dfl
  names(APAR.dfl)[names(APAR.dfl) %in% ut_bldg_index$Building] <- sapply(names(APAR.dfl)[names(APAR.dfl) %in% ut_bldg_index$Building], function(x) ut_bldg_index$Bldg.ID[grepl(x, ut_bldg_index$Building)])
  names(APAR.dfl)[names(APAR.dfl) %in% ut_bldg_index$Alt.Abbrev] <- sapply(names(APAR.dfl)[names(APAR.dfl) %in% ut_bldg_index$Alt.Abbrev], function(x) ut_bldg_index$Bldg.ID[grepl(x, ut_bldg_index$Alt.Abbrev)])
  saveRDS(APAR.dfl, "./Shiny-FDD/RDS/APAR_dfl.rds")
  
  #################### APAR RULE DEFINITIONS ####################
  
  ##### Mode 1: Heating #####
  Mode_1 <- function(df, OA) {
    
    # Normalized heating valve control, u.hc
    if ("HDV" %in% names(df)) {
      u.hc <- normalize(df$HDV, range = c(0,1))
    } else if ("HDVFB" %in% names(df)) {
      u.hc <- normalize(df$HDVFB, range = c(0,1))
    } else if ("HCV" %in% names(df)) {
      u.hc <- normalize(df$HCV, range = c(0,1))
    } else {
      u.hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Heating valve control, hc
    if ("HDV" %in% names(df)) {
      hc <- df$HDV
    } else if ("HDVFB" %in% names(df)) {
      hc <- df$HDVFB
    } else if ("HCV" %in% names(df)) {
      hc <- df$HCV
    } else {
      hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Supply air set point temperature, T.sas
    if ("HDS" %in% names(df)) {
      T.sas <- df$HDS
    } else if ("HDSAS" %in% names(df)) {
      T.sas <- df$HDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas <- df$RMS
    } else {
      T.sas <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Supply air temperature, T.sa
    if ("HDT" %in% names(df)) {
      T.sa <- df$HDT
    } else if ("HDSAT" %in% names(df)) {
      T.sa <- df$HDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa <- df$RMT
    } else {
      T.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("HDMAT" %in% names(df)) {
      T.ma <- df$HDMAT
    } else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    # Q.oa
    if (!all(is.na(T.ma)) & !all(is.na(T.ra))) {
      Q.oa <- T.ma - T.ra
    } else {
      Q.oa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Q.sa
    if (!all(is.na(T.oa)) & !all(is.na(T.ra))) {
      Q.sa <- T.oa - T.ra
    } else {
      Q.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 01 #####
    if (!all(is.na(T.sa)) & !all(is.na(T.ma)) & !all(is.na(hc))) {
      Rule01 <- mapply(function(T.ma,T.sa,hc) {
        if (!is.na(T.sa) & !is.na(T.ma) & !is.na(hc)) {
          if (hc > 0) {
            if (T.sa < (T.ma + Del.T_sf - Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.ma=T.ma, T.sa=T.sa, hc=hc, SIMPLIFY = T)
    } else {
      Rule01 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 02 #####
    if (!all(is.na(T.ra)) & !all(is.na(T.sa)) & !all(is.na(Q.oa)) & !all(is.na(Q.sa)) & !all(is.na(hc))) {
      Rule02 <- mapply(function(T.ra,T.sa,T.oa,Q.oa,Q.sa,hc) {
        if (!is.na(T.ra) & !is.na(T.sa) & !is.na(Q.oa) & !is.na(hc) & !is.na(Q.sa) & !is.na(T.oa)) {
          if (hc > 0) {
            if (abs(T.ra - T.oa) >= Del.T_min) {
              if (abs(Q.oa/Q.sa - Q_oa_min) > Ep_f) {
                1
              } else {
                0
              }
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.ra=T.ra, T.sa=T.sa, T.oa=T.oa, Q.oa=Q.oa, Q.sa=Q.sa, hc=hc, SIMPLIFY = T)
    } else {
      Rule02 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 03 #####
    if (!all(is.na(u.hc)) & !all(is.na(T.sas)) & !all(is.na(T.sa)) & !all(is.na(hc))) {
      Rule03 <- mapply(function(T.sas,T.sa,u.hc,hc) {
        if (!is.na(u.hc) & !is.na(T.sas) & !is.na(T.sa) & !is.na(hc)) {
          if (hc > 0) {
            if (abs(u.hc - 1) <= Ep_hc & (T.sas - T.sa) >= Ep_t) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sas=T.sas, T.sa=T.sa, u.hc=u.hc, hc=hc, SIMPLIFY = T)
    } else {
      Rule03 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ###### Rule 04 #####
    if (!all(is.na(u.hc)) & !all(is.na(hc))) {
      Rule04 <- mapply(function(u.hc,hc) {
        if (!is.na(u.hc) & !is.na(hc)) {
          if (hc > 0) {
            if (abs(u.hc - 1) <= Ep_hc) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, u.hc=u.hc, hc=hc, SIMPLIFY = T)
    } else {
      Rule04 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Mode01 <- list(Rule01, Rule02, Rule03, Rule04)
    names(Mode01) <- c("Rule01", "Rule02", "Rule03", "Rule04")
    
    return(Mode01)
    
  }
  
  ##### Mode 2: Cooling with outdoor air #####
  Mode_2 <- function(df, OA) {
    
    # Supply air set point temperature, T.sas
    if ("CDS" %in% names(df)) {
      T.sas <- df$CDS
    } else if ("CDSAS" %in% names(df)) {
      T.sas <- df$CDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas <- df$RMS
    } else {
      T.sas <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    # Cooling valve control, cc
    if ("CDV" %in% names(df)) {
      cc <- df$CDV
    } else if ("CDVFB" %in% names(df)) {
      cc <- df$CDVFB
    } else if ("CCV" %in% names(df)) {
      cc <- df$CCV
    } else if ("CHWO" %in% names(df)) {
      cc <- df$CHWO
    } else {
      cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    cc <- ifelse(is.na(cc), 0, cc)
    
    # Heating valve control, hc
    if ("HDV" %in% names(df)) {
      hc <- df$HDV
    } else if ("HDVFB" %in% names(df)) {
      hc <- df$HDVFB
    } else if ("HCV" %in% names(df)) {
      hc <- df$HCV
    } else {
      hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    hc <- ifelse(is.na(hc), 0, hc)
    
    # Supply air temperature, T.sa
    if ("CDT" %in% names(df)) {
      T.sa <- df$CDT
    } else if ("CDSAT" %in% names(df)) {
      T.sa <- df$CDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa <- df$RMT
    } else {
      T.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("CDMAT" %in% names(df)) {
      T.ma <- df$CDMAT
    } else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    ##### Rule 05 #####
    if (!all(is.na(T.sas)) & !all(is.na(hc)) & !all(is.na(cc))) {
      Rule05 <- mapply(function(T.sas,T.oa,hc,cc) {
        if (!is.na(T.sas) & !is.na(T.oa) & !is.na(hc) & !is.na(cc)) {
          if (hc == 0 & cc == 0) {
            if (T.oa > (T.sas - Del.T_sf + Ep_t)) {
              1
            } else {
              0
            } 
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sas=T.sas, T.oa=T.oa, hc=hc, cc=cc, SIMPLIFY = T)
    } else {
      Rule05 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 06 #####
    if (!all(is.na(T.ra)) & !all(is.na(T.sa)) & !all(is.na(hc)) & !all(is.na(cc))) {
      Rule06 <- mapply(function(T.ra,T.sa,hc,cc) {
        if (!is.na(T.sa) & !is.na(T.ra) & !is.na(hc) & !is.na(cc)) {
          if (hc == 0 & cc == 0) {
            if (T.sa > (T.ra - Del.T_rf + Ep_t)) {
              1
            } else {
              0
            } 
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0) 
        }
      }, T.ra=T.ra, T.sa=T.sa, hc=hc, cc=cc, SIMPLIFY = T)
    } else {
      Rule06 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 07 #####
    if (!all(is.na(T.ma)) & !all(is.na(T.sa)) & !all(is.na(hc)) & !all(is.na(cc))) {
      Rule07 <- mapply(function(T.ma,T.sa,hc,cc) {
        if (!is.na(T.ma) & !is.na(T.sa) & !is.na(hc) & !is.na(cc)) {
          if (hc == 0 & cc == 0) {
            if (abs((T.sa - Del.T_sf) - T.ma) > Ep_t) {
              1
            } else {
              0
            } 
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0) 
        }
      }, T.ma=T.ma, T.sa=T.sa, hc=hc, cc=cc, SIMPLIFY = T)
    } else {
      Rule07 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Mode02 <- list(Rule05, Rule06, Rule07)
    names(Mode02) <- c("Rule05", "Rule06", "Rule07")
    
    return(Mode02)
    
  }
  
  ##### Mode 3: Mechanical cooling with 100% outdoor air #####
  Mode_3 <- function(df, OA) {
    
    # Normalized cooling valve control, u.cc
    if ("CDV" %in% names(df)) {
      u.cc <- normalize(df$CDV, range = c(0,1))
    } else if ("CDVFB" %in% names(df)) {
      u.cc <- normalize(df$CDVFB, range = c(0,1))
    } else if ("CCV" %in% names(df)) {
      u.cc <- normalize(df$CCV, range = c(0,1))
    } else if ("CHWO" %in% names(df)) {
      u.cc <- normalize(df$CHWO, range = c(0,1))
    } else {
      u.cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Cooling valve control, cc
    if ("CDV" %in% names(df)) {
      cc <- df$CDV
    } else if ("CDVFB" %in% names(df)) {
      cc <- df$CDVFB
    } else if ("CCV" %in% names(df)) {
      cc <- df$CCV
    } else if ("CHWO" %in% names(df)) {
      cc <- df$CHWO
    } else {
      cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    cc <- ifelse(is.na(cc), 0, cc)
    
    # Supply air set point temperature, T.sas
    if ("CDS" %in% names(df)) {
      T.sas <- df$CDS
    } else if ("CDSAS" %in% names(df)) {
      T.sas <- df$CDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas <- df$RMS
    } else {
      T.sas <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Supply air temperature, T.sa
    if ("CDT" %in% names(df)) {
      T.sa <- df$CDT
    } else if ("CDSAT" %in% names(df)) {
      T.sa <- df$CDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa <- df$RMT
    } else {
      T.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("CDMAT" %in% names(df)) {
      T.ma <- df$CDMAT
    } else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    # difference between outdoor air temperature and the supply air set point temp
    T.oa100 <- ifelse(!is.na(T.sas), 
                      ifelse(!is.na(T.oa), 
                             ifelse(T.oa-T.sas <= 0, 1, 0), 
                             T.oa), 
                      T.sas)
                          
    ##### Rule 08 #####
    if (!all(is.na(T.sas)) & !all(is.na(T.oa100)) & !all(is.na(cc))) {
      Rule08 <- mapply(function(T.oa,T.sas,T.oa100,cc) {
        if (!is.na(T.sas) & !is.na(T.oa) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (T.oa < (T.sas - Del.T_sf + Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0) 
        }
      }, T.oa=T.oa, T.sas=T.sas, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule08 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 09 #####
    Rule09 <- mapply(function(T.oa,T.oa100,cc) {
      if (!is.na(T.oa) & !is.na(T.oa100) & !is.na(cc)) {
        if (T.oa100 == 1 & cc > 0) {
          if (T.oa > (T_co + Ep_t)) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      } else {
       as.numeric(0) 
      }
    }, T.oa=T.oa, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    
    ##### Rule 10 #####
    if (!all(is.na(T.ma)) & !all(is.na(T.oa100)) & !all(is.na(cc))) {
      Rule10 <- mapply(function(T.oa,T.ma,T.oa100,cc) {
        if (!is.na(T.ma) & !is.na(T.oa) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (abs(T.oa - T.ma) > Ep_t) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.ma=T.ma, T.oa=T.oa, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule10 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 11 #####
    if (!all(is.na(T.ma)) & !all(is.na(T.sa)) & !all(is.na(T.oa100)) & !all(is.na(cc))) {
      Rule11 <- mapply(function(T.sa,T.ma,T.oa100,cc) {
        if (!is.na(T.ma) & !is.na(T.sa) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (T.sa > (T.ma + Del.T_sf + Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sa=T.sa, T.ma=T.ma, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule11 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    # Rule 12
    if (!all(is.na(T.ra)) & !all(is.na(T.sa)) & !all(is.na(T.oa100)) & !all(is.na(cc))) {
      Rule12 <- mapply(function(T.sa,T.ra,T.oa100,cc) {
        if (!is.na(T.sa) & !is.na(T.ra) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (T.sa > (T.ra - Del.T_rf + Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sa=T.sa, T.ra=T.ra, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule12 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 13 #####
    if (!all(is.na(u.cc)) & !all(is.na(T.sas)) & !all(is.na(cc)) & !all(is.na(T.sa)) & !all(is.na(T.oa100))) {
      Rule13 <- mapply(function(T.sa,T.sas,u.cc,T.oa100,cc) {
        if (!is.na(T.sas) & !is.na(T.sa) & !is.na(u.cc) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (abs(u.cc - 1) <= Ep_cc & (T.sa - T.sas) >= Ep_t) {
              1
            } else {
              0
            } 
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sa=T.sa, T.sas=T.sas, u.cc=u.cc, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule13 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 14 #####
    if (!all(is.na(u.cc)) & !all(is.na(T.oa100)) & !all(is.na(cc))) {
      Rule14 <- mapply(function(u.cc,T.oa100,cc) {
        if (!is.na(u.cc) & !is.na(T.oa100) & !is.na(cc)) {
          if (T.oa100 == 1 & cc > 0) {
            if (abs(u.cc - 1) <= Ep_cc) {
              1
            } else {
              0
            } 
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc, T.oa100=T.oa100, cc=cc, SIMPLIFY = T)
    } else {
      Rule14 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Mode03 <- list(Rule08, Rule09, Rule10, Rule11, Rule12, Rule13, Rule14)
    names(Mode03) <- c("Rule08", "Rule09", "Rule10", "Rule11", "Rule12", "Rule13", "Rule14")
    
    return(Mode03)
    
  }
  
  ##### Mode 4: Mechanical cooling with minimum outdoor air #####
  Mode_4 <- function(df, OA) {
    
    # Normalized cooling valve control, u.cc
    if ("CDV" %in% names(df)) {
      u.cc <- normalize(df$CDV, range = c(0,1))
    } else if ("CDVFB" %in% names(df)) {
      u.cc <- normalize(df$CDVFB, range = c(0,1))
    } else if ("CCV" %in% names(df)) {
      u.cc <- normalize(df$CCV, range = c(0,1))
    } else if ("CHWO" %in% names(df)) {
      u.cc <- normalize(df$CHWO, range = c(0,1))
    } else {
      u.cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Cooling valve control, cc
    if ("CDV" %in% names(df)) {
      cc <- df$CDV
    } else if ("CDVFB" %in% names(df)) {
      cc <- df$CDVFB
    } else if ("CCV" %in% names(df)) {
      cc <- df$CCV
    } else if ("CHWO" %in% names(df)) {
      cc <- df$CHWO
    } else {
      cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    cc <- ifelse(is.na(cc), 0, cc)
    
    # Heating valve control, hc
    if ("HDV" %in% names(df)) {
      hc <- df$HDV
    } else if ("HDVFB" %in% names(df)) {
      hc <- df$HDVFB
    } else if ("HCV" %in% names(df)) {
      hc <- df$HCV
    } else {
      hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    hc <- ifelse(is.na(hc), 0, hc)
    
    # Supply air set point temperature, T.sas
    if ("CDS" %in% names(df)) {
      T.sas <- df$CDS
    } else if ("CDSAS" %in% names(df)) {
      T.sas <- df$CDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas <- df$RMS
    } else {
      T.sas <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Supply air temperature, T.sa
    if ("CDT" %in% names(df)) {
      T.sa <- df$CDT
    } else if ("CDSAT" %in% names(df)) {
      T.sa <- df$CDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa <- df$RMT
    } else {
      T.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("CDMAT" %in% names(df)) {
      T.ma <- df$CDMAT
    } else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    # Q.oa
    if (!all(is.na(T.ma)) & !all(is.na(T.ra))) {
      Q.oa <- T.ma - T.ra
    } else {
      Q.oa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Q.sa
    if (!all(is.na(T.oa)) & !all(is.na(T.ra))) {
      Q.sa <- T.oa - T.ra
    } else {
      Q.sa <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 15 #####
    if (!all(is.na(cc)) & !all(is.na(Q.sa))) {
      Rule15 <- mapply(function(T.oa,Q.sa,hc,cc) {
        if (!is.na(T.oa) & !is.na(Q.sa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (T.oa < (T_co - Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.oa=T.oa, Q.sa=Q.sa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule15 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 16 #####
    if (!all(is.na(T.sa)) & !all(is.na(T.ma)) & !all(is.na(cc)) & !all(is.na(Q.sa))) {
      Rule16 <- mapply(function(T.sa,T.ma,T.oa,Q.sa,cc,hc) {
        if (!is.na(T.sa) & !is.na(T.ma) & !is.na(T.oa) & !is.na(Q.sa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (T.sa > (T.ma + Del.T_sf + Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sa=T.sa,T.ma=T.ma, Q.sa=Q.sa, T.oa=T.oa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule16 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 17 #####
    if (!all(is.na(T.ra)) & !all(is.na(T.sa)) & !all(is.na(Q.sa)) & !all(is.na(cc))) {
      Rule17 <- mapply(function(T.sa,T.ra,Q.sa,cc,hc) {
        if (!is.na(T.ra) & !is.na(T.sa) & !is.na(Q.sa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (T.sa > (T.ra - Del.T_rf + Ep_t)) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.sa=T.sa,T.ra=T.ra, Q.sa=Q.sa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule17 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 18 #####
    if (!all(is.na(T.ra)) & !all(is.na(Q.oa)) & !all(is.na(Q.sa)) & !all(is.na(cc))) {
      Rule18 <- mapply(function(T.oa,T.ra,Q.oa,Q.sa,hc,cc) {
        if (!is.na(T.ra) & !is.na(Q.oa) & !is.na(Q.sa) & !is.na(T.oa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (abs(T.ra - T.oa) >= Del.T_min) {
              if (abs(Q.oa/Q.sa - Q_oa_min) > Ep_f) {
                1
              } else {
                0
              }
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, T.oa=T.oa,T.ra=T.ra,Q.oa=Q.oa, Q.sa=Q.sa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule18 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 19 #####
    if (!all(is.na(u.cc)) & !all(is.na(T.sas)) & !all(is.na(T.sa)) & !all(is.na(Q.sa)) & !all(is.na(cc))) {
      Rule19 <- mapply(function(u.cc,T.sas,T.sa,Q.sa,hc,T.oa,cc) {
        if (!is.na(u.cc) & !is.na(T.sas) & !is.na(T.sa) & !is.na(Q.sa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (abs(u.cc - 1) <= Ep_cc & (T.sa - T.sas) >= Ep_t) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc,T.sas=T.sas,T.sa=T.sa, Q.sa=Q.sa, T.oa=T.oa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule19 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 20 #####
    if (!all(is.na(u.cc)) & !all(is.na(Q.sa)) & !all(is.na(cc))) {
      Rule20 <- mapply(function(u.cc,Q.sa,T.oa,cc,hc) {
        if (!is.na(u.cc) & !is.na(Q.sa) & !is.na(cc)) {
          if (Q.sa > 0 & (cc > 0 | hc == 0)) {
            if (abs(u.cc - 1) <= Ep_cc) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc, Q.sa=Q.sa, T.oa=T.oa, cc=cc, hc=hc, SIMPLIFY = T)
    } else {
      Rule20 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Mode04 <- list(Rule15, Rule16, Rule17, Rule18, Rule19, Rule20)
    names(Mode04) <- c("Rule15", "Rule16", "Rule17", "Rule18", "Rule19", "Rule20")
    
    return(Mode04)
    
  }
  
  ##### Mode 5: Unknown Occupied Modes #####
  Mode_5 <- function(df, OA) {
    
    # Normalized heating valve control, u.hc
    if ("HDV" %in% names(df)) {
      u.hc <- normalize(df$HDV, range = c(0,1))
    } else if ("HDVFB" %in% names(df)) {
      u.hc <- normalize(df$HDVFB, range = c(0,1))
    } else if ("HCV" %in% names(df)) {
      u.hc <- normalize(df$HCV, range = c(0,1))
    } else {
      u.hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Normalized cooling valve control, u.cc
    if ("CDV" %in% names(df)) {
      u.cc <- normalize(df$CDV, range = c(0,1))
    } else if ("CDVFB" %in% names(df)) {
      u.cc <- normalize(df$CDVFB, range = c(0,1))
    } else if ("CCV" %in% names(df)) {
      u.cc <- normalize(df$CCV, range = c(0,1))
    } else if ("CHWO" %in% names(df)) {
      u.cc <- normalize(df$CHWO, range = c(0,1))
    } else {
      u.cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    ### MORE DAMPERS NEEDED
    # Normalized damper control, u.d
    if ("MAD" %in% names(df)) {
      u.d <- normalize(as.numeric(df$MAD), range = c(0,1))
    } else if ("RAD" %in% names(df)) {
      u.d <- normalize(as.numeric(df$RAD), range = c(0,1))
    } else if ("OAD" %in% names(df)) {
      u.d <- normalize(as.numeric(df$OAD), range = c(0,1))
    } else {
      u.d <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 21 #####
    if (!all(is.na(u.cc)) & !all(is.na(u.hc)) & !all(is.na(u.d))) {
      Rule21 <- mapply(function(u.cc,u.hc,u.d) {
        if (!is.na(u.cc) & !is.na(u.hc) & !is.na(u.d)) {
          if ((u.cc > Ep_cc) & (u.hc > Ep_hc) & ((Ep_d < u.d) & (u.d < (1 - Ep_d)))) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc,u.hc=u.hc,u.d=u.d, SIMPLIFY = T)
    } else {
      Rule21 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }  
    
    # Rule 22
    if (!all(is.na(u.cc)) & !all(is.na(u.hc))) {
      Rule22 <- mapply(function(u.cc,u.hc) {
        if (!is.na(u.cc) & !is.na(u.hc) & !is.na(u.hc)) {
          if (u.cc > Ep_cc & u.hc > Ep_hc) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc,u.hc=u.hc, SIMPLIFY = T)
    } else {
      Rule22 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }  
    
    ##### Rule 23 #####
    if (!all(is.na(u.hc)) & !all(is.na(u.d))) {
      Rule23 <- mapply(function(u.hc,u.d) {
        if (!is.na(u.hc) & !is.na(u.d)) {
          if (u.d > Ep_d & u.hc > Ep_hc) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, u.hc=u.hc,u.d=u.d, SIMPLIFY = T)
    } else {
      Rule23 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }  
    
    ##### Rule 24 #####
    if (!all(is.na(u.cc)) & !all(is.na(u.d))) {
      Rule24 <- mapply(function(u.cc,u.d) {
        if (!is.na(u.cc) & !is.na(u.d)) {
          if (u.cc > Ep_cc & ((Ep_d < u.d)  & (u.d < (1 - Ep_d)))) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, u.cc=u.cc,u.d=u.d, SIMPLIFY = T)
    } else {
      Rule24 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }  
    
    Mode05 <- list(Rule21, Rule22, Rule23, Rule24)
    names(Mode05) <- c("Rule21", "Rule22", "Rule23", "Rule24")
    
    return(Mode05)
    
  }
  
  ##### Mode 6: All Occupied Modes (Applies to Modes 1 - 5) #####
  Mode_6 <- function(df, OA) {
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("CDMAT" %in% names(df)) {
      T.ma <- df$CDMAT
    } else if ("HDMAT" %in% names(df)) {
      T.ma <- df$HDMAT
    }else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    # Rule 25C
    if("CDT|CDS" %in% names(df)){
      
      # CD Supply air set point temperature, T.sas_c
      if ("CDS" %in% names(df)) {
        T.sas_c <- df$CDS
      } else if ("CDSAS" %in% names(df)) {
        T.sas_c <- df$CDSAS
      } else if ("SAS" %in% names(df)) {
        T.sas_c <- df$SAS
      } else if ("RMS" %in% names(df)) {
        T.sas_c <- df$RMS
      } else {
        T.sas_c <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
      }
      
      # CD Supply air temperature, T.sa_c
      if ("CDT" %in% names(df)) {
        T.sa_c <- df$CDT
      } else if ("CDSAT" %in% names(df)) {
        T.sa_c <- df$CDSAT
      } else if ("SAT" %in% names(df)) {
        T.sa_c <- df$SAT
      } else if ("RMT" %in% names(df)) {
        T.sa_c <- df$RMT
      } else {
        T.sa_c <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
      }
      
      ##### Rule 25C #####
      if (!all(is.na(T.sas_c)) & !all(is.na(T.sa_c))) {
        Rule25C <- mapply(function(T.sas_c,T.sa_c) {
          if (!is.na(T.sas_c) & !is.na(T.sa_c)) {
            if (abs(T.sa_c - T.sas_c) > Ep_t) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        }, T.sas_c=T.sas_c,T.sa_c=T.sa_c, SIMPLIFY = T)
      } else {
        Rule25C <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
      } 
      
    } else {
      
      Rule25C <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
      
    }
    
    # Rule 25H
    if("HDT|HDS" %in% names(df)){
      
      # HD Supply air set point temperature, T.sas_h
      if ("HDS" %in% names(df)) {
        T.sas_h <- df$HDS
      } else if ("HDSAS" %in% names(df)) {
        T.sas_h <- df$HDSAS
      } else if ("SAS" %in% names(df)) {
        T.sas_h <- df$SAS
      } else if ("RMS" %in% names(df)) {
        T.sas_h <- df$RMS
      } else {
        T.sas_h <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
      }
      
      # HD Supply air temperature, T.sa_h
      if ("HDT" %in% names(df)) {
        T.sa_h <- df$HDT
      } else if ("HDSAT" %in% names(df)) {
        T.sa_h <- df$HDSAT
      } else if ("SAT" %in% names(df)) {
        T.sa_h <- df$SAT
      } else if ("RMT" %in% names(df)) {
        T.sa_h <- df$RMT
      } else {
        T.sa_h <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
      }
      
      ##### Rule 25H #####
      if (!all(is.na(T.sas_h)) & !all(is.na(T.sa_h))) {
        Rule25H <- mapply(function(T.sas_h,T.sa_h) {
          if (!is.na(T.sas_h) & !is.na(T.sa_h)) {
            if (abs(T.sa_h - T.sas_h) > Ep_t) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        }, T.sas_h=T.sas_h,T.sa_h=T.sa_h, SIMPLIFY = T)
      } else {
        Rule25H <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
      }
      
    } else {
      
      Rule25H <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
      
    }
    
    ##### Rule 26 #####
    if (!all(is.na(T.ra)) & !all(is.na(T.ma))) {
      Rule26 <- mapply(function(T.ra,T.ma,T.oa) {
        if (!is.na(T.ma) & !is.na(T.ra) & !is.na(T.oa)) {
          if (T.ma < min(T.ra, T.oa) - Ep_t) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.ra=T.ra,T.ma=T.ma,T.oa=T.oa, SIMPLIFY = T)
    } else {
      Rule26 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    ##### Rule 27 #####
    if (!all(is.na(T.ra)) & !all(is.na(T.ma))) {
      Rule27 <- mapply(function(T.ra,T.ma,T.oa) {
        if (!is.na(T.ma) & !is.na(T.ra) & !is.na(T.oa)) {
          if (T.ma > max(T.ra, T.oa) + Ep_t) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.ra=T.ra,T.ma=T.ma,T.oa=T.oa, SIMPLIFY = T)
    } else {
      Rule27 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Mode06 <- list(Rule25C, Rule25H, Rule26, Rule27)
    names(Mode06) <- c("Rule25C", "Rule25H", "Rule26", "Rule27")
    
    return(Mode06)
    
  }
  
  #################### APPLY APAR ####################
  
  # 0% outside air for unfilled pctOAs
  pctOAs <- lapply(pctOAs, function(x) lapply(x, function(y) if(all(is.na(y))) { y[all(is.na(y))] <- rep(0, length(y)) } else { y }))
  
  #### Framework for applying the rules #####
  
  mode.1 <- mapply(function(OA,df) mapply(Mode_1, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  mode.2 <- mapply(function(OA,df) mapply(Mode_2, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  mode.3 <- mapply(function(OA,df) mapply(Mode_3, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  mode.4 <- mapply(function(OA,df) mapply(Mode_4, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  mode.5 <- mapply(function(OA,df) mapply(Mode_5, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  mode.6 <- mapply(function(OA,df) mapply(Mode_6, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)

  #################### EXTENDED RULE-SET ####################
  
  # function to find n sets of value5 min repetitions in vector rule. returns single numeric
  fault_frequency <- function(rule, value, n){
    ans <- sum(ifelse(rle(rule == value)$values == TRUE, 
                      rle(rule == value)$lengths, NA)[!is.na(ifelse(rle(rule == value)$values == TRUE, 
                                                                  rle(rule == value)$lengths, NA))] > n)
    return(ans)
  }
  
  extended_rules <- function(df, OA) {
    
    # Mixing box damper control, md
    if ("MAD" %in% names(df)) {
      md <- as.numeric(df$MAD)
    } else {
      md <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }

    # Return air damper control, rd
    if ("RAD" %in% names(df)) {
      rd <- as.numeric(df$RAD)
    } else {
      rd <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Outside air damper control, od
    if ("OAD" %in% names(df)) {
      od <- as.numeric(df$OAD)
    } else {
      od <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Heating valve control, hc
    if ("HDV" %in% names(df)) {
      hc <- df$HDV
    } else if ("HDVFB" %in% names(df)) {
      hc <- df$HDVFB
    } else if ("HCV" %in% names(df)) {
      hc <- df$HCV
    } else {
      hc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    hc <- ifelse(is.na(hc), 0, hc)
    
    # Cooling valve control, cc
    if ("CDV" %in% names(df)) {
      cc <- df$CDV
    } else if ("CDVFB" %in% names(df)) {
      cc <- df$CDVFB
    } else if ("CCV" %in% names(df)) {
      cc <- df$CCV
    } else if ("CHWO" %in% names(df)) {
      cc <- df$CHWO
    } else {
      cc <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    cc <- ifelse(is.na(cc), 0, cc)
    
    # Mixed air temperature, T.ma
    if ("MAT" %in% names(df)) {
      T.ma <- df$MAT
    } else if ("HDMAT" %in% names(df)) {
      T.ma <- df$HDMAT
    } else if ("RAT" %in% names(df)) {
      T.ma <- df$Global.OAT * OA + df$RAT * (1 - OA)
    } else {
      T.ma <- df$Global.OAT * OA + rep(75, length.out = length(df[["Global.OAT"]])) * (1 - OA)
    }
    
    # Return air temperature, T.ra
    if ("RAT" %in% names(df)) {
      T.ra <- df$RAT
    } else {
      T.ra <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Room ambient temperature, T.a
    if ("RMT" %in% names(df)) {
      T.a <- df$RMT
    } else {
      T.a <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Room ambient temperature set point, T.as
    if ("RMS" %in% names(df)) {
      T.as <- df$RMS
    } else {
      T.as <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # CD Supply air set point temperature, T.sas_c
    if ("CDS" %in% names(df)) {
      T.sas_c <- df$CDS
    } else if ("CDSAS" %in% names(df)) {
      T.sas_c <- df$CDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas_c <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas_c <- df$RMS
    } else {
      T.sas_c <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # CD Supply air temperature, T.sa_c
    if ("CDT" %in% names(df)) {
      T.sa_c <- df$CDT
    } else if ("CDSAT" %in% names(df)) {
      T.sa_c <- df$CDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa_c <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa_c <- df$RMT
    } else {
      T.sa_c <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # HD Supply air set point temperature, T.sas_h
    if ("HDS" %in% names(df)) {
      T.sas_h <- df$HDS
    } else if ("HDSAS" %in% names(df)) {
      T.sas_h <- df$HDSAS
    } else if ("SAS" %in% names(df)) {
      T.sas_h <- df$SAS
    } else if ("RMS" %in% names(df)) {
      T.sas_h <- df$RMS
    } else {
      T.sas_h <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # HD Supply air temperature, T.sa_h
    if ("HDT" %in% names(df)) {
      T.sa_h <- df$HDT
    } else if ("HDSAT" %in% names(df)) {
      T.sa_h <- df$HDSAT
    } else if ("SAT" %in% names(df)) {
      T.sa_h <- df$SAT
    } else if ("RMT" %in% names(df)) {
      T.sa_h <- df$RMT
    } else {
      T.sa_h <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Cooling supply air fan speed control, sf_c
    if ("SAF" %in% names(df)) {
      sf_c <- as.numeric(df$SAF)
    } else if ("VFD" %in% names(df)) {
      sf_c <- as.numeric(df$VFD)
    } else if ("SVD" %in% names(df)) {
      sf_c <- as.numeric(df$SVD)
    } else if ("CDF" %in% names(df)) {
      sf_c <- as.numeric(df$CDF)
    } else if ("CD_SVD" %in% names(df)) {
      sf_c <- as.numeric(df$CD_SVD)
    } else if ("CD_VFD" %in% names(df)) {
      sf_c <- as.numeric(df$CD_VFD)
    } else {
      sf_c <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Heating supply air fan speed control, sf_h
    if ("SAF" %in% names(df)) {
      sf_h <- as.numeric(df$SAF)
    } else if ("VFD" %in% names(df)) {
      sf_h <- as.numeric(df$VFD)
    } else if ("SVD" %in% names(df)) {
      sf_h <- as.numeric(df$SVD)
    } else if ("HDF" %in% names(df)) {
      sf_h <- as.numeric(df$HDF)
    } else if ("HD_SVD" %in% names(df)) {
      sf_h <- as.numeric(df$HD_SVD)
    } else if ("HD_VFD" %in% names(df)) {
      sf_h <- as.numeric(df$HD_VFD)
    } else {
      sf_h <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # Return air fan speed control, rf
    if ("RAF" %in% names(df)) {
      rf <- as.numeric(df$RAF)
    } else if ("VFD.1" %in% names(df)) {
      rf <- as.numeric(df$VFD.1)
    } else if ("SVD.1" %in% names(df)) {
      rf <- as.numeric(df$SVD.1)
    } else {
      rf <- rep(as.numeric(NA), length.out = length(df[["Global.OAT"]]))
    }
    
    # T.oa
    T.oa <- df$Global.OAT
    
    ##### Rule 29H #####
    if (!all(is.na(T.sa_h)) & !all(is.na(T.a))) {
      Rule29H <- mapply(function(T.sa_h,T.a) {
        if (!is.na(T.sa_h) & !is.na(T.a)) {
          if (T.sa_h >= T.a+30) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.a=T.a,T.sa_h=T.sa_h, SIMPLIFY = T)
    } else {
      Rule29H <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule29H <- sum(Rule29H[!is.na(Rule29H)])
    
    ##### Rule 29C #####
    if (!all(is.na(T.sa_c)) & !all(is.na(T.a))) {
      Rule29C <- mapply(function(T.sa_c,T.a) {
        if (!is.na(T.sa_c) & !is.na(T.a)) {
          if (T.sa_c >= T.a+30) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.a=T.a,T.sa_c=T.sa_c, SIMPLIFY = T)
    } else {
      Rule29C <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule29C <- sum(Rule29C[!is.na(Rule29C)])
    
    ##### Rule 31 #####
    if (!all(is.na(rf))) {
      Rule31 <- mapply(function(rf) {
        if (!is.na(rf)) {
          if (rf >= 95) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, rf=rf, SIMPLIFY = T)
    } else {
      Rule31 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    if (!all(is.na(Rule31))) {
      Rule31 <- fault_frequency(Rule31, 1, 16)
    } else {
      Rule31 <- 0
    }
    
    ##### Rule 35 #####
    if (!all(is.na(T.a)) & !all(is.na(cc)) & !all(cc == 0)) {
      Rule35 <- c()
      Rule35rle <- rle(as.integer(T.a[(cc > 0) & (!is.na(T.a))]))
      x <- Rule35rle$values
      if (length(x) > 1) {
        for (i in 1:(length(Rule35rle$values)-1)) {
          if (x[i] < x[i+1]) {
            Rule35[i] <- 1
          } else {
            Rule35[i] <- 0
          }
        }
        Rule35[length(Rule35rle$values)] <- 0
        Rule35 <- sum(as.integer(Rule35rle$lengths[Rule35rle$lengths > 16]/16),Rule35)
      } else {
        Rule35 <- 0
      }
    } else {
      Rule35 <- 0
    }
    
    ##### Rule 37 #####
    if (!all(is.na(T.as)) & !all(is.na(T.a))) {
      Rule37 <- mapply(function(T.as,T.a) {
        if (!is.na(T.as) & !is.na(T.a)) {
          if (T.a >= T.as*1.05) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.a=T.a,T.as=T.as, SIMPLIFY = T)
    } else {
      Rule37 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule37 <- sum(Rule37[!is.na(Rule37)])
    
    ##### Rule 40 #####
    if (!all(is.na(T.a))) {
      Rule40 <- mapply(function(T.a) {
        if (!is.na(T.a)) {
          if (T.a >= 85) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.a=T.a, SIMPLIFY = T)
    } else {
      Rule40 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule40 <- sum(Rule40[!is.na(Rule40)])
    
    ##### Rule 41 #####
    if (!all(is.na(sf_c))) {
      Rule41 <- mapply(function(sf_c) {
        if (!is.na(sf_c)) {
          if (sf_c < 0 | sf_c > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, sf_c=sf_c, SIMPLIFY = T)
    } else {
      Rule41 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule41 <- sum(Rule41[!is.na(Rule41)])
    
    ##### Rule 42 #####
    if (!all(is.na(sf_h))) {
      Rule42 <- mapply(function(sf_h) {
        if (!is.na(sf_h)) {
          if (sf_h < 0 | sf_h > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, sf_h=sf_h, SIMPLIFY = T)
    } else {
      Rule42 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule42 <- sum(Rule42[!is.na(Rule42)])
    
    ##### Rule 43 #####
    if (!all(is.na(rf))) {
      Rule43 <- mapply(function(rf) {
        if (!is.na(rf)) {
          if (rf < 0 | rf > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, rf=rf, SIMPLIFY = T)
    } else {
      Rule43 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule43 <- sum(Rule43[!is.na(Rule43)])
    
    ##### Rule 44 #####
    if (!all(is.na(T.ma))) {
      Rule44 <- mapply(function(T.ma) {
        if (!is.na(T.ma)) {
          if (T.ma < 45 | T.ma > 90) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.ma=T.ma, SIMPLIFY = T)
    } else {
      Rule44 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule44 <- sum(Rule44[!is.na(Rule44)])
    
    ##### Rule 45 #####
    if (!all(is.na(T.oa))) {
      Rule45 <- mapply(function(T.oa) {
        if (!is.na(T.oa)) {
          if (T.oa < 10 | T.oa > 120) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.oa=T.oa, SIMPLIFY = T)
    } else {
      Rule45 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule45 <- sum(Rule45[!is.na(Rule45)])
    
    ##### Rule 46 #####
    if (!all(is.na(T.sa_c))) {
      Rule46 <- mapply(function(T.sa_c) {
        if (!is.na(T.sa_c)) {
          if (T.sa_c < 35 | T.sa_c > 90) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.sa_c=T.sa_c, SIMPLIFY = T)
    } else {
      Rule46 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule46 <- sum(Rule46[!is.na(Rule46)])
    
    ##### Rule 47 #####
    if (!all(is.na(T.sa_h))) {
      Rule47 <- mapply(function(T.sa_h) {
        if (!is.na(T.sa_h)) {
          if (T.sa_h < 60 | T.sa_h > 140) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.sa_h=T.sa_h, SIMPLIFY = T)
    } else {
      Rule47 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule47 <- sum(Rule47[!is.na(Rule47)])
    
    ##### Rule 48 #####
    if (!all(is.na(T.ra))) {
      Rule48 <- mapply(function(T.ra) {
        if (!is.na(T.ra)) {
          if (T.ra < 15 | T.ra > 160) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.ra=T.ra, SIMPLIFY = T)
    } else {
      Rule48 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule48 <- sum(Rule48[!is.na(Rule48)])
    
    ##### Rule 49 #####
    if (!all(is.na(cc))) {
      Rule49 <- mapply(function(cc) {
        if (!is.na(cc)) {
          
          if (cc < 0 | cc > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, cc=cc, SIMPLIFY = T)
    } else {
      Rule49 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule49 <- sum(Rule49[!is.na(Rule49)])
    
    ##### Rule 50 #####
    if (!all(is.na(hc)) & !all(is.na(cc))) {
      Rule50 <- mapply(function(hc,cc) {
        if (!is.na(hc) & !is.na(cc)) {
          if (cc == 0) {
            if (hc < 0 | hc > 100) {
              1
            } else {
              0
            }
          } else {
            as.numeric(0)
          }
        } else {
          as.numeric(0)
        }
      }, hc=hc, cc=cc, SIMPLIFY = T)
    } else {
      Rule50 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule50 <- sum(Rule50[!is.na(Rule50)])
    
    ##### Rule 51 #####
    if (!all(is.na(rd))) {
      Rule51 <- mapply(function(rd) {
        if (!is.na(rd)) {
          if (rd < 0 | rd > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, rd=rd, SIMPLIFY = T)
    } else {
      Rule51 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule51 <- sum(Rule51[!is.na(Rule51)])
    
    ##### Rule 52 #####
    if (!all(is.na(od))) {
      Rule52 <- mapply(function(od) {
        if (!is.na(od)) {
          if (od < 0 | od > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, od=od, SIMPLIFY = T)
    } else {
      Rule52 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule52 <- sum(Rule52[!is.na(Rule52)])
    
    ##### Rule 53 #####
    if (!all(is.na(md))) {
      Rule53 <- mapply(function(md) {
        if (!is.na(md)) {
          if (md < 0 | md > 100) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, md=md, SIMPLIFY = T)
    } else {
      Rule53 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule53 <- sum(Rule53[!is.na(Rule53)])
    
    ##### Rule 54 #####
    if (!all(is.na(T.sas_c))) {
      Rule54 <- mapply(function(T.sas_c) {
        if (!is.na(T.sas_c)) {
          if (T.sas_c < 50 | T.sas_c > 70) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.sas_c=T.sas_c, SIMPLIFY = T)
    } else {
      Rule54 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule54 <- sum(Rule54[!is.na(Rule54)])
    
    ##### Rule 55 #####
    if (!all(is.na(T.sas_h))) {
      Rule55 <- mapply(function(T.sas_h) {
        if (!is.na(T.sas_h)) {
          if (T.sas_h < 60 | T.sas_h > 140) {
            1
          } else {
            0
          }
        } else {
          as.numeric(0)
        }
      }, T.sas_h=T.sas_h, SIMPLIFY = T)
    } else {
      Rule55 <- rep(as.numeric(0), length.out = length(df[["Global.OAT"]]))
    }
    
    Rule55 <- sum(Rule55[!is.na(Rule55)])

    extended_mode <- list(Rule29C,Rule29H,Rule31,Rule35,Rule37,Rule40,Rule41,Rule42,
                          Rule43,Rule44,Rule45,Rule46,Rule47,Rule48,Rule49,Rule50,
                          Rule51,Rule52,Rule53,Rule54,Rule55)
    names(extended_mode) <- c("Rule29C","Rule29H","Rule31","Rule35","Rule37","Rule40","Rule41",
                              "Rule42","Rule43","Rule44","Rule45","Rule46","Rule47","Rule48",
                              "Rule49","Rule50","Rule51","Rule52","Rule53","Rule54","Rule55")
    
    return(extended_mode)
    
  }
  
  extended.rules <- mapply(function(OA,df) mapply(extended_rules, OA = OA, df = df, SIMPLIFY = F), OA = pctOAs, df = dfl)
  
  # collapse to one dataframe per building
  xtra.rules.bldg <- mapply(function(x,a,b) {
    x <- rbindlist(x)
    x$Building <- b
    x$AHU <- a
    x <- x[,c(22,23,1:21)]
    return(x)
  }, x = extended.rules, a = a.names, b = b.names, SIMPLIFY = F)
  
  ## count AHUs with faults
  ahus.w.faults <- lapply(xtra.rules.bldg1, function(x) {
    for (i in 1:nrow(x)) {
      x <- as.data.frame(x[-2,])
      y <- ifelse(x[,i] > 0, 1, 0)
      return(y)
    }
  })
  
  # collapse into single dataframe total
  xtra.rules <- rbindlist(xtra.rules.bldg)
  xtra.rules <- as.data.frame(xtra.rules, stringsAsFactors = F)
  
  # matrix of numeric values
  xtra.mat <- aggregate(xtra.rules[,3:23], by=list(Category=xtra.rules$Building), FUN=sum)
  x.names <- xtra.mat$Category
  xtra.mat <- as.matrix(xtra.mat[,c(2:22)])
  row.names(xtra.mat) <- x.names

  # new heatmap
  file0 <- paste0("./outputs/APAR_Fault_Detection/Extended_Ruleset_Heatmap_", Sys.Date(), ".pdf")
  cairo_pdf(filename = file0, width = 15, height = 6)
  
  xlp <- levelplot(xtra.mat,
                  main = "Total Number of AHU Faults Detected per UT Austin Main Campus Building",
                  sub = "15 minute resolution (max. period: Sep. 2017 - Dec. 2018)",
                  ylab = "Extended Rule-Set",
                  xlab = "UT Austin Main Campus Buildings",
                  scales = list(x = list(rot = 90)),
                  aspect = "fill",
                  col.regions = colorRampPalette(c("white", "red", "purple")))
  xlp
  dev.off()
  
  #################### OUTPUTS ####################

  # time periods (number of unique dates)
  Count.Dates <- lapply(dfl, function(x) lapply(x, function(y) length(unique(date(y$Date.Time)))))
  
  # combine outputs
  APAR <- mapply(function(m1,m2,m3,m4,m5,m6) mapply(function(m1,m2,m3,m4,m5,m6) c(m1,m2,m3,m4,m5,m6), m1=m1,m2=m2,m3=m3,m4=m4,m5=m5,m6=m6, SIMPLIFY = F), m1=mode.1,m2=mode.2,m3=mode.3,m4=mode.4,m5=mode.5,m6=mode.6, SIMPLIFY = F)
  
  # Sum faults
  APAR.sums <- lapply(APAR, function(x) lapply(x, function(y) lapply(y, function(z) sum(z, na.rm = T))))
  
  # collapse to one dataframe per building
  APAR.bldglvl <- mapply(function(x,a,b,cd) {
    x <- rbindlist(x)
    cd <- as.numeric(unlist(cd))
    x$Building <- b
    x$AHU <- a
    x$Count.Dates <- cd
    x <- x[,c(29:31,1:28)]
    return(x)
  }, x = APAR.sums, a = a.names, b = b.names, cd = Count.Dates, SIMPLIFY = F)
  
  APAR.bldglvlup <- APAR.bldglvl
  names(APAR.bldglvlup)[names(APAR.bldglvlup) %in% ut_bldg_index$Building] <- sapply(names(APAR.bldglvlup)[names(APAR.bldglvlup) %in% ut_bldg_index$Building], function(x) ut_bldg_index$Bldg.ID[grepl(x, ut_bldg_index$Building)])
  names(APAR.bldglvlup)[names(APAR.bldglvlup) %in% ut_bldg_index$Alt.Abbrev] <- sapply(names(APAR.bldglvlup)[names(APAR.bldglvlup) %in% ut_bldg_index$Alt.Abbrev], function(x) ut_bldg_index$Bldg.ID[grepl(x, ut_bldg_index$Alt.Abbrev)])
  saveRDS(APAR.bldglvlup, "./Shiny-FDD/RDS/APAR_buildings.rds")
  
  # collapse into single dataframe total
  APAR.df <- rbindlist(APAR.bldglvl)
  APAR.df <- as.data.frame(APAR.df, stringsAsFactors = F)
  
  # matrix of numeric values
  APAR.mat <- as.matrix(APAR.df[,c(4:27,30,31)])
  
  # faults per day
  APAR.daily <- APAR.mat / APAR.df$Count.Dates
  
  # add building names
  APAR.daily <- cbind(APAR.df$Building, as.data.frame(APAR.daily))
  colnames(APAR.daily)[1] <- "Building"
  
  # AHUs per building (weights: AHU = 1, others (FCU, MAU, RTU, OAU, etc.) = 0.5)
  Count.AHU <- sapply(a.names, function(x) length(x[grepl(x, pattern = "^AH")]) + 0.5*length(x[!grepl(x, pattern = "^AH")]))
  
  # aggregates (normalized by time length)
  APAR.agg <- aggregate(.~Building, APAR.daily, sum)
  rownames(APAR.agg) <- unique(APAR.daily$Building)
  APAR.agg <- as.matrix(APAR.agg[,-1])
  
  
  #APAR.agg["DCP",] <- APAR.agg["DCP",]/10
  
  # faults standardized by number of AHUs
  APAR.fin <- APAR.agg / Count.AHU
  
  # folder for outputs
  ifelse(!dir.exists("./outputs/APAR_Fault_Detection"), dir.create("./outputs/APAR_Fault_Detection"), FALSE)
  
  ##### Outputs #####
  
  # Rule explanations
  
  Mode1Rules <- c("Inconsistency between supply and mixed air temperatures.",
                  "Fraction of outdoor air entering the AHU is either too high or too low.",
                  "Heating coil valve is saturated at the fully open position and the supply air temperature is less than the set point value.",
                  "Warning that system is out of control.")
  
  Mode2Rules <- c("Outdoor air temperature is too high for this mode.",
                  "Inconsistency between the supply and return air temperatures.",
                  "Inconsistency between the supply and mixed air temperatures.")
  
  Mode3Rules <- c("Outdoor air temperature is too low for this mode.",
                  "Outdoor air temperature is too high for this mode.",
                  "Inconsistency between the outdoor and mixed air temperatures.",
                  "Inconsistency between the supply and mixed air temperatures.",
                  "Inconsistency between the supply and return air temperatures.",
                  "Cooling coil valve is saturated at the fully open position and the supply air temperature is greater than the set point.",
                  "Warning that the system is out of control.")
  
  Mode4Rules <- c("Outdoor air temperature is too low for this mode.",
                  "Inconsistencies between supply and mixed air temperatures.",
                  "Inconsistencies between supply and return air temperatures.",
                  "Fraction of outdoor air entering the AHU is either too high or too low.",
                  "Cooling coil valve is saturated at the fully open position and the supply air temperature is greater than the set point.",
                  "Warning that the system is out of control.")
  
  Mode5Rules <- c("Cooling coil valve, heating coil valve, and mixing box damper are modulating simultaneously.",
                  "Cooling coil valve and heating coil valve are modulating simultaneously.",
                  "Heating coil valve and mixing box damper are modulating simultaneously.",
                  "Cooling coil valve and mixing box damper are modulating simultaneously.")
  
  Mode6Rules <- c("Supply air temperature set point is not being satisfied.",
                  "Inconsistencies in the temp. measurements for the mixed, return, and outdoor air where the mixed air temp. is less than the expected minimum.",
                  "Inconsistencies in the temp. measurements for the mixed, return, and outdoor air where the mixed air temp. is greater than the expected maximum.")
  
  Mode7Rules <- c("Period of operation with excessive mode transitions.")
  
  modes <- c(Mode1Rules, Mode2Rules, Mode3Rules, Mode4Rules, Mode5Rules, Mode6Rules, Mode7Rules)
  rule.list <- paste0("Rule",formatC(seq_along(modes), format = "f", flag = "0", width = 2, digits = 0))
  rule.key <- paste0(rule.list,": ", modes)
  rk <- paste(rule.key, sep = "", collapse = "\n")
  
  rk1 <- paste(strwrap(rule.key[1:15],81), sep = "", collapse = "\n")
  rk2 <- paste(strwrap(rule.key[16:28],81), sep = "", collapse = "\n")
  
  ##### Heatmap #####
  library(ggplot2)
  library(grid)
  library(gridExtra)
  
  if (heatmap_display == "all") {
    
    file.name <- paste0("./outputs/APAR_Fault_Detection/APAR_Aggregate_Heatmap_", Sys.Date(), ".pdf")
    
    #png(file.name, width = 2670, height = 2000, res = 180)
    cairo_pdf(filename = file.name, width = 15, height = 6)
    
    lp <- levelplot(APAR.agg,
                    main = "Aggregate Number of AHU Faults Detected per UT Austin Main Campus Building",
                    sub = "15 minute resolution (max. period: Sep. 2017 - Dec. 2018)",
                    ylab = "Air-Handling Unit Performance Assessment Rules",
                    xlab = "UT Austin Main Campus Buildings",
                    scales = list(x = list(rot = 90)),
                    aspect = "fill",
                    col.regions = colorRampPalette(c("white", "red", "purple")))
    
    lp
    dev.off()
    
  } else {
    
    ## create heatmap of aggregated ahu faults for user input building
    df <- APAR.bldglvl[[heatmap_display]]
    
    # matrix of numeric values
    matrx <- as.matrix(df[,c(4:31)])
    
    # faults per day
    df.day <- matrx / df$Count.Dates
    
    # add AHU names
    row.names(df.day) <- df$AHU
    
    # generate plot info
    start.date <- min(as_datetime(unlist(lapply(dfl[[heatmap_display]], function(x) min(x[["Date.Time"]]))), tz = "America/Chicago"))
    end.date <- max(as_datetime(unlist(lapply(dfl[[heatmap_display]], function(x) max(x[["Date.Time"]]))), tz = "America/Chicago"))
    
    main.title <- paste0("Number of Faults Detected per Day for ", heatmap_display, " Air Handling Units")
    sub.title <- paste0("15 minute resolution (period: ", start.date, " to ", end.date, ")")
    x.lab <- paste0(ut_cfm$Name[ut_cfm$Building == heatmap_display][1], " (", heatmap_display, ") Air Handling Units")
    
    ahu.file <- paste0("./outputs/APAR_Fault_Detection/", heatmap_display,"_APAR_Heatmap_", Sys.Date(), ".png")
    
    png(ahu.file, width = 2670, height = 1010,  res = 180)
    
    lp <- levelplot(df.day,
                    main = main.title,
                    sub = sub.title,
                    ylab = "Air Handling Unit Performance Assessment Rules",
                    xlab = x.lab,
                    scales = list(x = list(rot = 90)),
                    aspect = "fill",
                    col.regions = colorRampPalette(c("white", "red", "purple")))

    dev.off()
  }

} #################### END ####################
