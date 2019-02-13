# Megan K. McHugh, Masters in Engineering (Sustainable Systems) Thesis Research, UT Austin 2018
# Intelligent Environments Lab & UT Facilities Services Energy Management Optimization
# Fault Detection of Air Handling Unit Chilled Water and Steam Valve Leakage
# Part 4: Shiny Web Application

##### LOAD LIBRARIES & DATA #####
library(shiny)
library(shinydashboard)
library(lattice)
library(lubridate)
library(DT)
library(leaflet)
library(leaflet.minicharts)
library(data.table)
library(zoo)
library(xts)

bldg.names <- readRDS("./RDS/analysis_building_names.rds")
b.names <- readRDS("./RDS/b_names.rds")
a.names <- readRDS("./RDS/a_names.rds")

## naming vectors
ut_bldg_index <- readRDS("./RDS/ut_bldg_index.rds")
ut_ref <- readRDS("./RDS/ut_cfm_and_occupancy.rds")

chwleak.bldgs <- readRDS("./RDS/chwleak_bldgs.rds")
stmleak.bldgs <- readRDS("./RDS/stmleak_bldgs.rds")

# UT master reference guide sorting
ref.bldgs <- c("All Buildings",sort(unique(c(unique(ut_ref$Building),unique(ut_ref$Alt.Abbrev))))[-1])
names(ref.bldgs) <- ref.bldgs

labeled.bldgs <- c("All Buildings" = "all", "ADH" = "0347", "AND" = "0009", "ARC" = "0135", "ART" = "0049", "BAT" = "0073", "BEL" = "0418", "BEN" = "0081", "BHD" = "0105", "BIO" = "0089", "BLD" = "0098", "BMA" = "0114", "BMC" = "0185", "BMK" = "0117", "BMS (EAS)" = "0115", "BRB" = "0457", "BTL" = "0065", "BUR" = "0099", "CAL" = "0954", "CBA" = "0107", "CBC" = "CBC", "CCJ" = "0603", "CMA" = "0180", "CMB" = "0182", "COM" = "0178", "CPE" = "0231", "CRB" = "0160", "CRD" = "0129", "CS7" = "0143", "DCP" = "0451", "DFA" = "0130", "DPI" = "3355", "ECJ" = "0227", "EER" = "0223", "EME" = "EME", "EPS" = "0153", "ERC (SRC)" = "0450", "ETC" = "0230", "FAC" = "0605", "FC1" = "0550", "FC3" = "0552", "FNT (NST)" = "0242", "GAR" = "0249", "GDC" = "0152", "GEA" = "0305", "GEO" = "GEO", "GOL" = "0017", "GRE" = "0273", "GSB" = "0106", "GWB" = "0645", "HDB" = "0700", "HMA" = "0297", "HRC" = "0310", "HRH" = "0433", "HSM (CMC)" = "0183", "HTB" = "0701", "IPF" = "9714", "JES (JCD)" = "0598", "JON" = "0602", "KIN" = "0346", "LBJ" = "0113", "MAI" = "0393", "MBB" = "0740", "MEZ" = "0425", "MHD" = "0427", "MNC" = "0421", "MRH" = "0131", "MTC" = "0235", "NEZ" = "9716", "NHB" = "0470", "NMS" = "0741", "NUR" = "8008", "PAC" = "0132", "PAI" = "0473", "PAR" = "0233", "PAT" = "0952", "PCL" = "0559", "PG5" = "PG5", "PHD" = "0497", "PHO" = "O465", "PHR" = "0465", "POB (ACES)" = "0224", "PPE" = "0448", "RHD" = "0537", "RLM" = "0116", "RRH (RBRH)" = "0108", "RSC" = "0422", "SEA" = "0985", "SER" = "0561", "SJH" = "0502", "SRH" = "0118", "SSB" = "0980", "STD (STA)" = "9710", "SUT" = "0585", "SZB" = "0025", "TMM" = "0593", "TNH" = "0601", "TSC" = "0419", "UIL" = "0164", "UNB" = "0609", "UTA" = "0030", "UTC" = "0500", "WAG" = "0649", "WCH" = "0257", "WEL" = "0161", "WIN" = "0201", "WMB" = "0652", "WWH" = "0040")

ahu.names <- readRDS("./RDS/ahu_namesID.rds")

## STM detailed leakage calculations

stm_calcs <- readRDS("./RDS/stm_detailed.rds")
stm_calcs <- stm_calcs[order(names(stm_calcs))]

dfl.lengths <- lapply(stm_calcs, function(x) lapply(x, nrow))

stm.calcs <- mapply(function(x,a,b,df.len) mapply(function(x,a,b,df.len) {
  x$Building <- rep(b, df.len)
  x$AHU <- rep(a, df.len)
  x$Date.Time <- strftime(x$Date.Time, format = '%Y-%m-%d %R', usetz = TRUE)
  x <- x[,c(12,13,1:11)]
  return(x)
}, x=x, a=a, b=b, df.len=df.len, SIMPLIFY = F), x = stm_calcs, a = a.names, b = b.names, df.len = dfl.lengths, SIMPLIFY = F)

for (i in 1:length(stm.calcs)) {
  names(stm.calcs)[[i]] <- labeled.bldgs[grepl(names(stm.calcs)[[i]], names(labeled.bldgs))]
}

stm.detailed <- mapply(function(x,a,b,df.len) mapply(function(x,a,b,df.len) {
  x$Building <- rep(b, df.len)
  x$AHU <- rep(a, df.len)
  x <- x[,c(12,13,1:11)]
  return(x)
}, x=x, a=a, b=b, df.len=df.len, SIMPLIFY = F), x = stm_calcs, a = a.names, b = b.names, df.len = dfl.lengths, SIMPLIFY = F)

stm.calc.bldg <- lapply(stm.detailed, rbindlist)
stm.calc.bldg <- lapply(stm.calc.bldg, as.data.frame, stringsAsFactors = FALSE)

## CHW detailed leakage calculations

chw_calcs <- readRDS("./RDS/chw_detailed.rds")
chw_calcs <- chw_calcs[order(names(chw_calcs))]

dfl.lengths <- lapply(chw_calcs, function(x) lapply(x, nrow))

chw.calcs <- mapply(function(x,a,b,df.len) mapply(function(x,a,b,df.len) {
  x$Building <- rep(b, df.len)
  x$AHU <- rep(a, df.len)
  x$Date.Time <- strftime(x$Date.Time, format = '%Y-%m-%d %R', usetz = TRUE)
  x <- x[,c(18,19,1:17)]
  return(x)
}, x=x, a=a, b=b, df.len=df.len, SIMPLIFY = F), x = chw_calcs, a = a.names, b = b.names, df.len = dfl.lengths, SIMPLIFY = F)

for (i in 1:length(chw.calcs)) {
  names(chw.calcs)[[i]] <- labeled.bldgs[grepl(names(chw.calcs)[[i]], names(labeled.bldgs))]
}

chw.detailed <- mapply(function(x,a,b,df.len) mapply(function(x,a,b,df.len) {
  x$Building <- rep(b, df.len)
  x$AHU <- rep(a, df.len)
  x <- x[,c(18,19,1:17)]
  return(x)
}, x=x, a=a, b=b, df.len=df.len, SIMPLIFY = F), x = chw_calcs, a = a.names, b = b.names, df.len = dfl.lengths, SIMPLIFY = F)

chw.calc.bldg <- lapply(chw.detailed, rbindlist)
chw.calc.bldg <- lapply(chw.calc.bldg, as.data.frame, stringsAsFactors = FALSE)

## Leakage Summary Dataframes
chw.leak.sum <- readRDS("./RDS/chw_leak_sum.rds")
stm.leak.sum <- readRDS("./RDS/stm_leak_sum.rds")

## APAR Matrix
APAR.agg <- readRDS("./RDS/APAR_aggregate_matrix.rds")
APAR.fin <- as.data.frame(APAR.agg)
APAR.rownames <- row.names(APAR.fin)

APAR.fin <- sapply(APAR.fin, function(x) round(x, digits = 2))
APAR.fin <- as.data.frame(APAR.fin)

APAR.fin$Building <- APAR.rownames
APAR.fin <- APAR.fin[,c(27,1:26)]

APAR.bldglvl <- readRDS("./RDS/APAR_buildings.rds")
APAR.dfl <- readRDS("./RDS/APAR_dfl.rds")

## Make new dataframes for easy leaflet mapping
chw.leaf <- list()
chw.sum.bldgs <- unique(chw.leak.sum$Building)
for (i in 1:length(chw.sum.bldgs)) {
  chw.leaf[[i]] <- chw.calc.bldg[[chw.sum.bldgs[i]]][c('Building','AHU','Date.Time','CHW.Vlv.Leakage')]
}
names(chw.leaf) <- chw.sum.bldgs

for (i in 1:length(chw.leaf)) {
  names(chw.leaf)[[i]] <- labeled.bldgs[grepl(names(chw.leaf)[[i]], names(labeled.bldgs))]
}

stm.leaf <- list()
stm.sum.bldgs <- unique(stm.leak.sum$Building)
for (i in 1:length(stm.sum.bldgs)) {
  stm.leaf[[i]] <- stm.calc.bldg[[stm.sum.bldgs[i]]][c('Building','AHU','Date.Time','STM.Vlv.Leakage')]
}
names(stm.leaf) <- stm.sum.bldgs

for (i in 1:length(stm.leaf)) {
  names(stm.leaf)[[i]] <- labeled.bldgs[grepl(names(stm.leaf)[[i]], names(labeled.bldgs))]
}

##### POPULATE DASHBOARD #####

########## SIDERBAR INPUTS ##########
sidebar <- dashboardSidebar(
  hr(),
  sidebarMenu(id="tabs",
              menuItem("Plot", tabName = "plot", icon = icon("line-chart"),
                       menuSubItem("APAR", tabName = "aparplot", icon = icon("angle-right")),
                       menuSubItem("Leakage Summary Map", tabName = "leaksumplot", icon = icon("angle-right")),
                       menuSubItem("Leakage Details Plot", tabName = "leakcalcplot", icon = icon("angle-right")),
                       menuSubItem("X/Y Inputs Plot", tabName = "xyinputplot", icon = icon("angle-right")),
                       selected = TRUE),
              menuItem("Table", tabName = "tab", icon = icon("table"),
                       menuSubItem("APAR Matrix", tabName = "apartab", icon = icon("angle-right")),
                       menuSubItem("Leakage Summary", tabName = "leaksumtab", icon = icon("angle-right")),
                       menuSubItem("Detailed Leakage Calcs", tabName = "leakcalctab", icon = icon("angle-right"))
              ),
              menuItem("Download Data", tabName = "downloads",  icon = icon("file-text-o"),
                       menuSubItem("BAS Raw Data", tabName = "raw", icon = icon("angle-right")),
                       menuSubItem("UT AHU Schematics", tabName = "scheme", icon = icon("angle-right")),
                       menuSubItem("UT AHU Reference Data", tabName = "ref", icon = icon("angle-right"))
              ) #, 
              # menuItem("ReadMe", tabName = "readme", icon=icon("mortar-board")),
              # menuItem("About", tabName = "about", icon = icon("question"))
  ),
  hr(),
  ##### BAS Raw Data #####
  conditionalPanel("input.tabs=='raw'",
                   fluidRow(
                     column(1),
                     column(10,
                            selectInput("whatraw", "Building:",
                                        labeled.bldgs),
                            htmlOutput("selahuRaw")
                     )
                   )
  ),
  ##### UT AHU Schematics #####
  conditionalPanel("input.tabs=='scheme'",
                   fluidRow(
                     column(1),
                     column(10,
                            selectInput("whatscheme", "Building:",
                                        labeled.bldgs),
                            htmlOutput("selahuScheme")
                     )
                   )
  ),
  ##### APAR Plots #####
  conditionalPanel("input.tabs=='aparplot'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whichaparplot", "Display:", 
                                         c("Heatmap" = "aparheat",
                                           "Frequency" = "aparfreq")),
                            checkboxInput("aparandover", "Scrape Andover?"),
                            checkboxInput("aparsiemens", "Scrape Siemens?"),
                            selectInput("whataparplot", "Building:",
                                        labeled.bldgs)
                     )
                   )
  ),
  ##### APAR Table #####
  conditionalPanel("input.tabs=='apartab'",
                   fluidRow(
                     column(1),
                     column(10,
                            selectInput("whatapartab", "Building:",
                                        labeled.bldgs)
                     )
                   )
  ),
  ##### Leakage Summary Map #####
  conditionalPanel("input.tabs=='leaksumplot'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whichleaksumplot", "AHU Valves:", 
                                         c("Chilled Water" = "chwleaksumplot",
                                           "Steam" = "stmleaksumplot"),
                                         selected = "chwleaksumplot")
                     )
                   )
  ),
  ##### Leakage Summary Table #####
  conditionalPanel("input.tabs=='leaksumtab'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whichleaksumtab", "AHU Valves:", 
                                         c("Chilled Water" = "chwleaksumtab",
                                           "Steam" = "stmleaksumtab"),
                                         selected = "chwleaksumtab")
                     )
                   )
  ),
  ##### Leakage Calcs Plot #####
  conditionalPanel("input.tabs=='leakcalcplot'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whichleakcalcplot", "AHU Valves:", 
                                         c("Chilled Water" = "chwleakcalcplot",
                                           "Steam" = "stmleakcalcplot"),
                                         selected = "chwleakcalcplot"),
                            selectInput("whatleakcalcplot", "Building:",
                                        labeled.bldgs),
                            htmlOutput("selahuLeakPlot")
                     )
                   )
  ),
  ##### Leakage Calcs Table #####
  conditionalPanel("input.tabs=='leakcalctab'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whichleakcalctab", "AHU Valves:", 
                                         c("Chilled Water" = "chwleakcalctab",
                                           "Steam" = "stmleakcalctab"),
                                         selected = "chwleakcalctab"),
                            selectInput("whatleakcalctab", "Building:",
                                        labeled.bldgs[-1]),
                            htmlOutput("selahuLeakTab")
                     )
                   )
  ),
  ##### X/Y Inputs Plot #####
  conditionalPanel("input.tabs=='xyinputplot'",
                   fluidRow(
                     column(1),
                     column(10,
                            radioButtons("whatxyplot", "Display:", 
                                         c("APAR" = "apar",
                                           "Leakage Summary Map" = "leakmap",
                                           "Leakage Details Plot" = "leakplot",
                                           "X/Y Input Plots" = "makeplot")))
                   )
  )
)

########## BODY INPUTS ##########
body <- dashboardBody(
  tabItems(
    tabItem(tabName = "aparplot",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "APAR Fault Detection Heatmap",                
                 column(width = 12,
                        plotOutput("APARheatmap", height="500px")),
                 br(),br(),
                 downloadButton('downloadAPARheatmap', 'Download')
            )
    ),
    tabItem(tabName = "apartab",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "APAR Fault Detection Matrix",                
                 column(width = 12,
                        DT::dataTableOutput("APARmatrix"),
                        style = "height:500px; overflow-y: scroll; overflow-x: scroll;"),
                 br(),br(),
                 downloadButton('downloadAPARmatrix', 'Download')
            )
    ),
    tabItem(tabName = "leaksumplot",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "UT Austin Campus AHU Valve Leakage Map",              
                 column(width = 12,
                        leafletOutput("LeakageMap", height = "650px")),
                 br(),br(),
                 dateRangeInput(inputId = "mapdaterange", label = "Date range:", start = minmin, end = maxmax),
                 downloadButton('downloadLeakMap', 'Download')
            )
    ),
    tabItem(tabName = "leaksumtab",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "UT Austin AHU Valve Leakage Summary",                
                 column(width = 12,
                        DT::dataTableOutput("LeakSumTab"),
                        style = "height:800px; overflow-y: scroll; overflow-x: scroll;"),
                 br(),br(),
                 downloadButton('downloadLeakSumTab', 'Download')
            )
    ),
    tabItem(tabName = "leakcalctab",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "UT Austin AHU Valve Leakage Calculations",                
                 column(width = 12,
                        DT::dataTableOutput("LeakDetailsTab"),
                        style = "height:800px; overflow-y: scroll; overflow-x: scroll;"),
                 br(),br(),
                 downloadButton('downloadLeakCalcTab', 'Download')
            )
    ),
    tabItem(tabName = "ref",
            box( width = NULL, status = "primary", solidHeader = TRUE, title = "UT Austin Building and AHU Reference Data",                
                 column(width = 12,
                        DT::dataTableOutput("Reference"),
                        style = "height:800px; overflow-y: scroll; overflow-x: scroll;"),
                 br(),br(),
                 downloadButton('downloadRef', 'Download')
            )
    )
  )
)

############### UI ############### 
ui <- dashboardPage(dashboardHeader(title = "UT Austin FDD"),
                    sidebar = sidebar,
                    body = body)

############### SERVER ###############

server <- function(input, output) {
  
  ########## SIDEBAR OUTPUTS ##########
  
  ##### Leakage Calc Plot #####
  
  # Select AHU
  selahuLeakPlot <- reactive({
    if (input$whatleakcalcplot != "all") {
      ahu.names[[input$whatleakcalcplot]]
    }
  })
  output$selahuLeakPlot <- renderUI({
    selectInput("selLeakPlot", "Air Handling Unit:", selahuLeakPlot())
  })
  
  # Select Date Range
  seldateLeakPlot <- reactive({
    if (input$whatleakcalcplot != "all") {
      
    } else {
      
    }
  })
  
  ##### Leakage Calc Table #####
  
  # Select AHU
  selahuLeakTab <- reactive({
    if (input$whatleakcalctab != "all") {
      ahu.names[[input$whatleakcalctab]][-1]
    }
  })
  output$selahuLeakTab <- renderUI({
    selectInput("selLeakTab", "Air Handling Unit:", selahuLeakTab())
  })
  
  ##### Download Schematics #####
  
  # Select AHU
  selahuScheme <- reactive({
    if (input$whatscheme != "all"){
      ahu.names[[input$whatscheme]]
    }
  })
  output$selahuScheme <- renderUI({
    selectInput("selscheme", "Air Handling Unit:", selahuScheme())
  })
  
  ##### Download Reference Table #####
  
  # Select AHU
  selRef <- reactive({
    whatrefs <- c(input$whatref)
    ut_ref$AHU[ut_ref$Bldg.ID %in% whatrefs]
  })
  output$selRef <- renderUI({
    selectInput("selrefahu", "Air Handling Unit:", selRef())
  })
  
  ##### Download Raw Data #####
  
  # Select AHU
  selahuRaw <- reactive({
    if (input$whatraw != "all"){
      ahu.names[[input$whatraw]]
    }
  })
  output$selahuRaw <- renderUI({
    selectInput("selraw", "Air Handling Unit:", selahuRaw())
  })
  
  ########## BODY OUTPUTS ##########
  
  ##### APAR Heatmap #####
  output$APARheatmap <- renderPlot({
    if (input$whichaparplot == "aparheat") {
      if (input$whataparplot == "all") {
        
        levelplot(APAR.agg,
                  main = "Aggregate Number of AHU Faults Detected per UT Austin Main Campus Building",
                  sub = "15 minute resolution (max. period: Sep. 2017 - Oct. 2018)",
                  ylab = "Air-Handling Unit Performance Assessment Rules",
                  xlab = "UT Austin Main Campus Buildings",
                  scales = list(x = list(rot = 90)),
                  aspect = "fill",
                  col.regions = colorRampPalette(c("white", "red", "purple")))
      } else {
        
        ## create heatmap of aggregated ahu faults for user input building
        df <- APAR.bldglvl[[input$whataparplot]]
        
        # matrix of numeric values
        matrx <- as.matrix(df[,c(4:31)])
        
        # faults per day
        df.day <- matrx / df$Count.Dates
        
        # add AHU names
        row.names(df.day) <- df$AHU
        
        # generate plot info
        start.date <- min(as_datetime(unlist(lapply(APAR.dfl[[input$whataparplot]], function(x) min(x[["Date.Time"]]))), tz = "America/Chicago"))
        end.date <- max(as_datetime(unlist(lapply(APAR.dfl[[input$whataparplot]], function(x) max(x[["Date.Time"]]))), tz = "America/Chicago"))
        
        bldg <- names(labeled.bldgs[labeled.bldgs == input$whataparplot])
        main.title <- paste0("Number of Faults Detected per Day for ", bldg, " Air Handling Units")
        sub.title <- paste0("15 minute resolution (period: ", start.date, " to ", end.date, ")")
        x.lab <- paste0(ut_bldg_index$Name[ut_bldg_index$Bldg.ID == input$whataparplot][1], " (", bldg, ") Air Handling Units")
        
        if (nrow(df) <= 20) {
          levelplot(df.day,
                    main = main.title,
                    sub = sub.title,
                    ylab = "Air Handling Unit Performance Assessment Rules",
                    xlab = x.lab,
                    aspect = "fill",
                    col.regions = colorRampPalette(c("white", "red", "purple")))
        } else {
          levelplot(df.day,
                    main = main.title,
                    sub = sub.title,
                    ylab = "Air Handling Unit Performance Assessment Rules",
                    xlab = x.lab,
                    scales = list(x = list(rot = 90)),
                    aspect = "fill",
                    col.regions = colorRampPalette(c("white", "red", "purple")))
        }
        
      }
    }
  })
  
  ##### APAR Matrix #####
  output$APARmatrix <- renderDataTable({
    if (input$whatapartab == "all") {
      
      datatable(APAR.fin, options = list(paging = FALSE), rownames = FALSE, caption = 'APAR Faults per Day for Building Aggregated AHUs')
      
    } else {
      
      df <- APAR.bldglvl[[input$whatapartab]]
      df <- df[,c(2:31)]
      names(df)[2] <- "Period.Days"
      
      datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'Total APAR Faults per AHU')
    }
  })
  
  
  ##### APAR Frequency #####
  
  ##### Leakage Summary Map #####
  output$LeakageMap <- renderLeaflet({
    if (input$whichleaksumplot == "chwleaksumplot") {
      
      chw.sub <- readRDS("./RDS/chw_leaf.rds")
      
      chw.map <- leaflet() %>%
        addTiles()
      
      # chilled water interactive map
      chw.map %>%
        addMinicharts(
          chw.sub$Longitude, chw.sub$Latitude,
          chartdata = chw.sub$Leakage.ton_hr,
          showLabels = TRUE,
          width = 45,
          popup = popupArgs(
            labels = chw.sub$Building,
            showTitle = F,
            showValues = F,
            html = chw.sub$Labels
          )
        )
      
    } else if (input$whichleaksumplot == "stmleaksumplot") {
      
      stm.sub <- readRDS("./RDS/stm_leaf.rds")
      
      stm.map <- leaflet() %>%
        addTiles()
      
      # steam interactive plot
      stm.map %>%
        addMinicharts(
          stm.sub$Longitude, stm.sub$Latitude,
          chartdata = stm.sub$Leakage.lbs,
          showLabels = TRUE,
          width = 45,
          fillColor = "#f95562",
          popup = popupArgs(
            labels = stm.sub$Building,
            showTitle = F,
            showValues = F,
            html = stm.sub$Labels
          )
        )
      
    }
  })
  
  ##### Leakage Summary Table #####
  output$LeakSumTab <- renderDataTable({
    if (input$whichleaksumtab == "chwleaksumtab") {
      
      chw.sum.cols <- c('Building','AHU','Days','Total Leakage (ton-hr)','Daily Leakage (ton-hr)','Annual Leakage (ton-hr)','Valve Closed %','Avg. Leakage Valve Closed','Max. TempF Valve Closed','Days/Year Valve Closed','Fully Burdened Cost/Day','Fuel Only Cost/Day','Fully Burdened Cost/Year','Fuel Only Cost/Year')
      
      datatable(chw.leak.sum, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Chilled Water Valve Leakage', colnames = chw.sum.cols)
      
    } else if (input$whichleaksumtab == "stmleaksumtab") {
      
      stm.sum.cols <- c('Building','AHU','Days','Total Leakage (lbs)','Daily Leakage (lbs)','Annual Leakage (lbs)','Valve Closed %','Avg. Leakage Valve Closed','Min. TempF Valve Closed','Days/Year Valve Closed','Fully Burdened Cost/Day','Fuel Only Cost/Day','Fully Burdened Cost/Year','Fuel Only Cost/Year')
      
      datatable(stm.leak.sum, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Steam Valve Leakage', colnames = stm.sum.cols)
      
    }
  })
  
  ##### Leakage Details Table #####
  output$LeakDetailsTab <- renderDataTable({
    if (input$whichleakcalctab == "chwleakcalctab") {
      
      chw.sum.cols <- c('Building','AHU','Date/Time','CD Fan Ctl','CD CFM actual','Mixed Air TempF','Mixed Air RH','Mixed Air Hum. Ratio','Mixed Air Enthalpy','CD Supply Air TempF','CD Supply Air RH','CD Supply Air Hum. Ratio','CD Supply Air Enthalpy','CD Set Point TempF','CD Set Point Enthalpy','Cooling BTU','Cooling ton-hr','CHW Valve Ctl','CHW Valve Leakage (ton-hr)')
      
      df <- chw.calcs[[input$whatleakcalctab]][[input$selLeakTab]]
      datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Chilled Water Valve Leakage', colnames = chw.sum.cols)
      
    } else if (input$whichleakcalctab == "stmleakcalctab") {
      
      stm.sum.cols <- c('Building','AHU','Date/Time','HD Fan Ctl','HD CFM actual','Mixed Air TempF','Mixed Air RH','HD Supply Air TempF','HD Set Point TempF','Heating BTU','Heating lbs','STM Valve Ctl','STM Valve Leakage (lbs)')
      
      df <- stm.calcs[[input$whatleakcalctab]][[input$selLeakTab]]
      datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Steam Valve Leakage', colnames = stm.sum.cols)
      
    }
  })
  
  ##### UT Reference Spreadsheet #####
  output$Reference <- renderDataTable({
    
    ut.ref <- ut_ref
    char_cols <- lapply(ut.ref, class) == "character"
    ut.ref[,char_cols] <- lapply(ut.ref[,char_cols], as.factor)
    
    datatable(ut.ref, rownames = FALSE, filter = 'top')
    
  })
  
  ##### BAS Raw Data #####
  output$BAS <- renderDataTable({
    if (input$whichleakcalctab == "chwleakcalctab") {
      
      bas.raw <- readRDS("./RDS/BAS_formatted_data.rds")
      bas.raw <- bas.raw[order(names(bas.raw))]
      
      dfl.rows <- lapply(bas.raw, function(x) lapply(x, nrow))
      dfl.cols <- lapply(bas.raw, function(x) lapply(x, ncol))
      
      bas_raw <- mapply(function(x,a,b,df.len,df.col) mapply(function(x,a,b,df.len,df.col) {
        x$Building <- rep(b, df.len)
        x$AHU <- rep(a, df.len)
        c1 <- df.col+1
        c2 <- df.col+2
        x <- x[,c(c1,c2,1:df.col)]
        return(x)
      }, x=x, a=a, b=b, df.len=df.len, df.col=df.col, SIMPLIFY = F), x = bas.raw, a = a.names, b = b.names, df.len = dfl.rows, df.col = dfl.cols, SIMPLIFY = F)
      
      for (i in 1:length(bas_raw)) {
        names(bas_raw)[[i]] <- labeled.bldgs[grepl(names(bas_raw)[[i]], names(labeled.bldgs))]
      }
      
      if (input$selLeakTab != "all") {
        df <- chw.calcs[[input$whatleakcalctab]][[input$selLeakTab]]
        datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Chilled Water Valve Leakage')
      } else {
        chw.calc.bldg <- lapply(chw.calcs, rbindlist)
        df <- chw.calc.bldg[[input$whatleakcalctab]]
        datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Chilled Water Valve Leakage')
      }
      
    } else if (input$whichleakcalctab == "stmleakcalctab") {
      
      stm_calcs <- readRDS("./RDS/stm_detailed.rds")
      stm_calcs <- stm_calcs[order(names(stm_calcs))]
      
      b.names <- readRDS("./RDS/b_names.rds")
      a.names <- readRDS("./RDS/a_names.rds")
      dfl.lengths <- lapply(stm_calcs, function(x) lapply(x, nrow))
      
      stm.calcs <- mapply(function(x,a,b,df.len) mapply(function(x,a,b,df.len) {
        x$Building <- rep(b, df.len)
        x$AHU <- rep(a, df.len)
        x <- x[,c(18,19,1:17)]
        return(x)
      }, x=x, a=a, b=b, df.len=df.len, SIMPLIFY = F), x = stm_calcs, a = a.names, b = b.names, df.len = dfl.lengths, SIMPLIFY = F)
      
      for (i in 1:length(stm.calcs)) {
        names(stm.calcs)[[i]] <- labeled.bldgs[grepl(names(stm.calcs)[[i]], names(labeled.bldgs))]
      }
      
      if (input$selLeakTab != "all") {
        df <- stm.calcs[[input$whatleakcalctab]][[input$selLeakTab]]
        datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Steam Valve Leakage')
      } else {
        stm.calc.bldg <- lapply(stm.calcs, rbindlist)
        df <- stm.calc.bldg[[input$whatleakcalctab]]
        datatable(df, options = list(paging = FALSE), rownames = FALSE, caption = 'AHU Steam Valve Leakage')
      }
      
    }
  })
  
}

##### RUN SHINY APP #####
shinyApp(ui, server)




