# Megan McHugh, Masters Thesis Research, 2018
# Intelligent Environments Lab & UT Facilities Services Energy Management Optimization
# Automatic Fault Detection of Air Handling Unit Chilled Water and Steam Valve Leakage
# Part 2: Mapping AHU Valve Leakage

#################### DEFINE FUNCTION ####################

AHU_Leakage_Map <- function(static_map = c("chw", "stm", "both"), interactive_map = c("chw", "stm")) {
  
  library(ggmap)
  library(ggplot2)
  library(ggrepel)
  library(leaflet)
  library(leaflet.minicharts)
  library(magrittr)
  library(scales)
  
  # specify map boundaries
  lat <- c(30.2695, 30.2965) # from osm. Original: lat <- c(30.274757, 30.293154)
  lon <- c(-97.7553, -97.7085) # from osm. Original: lon <- c(-97.744580, -97.721745)

  ut_index <- readRDS(file = "./Shiny-FDD/RDS/ut_cfm_and_occupancy.rds")
  ut_sub_index <- unique(ut_index[,c(1,4,14,15,19)])
  ut_sub_index <- ut_sub_index[!with(ut_sub_index, is.na(ut_sub_index["Latitude"]) & is.na(ut_sub_index["Longitude"])),]
  
  chw.leak <- readRDS("./Shiny-FDD/RDS/chw_leak_sum.rds")
  stm.leak <- readRDS("./Shiny-FDD/RDS/stm_leak_sum.rds")
  
  for (i in 1:nrow(chw.leak)) {
    b <- chw.leak$Building[i]
    chw.leak$Bldg.ID[i] <-  ut_sub_index$Bldg.ID[ut_sub_index$Building == b | ut_sub_index$Alt.Abbrev == b ]
  }
  
  for (i in 1:nrow(stm.leak)) {
    b <- stm.leak$Building[i]
    stm.leak$Bldg.ID[i] <-  ut_sub_index$Bldg.ID[ut_sub_index$Building == b | ut_sub_index$Alt.Abbrev == b ]
  }
  
  chw.bldgs <- as.data.frame(cbind(unique(chw.leak$Building), unique(chw.leak$Bldg.ID)), stringsAsFactors = F)
  colnames(chw.bldgs) <- c("Building","Bldg.ID")
  
  stm.bldgs <- as.data.frame(cbind(unique(stm.leak$Building), unique(stm.leak$Bldg.ID)), stringsAsFactors = F)
  colnames(stm.bldgs) <- c("Building","Bldg.ID")

  for (i in 1:nrow(stm.bldgs)) {
    b <- stm.bldgs[i,1]
    stm.bldgs$Leakage.lbs[i] <- sum(stm.leak$Total.STM.Vlv.lbs[stm.leak$Building == b])
  }
  
  for (i in 1:nrow(chw.bldgs)) {
    b <- chw.bldgs[i,1]
    chw.bldgs$Leakage.ton_hr[i] <- sum(chw.leak$Total.CHW.Vlv.ton_hr[chw.leak$Building == b])
  }
  
  # merge total leakages and coordinates list
  stm.sub <- merge(stm.bldgs, ut_sub_index, by = c("Building","Bldg.ID"))
  chw.sub <- merge(chw.bldgs, ut_sub_index, by = c("Building","Bldg.ID"))

  ##### GENERATE MAP #####
  
  map <- readRDS("./inputs/stamen_UT_map.rds")
  
  # folder for map outputs
  ifelse(!dir.exists("./outputs/AHU_Leakage_Maps"), dir.create("./outputs/AHU_Leakage_Maps"), FALSE)
  
  ########## ggmap static maps ########## 
  if (grepl(pattern = "chw", static_map, ignore.case = T)) {
    
    # chilled water leakage map
    chw.high <- signif(max(chw.sub$Leakage.ton_hr), digits = 1) + 1000
    chw.br <- chw.high/4
    
    chw.map <- ggmap(map) + 
      
      geom_point(data = chw.sub,
                 aes(x = Longitude, y = Latitude, 
                     size = Leakage.ton_hr, 
                     color = Leakage.ton_hr)) + 
      
      scale_color_gradient(low = "#56b1f7", high = "#132b43", guide = "legend", 
                           limits = c(0, chw.high), breaks = seq(0, chw.high, by = chw.br),
                           labels = comma) +
      
      scale_fill_gradient(low = "#56b1f7", high = "#132b43") +
      
      guides(color = guide_legend(), size = guide_legend()) +
      
      scale_size_continuous(limits = c(0, chw.high), 
                            breaks = seq(0, chw.high, by = chw.br), 
                            labels = comma) +
      
      labs(title = "The University of Texas at Austin", 
           subtitle = "Total AHU Chilled Water Valve Leakage (ton-hr) per Building", 
           size = "CHW Leakage \n(ton-hr)",
           color = "CHW Leakage \n(ton-hr)") + 
      
      geom_label_repel(aes(x = Longitude, y = Latitude, label = Building), data = chw.sub) +
      
      theme(legend.justification = c(1,0), 
            legend.position = c(0.95, 0.05), 
            legend.key = element_blank(), 
            legend.background = element_rect(fill = "#EBEBEB", size = 0.5, 
                                             linetype = "solid", color = "black"),
            legend.title = element_text(face = "bold"),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
    
    chw.map.name <- paste0("./outputs/AHU_Leakage_Maps/CHW_Leakage_Map_", Sys.Date(), ".pdf")
    cairo_pdf(chw.map.name, width = 6, height = 6)
    chw.map
    dev.off()
    
  } else if (grepl(pattern = "stm", static_map, ignore.case = T)) {
    
    # steam leakage map
    stm.high <- signif(max(stm.sub$Leakage.lbs), digits = 2) + 10000
    stm.br <- stm.high/4
    
    stm.map <- ggmap(map) + 
      
      geom_point(data = stm.sub,
                 aes(x = Longitude, y = Latitude, 
                     size = Leakage.lbs, 
                     color = Leakage.lbs)) + 
      
      scale_color_gradient(low = "#f95562", high = "#43121a", guide = "legend", 
                           limits = c(0, stm.high), breaks = seq(0, stm.high, by = stm.br),
                           labels = comma) +
      
      scale_fill_gradient(low = "#f95562", high = "#43121a") +
      
      guides(color = guide_legend(), size = guide_legend()) +
      
      scale_size_continuous(limits = c(0, stm.high), 
                            breaks = seq(0, stm.high, by = stm.br), 
                            labels = comma) +
      
      labs(title = "The University of Texas at Austin", 
           subtitle = "Total AHU Steam Valve Leakage (lbs) per Building", 
           size = "STM Leakage \n(lbs)",
           color = "STM Leakage \n(lbs)") + 
      
      geom_label_repel(aes(x = Longitude, y = Latitude, label = Building), data = stm.sub) +
      
      theme(legend.justification = c(1,0), 
            legend.position = c(0.95, 0.05), 
            legend.key = element_blank(), 
            legend.background = element_rect(fill = "#EBEBEB", size = 0.5, 
                                             linetype = "solid", color = "black"),
            legend.title = element_text(face = "bold"),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
    
    stm.map.name <- paste0("./outputs/AHU_Leakage_Maps/STM_Leakage_Map_", Sys.Date(), ".pdf")
    cairo_pdf(stm.map.name, width = 6, height = 6)
    stm.map
    dev.off()
    
  } else {
    
    # chilled water leakage map
    chw.high <- signif(max(chw.sub$Leakage.ton_hr), digits = 1) + 1000
    chw.br <- chw.high/4
    
    chw.map <- ggmap(map) + 
      
      geom_point(data = chw.sub,
                 aes(x = Longitude, y = Latitude, 
                     size = Leakage.ton_hr, 
                     color = Leakage.ton_hr)) + 
      
      scale_color_gradient(low = "#56b1f7", high = "#132b43", guide = "legend", 
                           limits = c(0, chw.high), breaks = seq(0, chw.high, by = chw.br),
                           labels = comma) +
      
      scale_fill_gradient(low = "#56b1f7", high = "#132b43") +
      
      guides(color = guide_legend(), size = guide_legend()) +
      
      scale_size_continuous(limits = c(0, chw.high), 
                            breaks = seq(0, chw.high, by = chw.br), 
                            labels = comma) +
      
      labs(title = "The University of Texas at Austin", 
           subtitle = "Total AHU Chilled Water Valve Leakage (ton-hr) per Building", 
           size = "CHW Leakage \n(ton-hr)",
           color = "CHW Leakage \n(ton-hr)") + 
      
      geom_label_repel(aes(x = Longitude, y = Latitude, label = Building), data = chw.sub) +
      
      theme(legend.justification = c(1,0), 
            legend.position = c(0.95, 0.05), 
            legend.key = element_blank(), 
            legend.background = element_rect(fill = "#EBEBEB", size = 0.5, 
                                             linetype = "solid", color = "black"),
            legend.title = element_text(face = "bold"),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
    
    chw.map
    
    ggsave("./outputs/AHU_Leakage_Maps/CHW_Leakage_Map.png", chw.map)
    
    # steam leakage map
    stm.high <- signif(max(stm.sub$Leakage.lbs), digits = 2) + 10000
    stm.br <- stm.high/4
    
    stm.map <- ggmap(map) + 
      
      geom_point(data = stm.sub,
                 aes(x = Longitude, y = Latitude, 
                     size = Leakage.lbs, 
                     color = Leakage.lbs)) + 
      
      scale_color_gradient(low = "#f95562", high = "#43121a", guide = "legend", 
                           limits = c(0, stm.high), breaks = seq(0, stm.high, by = stm.br),
                           labels = comma) +
      
      scale_fill_gradient(low = "#f95562", high = "#43121a") +
      
      guides(color = guide_legend(), size = guide_legend()) +
      
      scale_size_continuous(limits = c(0, stm.high), 
                            breaks = seq(0, stm.high, by = stm.br), 
                            labels = comma) +
      
      labs(title = "The University of Texas at Austin", 
           subtitle = "Total AHU Steam Valve Leakage (lbs) per Building", 
           size = "STM Leakage \n(lbs)",
           color = "STM Leakage \n(lbs)") + 
      
      geom_label_repel(aes(x = Longitude, y = Latitude, label = Building), data = stm.sub) +
      
      theme(legend.justification = c(1,0), 
            legend.position = c(0.95, 0.05), 
            legend.key = element_blank(), 
            legend.background = element_rect(fill = "#EBEBEB", size = 0.5, 
                                             linetype = "solid", color = "black"),
            legend.title = element_text(face = "bold"),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.ticks.x = element_blank(), axis.ticks.y = element_blank())
    
    stm.map
    
    ggsave("./outputs/AHU_Leakage_Maps/STM_Leakage_Map.png", stm.map)
    
  }
  
  
  ########## LEAFLET MAP ########## 
  
  # prepare DFs for popup map info
  chw.popup <- chw.leak[,c("Bldg.ID","Building","AHU","Total.CHW.Vlv.ton_hr")]
  stm.popup <- stm.leak[,c("Bldg.ID","Building", "AHU","Total.STM.Vlv.lbs")]
  
  chw.popup <- merge(chw.popup, chw.sub[,c(2,5:6)], by = c("Bldg.ID"))
  stm.popup <- merge(stm.popup, stm.sub[,c(2,5:6)], by = c("Bldg.ID"))
  
  chw.popup <- chw.popup[order(chw.popup$Total.CHW.Vlv.ton_hr, decreasing = TRUE),]
  stm.popup <- stm.popup[order(stm.popup$Total.STM.Vlv.lbs, decreasing = TRUE),]
  
  
  ##### Label maker #####
  
  # chw labels
  
  chw.char <- character(length(chw.popup$AHU))
  
  for (j in 1:length(chw.char)) {
    chw.char[j] <- paste(chw.popup$AHU[j], ":", 
                         ceiling(chw.popup$Total.CHW.Vlv.ton_hr[j]), 
                         "ton-hr", "<br>")
  }
  
  chw.popup$Labels <- chw.char
  
  for (i in 1:nrow(chw.sub)) {
    b <- chw.sub$Bldg.ID[i]
    chw.sub$Labels[i] <- paste("<b>", paste0("<a href = 'https://facilitiesservices.utexas.edu/buildings/UTM/", chw.sub$Bldg.ID[chw.sub$Bldg.ID == b], "'>"), chw.sub$Building[chw.sub$Bldg.ID == b], "</a><br>", 
                               "Total leakage:",
                               ceiling(sum(chw.popup$Total.CHW.Vlv.ton_hr[chw.popup$Bldg.ID == b])),"ton-hr", "</b>",
                               "<br>", paste(chw.popup$Labels[chw.popup$Bldg.ID == b], collapse = " "))
  }
  
  # steam labels
  stm.char <- character(length(stm.popup$AHU))
  
  for (j in 1:length(stm.char)) {
    stm.char[j] <- paste(stm.popup$AHU[j], ":", 
                         ceiling(stm.popup$Total.STM.Vlv.lbs[j]), 
                         "lbs", "<br>")
  }
  
  stm.popup$Labels <- stm.char
  
  for (i in 1:nrow(stm.sub)) {
    b <- stm.sub$Bldg.ID[i]
    stm.sub$Labels[i] <- paste("<b>", paste0("<a href = 'https://facilitiesservices.utexas.edu/buildings/UTM/", stm.sub$Bldg.ID[stm.sub$Bldg.ID == b], "'>"), stm.sub$Building[stm.sub$Bldg.ID == b], "</a><br>", 
                               "Total leakage:",
                               ceiling(sum(stm.popup$Total.STM.Vlv.lbs[stm.popup$Bldg.ID == b])),"lbs", "</b>",
                               "<br>", paste(stm.popup$Labels[stm.popup$Bldg.ID == b], collapse = " "))
  }
  
  saveRDS(chw.sub, "./Shiny-FDD/RDS/chw_leaf.rds")
  saveRDS(stm.sub, "./Shiny-FDD/RDS/stm_leaf.rds")
  
  ########## INTERACTIVE MAP ########## 
  
  if (grepl(pattern = "chw", interactive_map, ignore.case = T)) {
    
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
    
  } else if (grepl(pattern = "stm", interactive_map, ignore.case = T)) {
    
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
  } else {
    
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
  
}

#################### FUNCTION END ####################



