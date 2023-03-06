# Obstetrics in Germany

options("rgdal_show_exportToProj4_warnings" = "none")

#### Load packages ####

library(shiny)
library(shinydashboard)
library(shinythemes)
library(leaflet)
library(rgdal)
library(htmltools) # fuer die Infolabels der Krankenhaeuser
library(htmlwidgets)# um dynamische Karten als html zu speichern
library(plyr)
library(dplyr)
library(readr)
library(rsconnect) #deploys applications to the shinyapps.io service
library(raster)
library(rmapshaper) # makes spatial files faster
library(sp)


#### Read and prepare data ####

##### Data for 2014 #####

load("www/output_2014_readytomap_and_analyse_NEWCASESincl.Rdata")

# 2 NAs in der Variablen Anzahl Kinder manuell nachtragen

Data2014_lonlat_BelegAerzte_excluded_Gebfinal <- Data2014_lonlat_BelegAerzte_excluded_Gebfinal %>%
  dplyr::transmute(Data2014_lonlat_BelegAerzte_excluded_Gebfinal, Anzahl.Kinder = if_else(Data2014_lonlat_BelegAerzte_excluded_Gebfinal$QB_ID == 457,
                                                                                   Data2014_lonlat_BelegAerzte_excluded_Gebfinal$Anzahl.Lebendgeburten.OPS,
                                                                                   if_else(Data2014_lonlat_BelegAerzte_excluded_Gebfinal$QB_ID == 2019,
                                                                                           Data2014_lonlat_BelegAerzte_excluded_Gebfinal$Anzahl.Lebendgeburten.OPS,Data2014_lonlat_BelegAerzte_excluded_Gebfinal$Anzahl.Kinder)))

Mapdata2014 <- Data2014_lonlat_BelegAerzte_excluded_Gebfinal

Sample2014 <- Mapdata2014

Sample2014 <- dplyr::as_tibble(Sample2014)

# BundL <- rgdal::readOGR("www/gadm36_DEU_1.shp") 

# gc(full = TRUE)

# BundL_simple <- rmapshaper::ms_simplify(BundL)

# rgdal::writeOGR(BundL_simple,"www/gadm36_DEU_1_simple.shp", driver = "ESRI Shapefile", layer= "DEU")

BundL_simple <- rgdal::readOGR("www/gadm36_DEU_1_simple.shp")

gc(full = TRUE)

# Variablen, die auf der Karte angezeigt werden ins Englische übersetzen

names(Sample2014) [16] <- "Ownership"
names(Sample2014) [10] <- "Pediatrics department"

# Ja/Nein Variable umkodieren

Sample2014$`Pediatrics department` <- plyr::mapvalues(Sample2014$`Pediatrics department`, from = c(0, 1), to = c("No", "Yes"))

# Auspreagungen der Variable Ownership noch ins Englische uebersetzen

Sample2014$Ownership <- plyr::mapvalues(Sample2014$Ownership, from = c("freigemeinnützig","öffentlich","privat"), to = c(1,2,3))
Sample2014$Ownership <- factor(Sample2014$Ownership,
                               levels = c(1,2,3),
                               labels = c("chartitable hospital","public hospital","private hospital"))

# Jetzt noch eine neue Variable erstellen, die die Geburtenzahlen kategorisiert

Sample2014 <- dplyr::mutate(Sample2014, "Children born in hospital" = if_else(Sample2014$Anzahl.Kinder <= 500, "> 0 - 500", 
                                                                       if_else(Sample2014$Anzahl.Kinder > 500 & Sample2014$Anzahl.Kinder <= 1000, "501 - 1000", 
                                                                               if_else(Sample2014$Anzahl.Kinder > 1000 & Sample2014$Anzahl.Kinder <= 1500, "1001 - 1500", 
                                                                                       if_else(Sample2014$Anzahl.Kinder > 1500 & Sample2014$Anzahl.Kinder <= 2000, "1501 - 2000",
                                                                                               if_else(Sample2014$Anzahl.Kinder > 2000 & Sample2014$Anzahl.Kinder <= 2500, "2001 - 2500",
                                                                                                       if_else(Sample2014$Anzahl.Kinder > 2500 & Sample2014$Anzahl.Kinder <= 3000, "2501 - 3000","> 3000")))))))

# Noch Infolabels zu den einzelnen Krankenhaeusern hinzufügen

Sample2014$Label <- paste("<p>","Ownership:", " ",Sample2014$Ownership, "</p>",
                          "<p>", "Livebirths in hospital:", " ",Sample2014$`Children born in hospital`, "</p>",
                          "<p>", "Pediatrics department:", " ", Sample2014$`Pediatrics department`, "</p>")


# Subdatensatz fuer die verschiedenen Bereiche fuer Anzahl an Kindern

Sample2014_childrenbornUnder500 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "> 0 - 500")

Sample2014_childrenbornUnder1000 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "501 - 1000")

Sample2014_childrenbornUnder1500 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "1001 - 1500")

Sample2014_childrenbornUnder2000 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "1501 - 2000")

Sample2014_childrenbornUnder2500 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "2001 - 2500")

Sample2014_childrenbornOver3000 <- Sample2014 %>%
  dplyr::filter(Sample2014$`Children born in hospital` == "> 3000")

Sample2014_Pediatrics <- Sample2014 %>%
  dplyr::filter(Sample2014$`Pediatrics department` == "Yes")

Sample2014_nopediatrics <- Sample2014 %>%
  dplyr::filter(Sample2014$`Pediatrics department` == "No")


##### Data for 2019 #####

# Datenladen

load("www/output_2019_readytomap_and_analyse_NEWCASES.Rdata")

# 1 NAs in der Variablen Anzahl Kinder manuell nachtragen

Data2019_lonlat_BelegAerzte_excluded_Gebfinal <- Data2019_lonlat_BelegAerzte_excluded_Gebfinal %>%
  dplyr::transmute(Data2019_lonlat_BelegAerzte_excluded_Gebfinal, Anzahl.Kinder = if_else(Data2019_lonlat_BelegAerzte_excluded_Gebfinal$QB_ID == 1878,
                                                                                   Data2019_lonlat_BelegAerzte_excluded_Gebfinal$Anzahl.Lebendgeburten.OPS,
                                                                                   Data2019_lonlat_BelegAerzte_excluded_Gebfinal$Anzahl.Kinder))

Mapdata2019 <- Data2019_lonlat_BelegAerzte_excluded_Gebfinal

Sample2019 <- Mapdata2019

Sample2019 <- dplyr::as_tibble(Sample2019)


# Variablen, die auf der Karte angezeigt werden ins Englische übersetzen

names(Sample2019) [17] <- "Ownership"
names(Sample2019) [11] <- "Pediatrics department"

# Ja/Nein Variable umkodieren

Sample2019$`Pediatrics department` <- plyr::mapvalues(Sample2019$`Pediatrics department`, from = c(0, 1), to = c("No", "Yes"))

# Auspreagungen der Variable Ownership noch ins Englische uebersetzen

Sample2019$Ownership <- plyr::mapvalues(Sample2019$Ownership, from = c("freigemeinnützig","öffentlich","privat"), to = c(1,2,3))
Sample2019$Ownership <- factor(Sample2019$Ownership,
                               levels = c(1,2,3),
                               labels = c("chartitable hospital","public hospital","private hospital"))

# Jetzt noch eine neue Variable erstellen, die die Geburtenzahlen kategorisiert

Sample2019 <- dplyr::mutate(Sample2019, "Children born in hospital" = if_else(Sample2019$Anzahl.Kinder <= 500, "> 0 - 500", 
                                                                       if_else(Sample2019$Anzahl.Kinder > 500 & Sample2019$Anzahl.Kinder <= 1000, "501 - 1000", 
                                                                               if_else(Sample2019$Anzahl.Kinder > 1000 & Sample2019$Anzahl.Kinder <= 1500, "1001 - 1500", 
                                                                                       if_else(Sample2019$Anzahl.Kinder > 1500 & Sample2019$Anzahl.Kinder <= 2000, "1501 - 2000",
                                                                                               if_else(Sample2019$Anzahl.Kinder > 2000 & Sample2019$Anzahl.Kinder <= 2500, "2001 - 2500",
                                                                                                       if_else(Sample2019$Anzahl.Kinder > 2500 & Sample2019$Anzahl.Kinder <= 3000, "2501 - 3000","> 3000")))))))

# Noch Infolabels zu den einzelnen Krankenhaeusern hinzufügen

Sample2019$Label <- paste("<p>","Ownership:", " ",Sample2019$Ownership, "</p>",
                          "<p>", "Livebirths in hospital:", " ",Sample2019$`Children born in hospital`, "</p>",
                          "<p>", "Pediatrics department:", " ", Sample2019$`Pediatrics department`, "</p>")


# Subdatensatz fuer Paediatrie Ja/Nein

Sample2019_Pediatrics <- Sample2019 %>%
  dplyr::filter(Sample2019$`Pediatrics department` == "Yes")

Sample2019_nopediatrics <- Sample2019 %>%
  dplyr::filter(Sample2019$`Pediatrics department` == "No")

# Subdatensatz fuer die verschiedenen Bereiche fuer Anzahl an Kindern

Sample2019_childrenbornUnder500 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "> 0 - 500")

Sample2019_childrenbornUnder1000 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "501 - 1000")

Sample2019_childrenbornUnder1500 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "1001 - 1500")

Sample2019_childrenbornUnder2000 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "1501 - 2000")

Sample2019_childrenbornUnder2500 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "2001 - 2500")

Sample2019_childrenbornOver3000 <- Sample2019 %>%
  dplyr::filter(Sample2019$`Children born in hospital` == "> 3000")


##### Load spatial data #####

mindistances2014 <- readr::read_rds("www/mindistances2014_NEWCASESincl.RDS")

mindistances2019 <- readr::read_rds("www/mindistances2019_NEWCASESincl.RDS")

mindistances2019_pediatrics <- readr::read_rds("www/mindistances2019_NEWCASESincl_pediatrics.RDS")

mindistances2019_births600 <- readr::read_rds("www/mindistances2019_NEWCASESincl_births600.RDS")




# Datensaetze von Jahr 2014 und 2019 mit Fahrzeiten ueber 30 Minuten erstellen

mindistanceFZ2014 <- mindistances2014 %>%
  dplyr::filter(mindistances2014$mintime > 30)

mindistanceFZ2019 <- mindistances2019 %>%
  dplyr::filter(mindistances2019$mintime > 30)


# Datensaetze 2019 pediatrics mit Fahrzeiten ueber 30 Minuten erstellen


mindistanceFZ2019_pediatrics <- mindistances2019_pediatrics %>%
  dplyr::filter(mindistances2019_pediatrics$mintime > 30)

# Datensaetze von 2019 pediatrics mit Fahrzeiten ueber 40 Minuten erstellen


mindistanceFZ_40_2019_pediatrics <- mindistances2019_pediatrics %>%
  dplyr::filter(mindistances2019_pediatrics$mintime > 40)


# Datensatz fuer 2019 filtern, sodass nur noch KH mit >= 600 Geburten drin sind

Sample2019_births600 <- Sample2019 %>%
  dplyr::filter(Sample2019$Anzahl.Kinder >= 600)

# Datensaetze 2019 births600 mit Fahrzeiten ueber 30 Minuten erstellen


mindistanceFZ2019_births600 <- mindistances2019_births600 %>%
  dplyr::filter(mindistances2019_births600$mintime > 30)

# Datensaetze von 2019 births600 mit Fahrzeiten ueber 40 Minuten erstellen

mindistanceFZ_40_2019_births600 <- mindistances2019_births600 %>%
  dplyr::filter(mindistances2019_births600$mintime > 40)


##### Create color palettes #####

binpalgreen <- colorBin(palette = c("lightgreen","green","yellow","orange","red"),
                        domain = min(mindistances2014["mintime"]):max(mindistances2014["mintime"]),
                        bins = c(0,15,30,40,60,200),
                        na.color = "#00000000")

binpal2 <- colorBin(palette = c("yellow","yellow4"),
                    domain = min(mindistanceFZ2014["mintime"]):max(mindistanceFZ2014["mintime"]),
                    bins = c(30,40,200),
                    na.color = "#00000000")

binpal3 <- colorBin(palette = c("firebrick1","firebrick4"),
                    domain = min(mindistanceFZ2019["mintime"]):max(mindistanceFZ2019["mintime"]),
                    bins = c(30,40,200),
                    na.color = "#00000000")


binpal.new.2014 <- colorBin(palette = c("#fff7bc","#d95f0e"),
                            domain = min(mindistanceFZ2014["mintime"]):max(mindistanceFZ2014["mintime"]),
                            bins = c(30,40,200),
                            na.color = "#00000000")

binpal.new.2019 <- colorBin(palette = c("#fff7bc","#d95f0e"),
                            domain = min(mindistanceFZ2019["mintime"]):max(mindistanceFZ2019["mintime"]),
                            bins = c(30,40,200),
                            na.color = "#00000000")


binpal.new.2019.pediatrics <- colorBin(palette = c("#fff7bc","#d95f0e"),
                                       domain = min(mindistanceFZ2019_pediatrics["mintime"]):max(mindistanceFZ2019_pediatrics["mintime"]),
                                       bins = c(30,40,200),
                                       na.color = "#00000000")

binpal.new.2019.births600 <- colorBin(palette = c("#fff7bc","#d95f0e"),
                                      domain = min(mindistanceFZ2019_births600["mintime"]):max(mindistanceFZ2019_births600["mintime"]),
                                      bins = c(30,40,200),
                                      na.color = "#00000000")

#### Dashboard ####

# UI #####

# Define UI for application that draws a histogram
ui <- bootstrapPage(
  tags$head(includeHTML("www/gtag.html")),
  navbarPage(theme = shinytheme("flatly"), collapsible = TRUE,
             HTML('<a style="text-decoration:none;cursor:default;color:#FFFFFF;" class="active" href="#">Obstetric departments in Germany</a>'), id="nav",
             windowTitle = "Obstetric departments in Germany",
             
             tabPanel("Obstetric departments",
                     
                      div(class="outer",
                          tags$head(includeCSS("www/styles.css")),
                          leaflet::leafletOutput("mymap1", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                          
                          radioButtons("RB1",
                                       label = h5("Choose the year:"),
                                       choices = list("Hospital sites with an obstetrics department in 2014", 
                                                      "Hospital sites with an obstetrics department in 2019")
                          )))),
             
             tabPanel("Accessibility",
                      
                      column(6,
                      tabsetPanel(
                        tabPanel("For the year 2014",
                                
                                     leaflet::leafletOutput("mymap2",height="800px"),
                                 
                                          ))),
             
                      column(6,
                       tabsetPanel(
                        tabPanel("For the year 2019",
                                 
                                 leaflet::leafletOutput("mymap3",height="800px"),
                               
                               
                      )))
             ),
             
             tabPanel("Differences in accessibility",
                      
                      div(class="outer",
                          tags$head(includeCSS("www/styles.css")),
                          leaflet::leafletOutput("mymap4", width="100%", height="100%"),
                          
                          absolutePanel(id = "controls", class = "panel panel-default",
                                        top = 75, left = 55, width = 400, fixed=TRUE,
                                        draggable = TRUE, height = "auto",
                                        
                                        radioButtons("RB3",
                                                     label = h5("Choose which map you want to see:"),
                                                     choices = list("Differences in driving times above 30 minutes for 2014 and 2019", 
                                                                    "Driving times above 30 minutes for 2014",
                                                                    "Driving times above 30 minutes for 2019")
                                        )))),
             
             
             
             tabPanel("Health policy scenarios",
                      
                      column(6,
                             tabsetPanel(
                               tabPanel("Only obstetric hospital sites with an additional pediatrics department",
                                        
                                        leaflet::leafletOutput("mymap5",height="800px"),
                                        
                          
                               ))),
                      
                      column(6,
                             tabsetPanel(
                               tabPanel("Only obstetric hospital sites with annual number of livebirths over 600",
                                        
                                        leaflet::leafletOutput("mymap6",height="800px"),
                                        
                                        
                               )))
                      
                    ),
             
             tabPanel("About this dashboard",
                      h4("Background"), 
                      "The maps seen in this dashboard are part of a study on the closure of hospitals with an obstetrics department in Germany. The full article is published in ... and available under ... ",
                      
                      tags$br(), tags$br(),
                      
                      h4("Underlying data"),
                      "These maps were created with data of the structured quality reports of all acute hospital sites in Germany for the years 2014 and 2019. The data is collected by",
                      tags$a(href="https://www.g-ba.de/", "the Federal Joint Comittee (G-BA)"),
                      "and are freely available on",
                      tags$a(href="https://g-ba-qualitaetsberichte.de/#/search", "the website of the G-BA"),
                      ".",
                      
                      tags$br(), tags$br(),
                      
                      
                      h4("Contact information"),
                      "For further information, you can contact:",
                      
                      "Jan Hoffmann (jan.hoffmann@uk-koeln.de)",
                      
             )

             
  ))
                      
                      

                                        
                                        

# Server ####

# Define server logic required to draw a histogram
server <- function(input, output) { # mit {} koennte jetzt hier der komplette Code aus der Maps Syntax reingeschrieben und ggf. veraendert werden
  
  output$mymap1 <- leaflet::renderLeaflet({
    if(input$RB1 == "Hospital sites with an obstetrics department in 2014")
      {leaflet::leaflet(Sample2014,
                        options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(lng = ~Sample2014_nopediatrics$lon,
                         lat = ~Sample2014_nopediatrics$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "Obstetric department only",
                         label = lapply(Sample2014_nopediatrics$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_Pediatrics$lon,
                         lat = ~Sample2014_Pediatrics$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "Additional pediatrics department",
                         label = lapply(Sample2014_Pediatrics$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornUnder500$lon,
                         lat = ~Sample2014_childrenbornUnder500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "> 0 - 500 children born",
                         label = lapply(Sample2014_childrenbornUnder500$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornUnder1000$lon,
                         lat = ~Sample2014_childrenbornUnder1000$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "501 - 1000 children born",
                         label = lapply(Sample2014_childrenbornUnder1000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornUnder1500$lon,
                         lat = ~Sample2014_childrenbornUnder1500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "1001 - 1500 children born",
                         label = lapply(Sample2014$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornUnder2000$lon,
                         lat = ~Sample2014_childrenbornUnder2000$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "1501 - 2000 children born",
                         label = lapply(Sample2014_childrenbornUnder2000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornUnder2500$lon,
                         lat = ~Sample2014_childrenbornUnder2500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "2001 - 2500 children born",
                         label = lapply(Sample2014_childrenbornUnder2500$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2014_childrenbornOver3000$lon,
                         lat = ~Sample2014_childrenbornOver3000$lat,
                         color = "yellow",
                         weight = 1,
                         radius = 5,
                         group = "> 3000 children born",
                         label = lapply(Sample2014_childrenbornOver3000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLayersControl(overlayGroups = c("Obstetrics department only",
                                           "Additional pediatrics department",
                                           "> 0 - 500 children born",
                                           "501 - 1000 children born",
                                           "1001 - 1500 children born",
                                           "1501 - 2000 children born",
                                           "2001 - 2500 children born",
                                           "> 3000 children born"
        ),
        options = layersControlOptions(collapsed = TRUE)
        )}
    else if (input$RB1 == "Hospital sites with an obstetrics department in 2019")
      {leaflet::leaflet(Sample2019,
                        options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(lng = ~Sample2019_nopediatrics$lon,
                         lat = ~Sample2019_nopediatrics$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "Obstetrics department only",
                         label = lapply(Sample2019_nopediatrics$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_Pediatrics$lon,
                         lat = ~Sample2019_Pediatrics$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "Additional pediatrics department",
                         label = lapply(Sample2019_Pediatrics$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornUnder500$lon,
                         lat = ~Sample2019_childrenbornUnder500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "> 0 - 500 children born",
                         label = lapply(Sample2019_childrenbornUnder500$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornUnder1000$lon,
                         lat = ~Sample2019_childrenbornUnder1000$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "501 - 1000 children born",
                         label = lapply(Sample2019_childrenbornUnder1000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornUnder1500$lon,
                         lat = ~Sample2019_childrenbornUnder1500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "1001 - 1500 children born",
                         label = lapply(Sample2019$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornUnder2000$lon,
                         lat = ~Sample2019_childrenbornUnder2000$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "1501 - 2000 children born",
                         label = lapply(Sample2019_childrenbornUnder2000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornUnder2500$lon,
                         lat = ~Sample2019_childrenbornUnder2500$lat,
                         color = "blue",
                         weight = 1,
                         radius = 5,
                         group = "2001 - 2500 children born",
                         label = lapply(Sample2019_childrenbornUnder2500$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(lng = ~Sample2019_childrenbornOver3000$lon,
                         lat = ~Sample2019_childrenbornOver3000$lat,
                         color = "yellow",
                         weight = 1,
                         radius = 5,
                         group = "> 3000 children born",
                         label = lapply(Sample2019_childrenbornOver3000$Label, HTML),
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLayersControl(overlayGroups = c("Obstetrics department only",
                                           "Additional pediatrics department",
                                           "> 0 - 500 children born",
                                           "501 - 1000 children born",
                                           "1001 - 1500 children born",
                                           "1501 - 2000 children born",
                                           "2001 - 2500 children born",
                                           "> 3000 children born"
        ),
        options = layersControlOptions(collapsed = TRUE)
        )}
  })
  
  gc(full = TRUE)
  
  output$mymap2 <- leaflet::renderLeaflet({
    leaflet::leaflet(Sample2014,
                     options = leafletOptions(preferCanvas = TRUE))%>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(updateWhenIdle = TRUE,
                                                       updateWhenZooming = FALSE)) %>% 
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(data = mindistances2014, lng = ~lon, lat = ~lat, radius = 5,
                         color = ~binpalgreen(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.1,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = Sample2014,
                         lng = ~lon, 
                         lat = ~lat,
                         label = lapply(Sample2014$Label, HTML),
                         radius = 1,
                         group = "Hospitals",
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLegend(data = mindistances2014, pal = binpalgreen, values = ~mintime, group = "Legend",
                  title = "Driving times to the next obstetric hospital 2014 (min)") %>% 
        addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
      })
  
  gc(full = TRUE)
  
   
    output$mymap3 <- leaflet::renderLeaflet({
      leaflet::leaflet(Sample2019,
                       options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(updateWhenIdle = TRUE,
                                                       updateWhenZooming = FALSE)) %>% 
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(data = mindistances2019, lng = ~lon, lat = ~lat, radius = 2,
                         color = ~binpalgreen(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.1,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = Sample2019,
                         lng = ~lon, 
                         lat = ~lat,
                         label = lapply(Sample2019$Label, HTML),
                         radius = 1,
                         group = "Hospitals",
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLegend(data = mindistances2019, pal = binpalgreen, values = ~mintime, group = "Legend",
                  title = "Driving time to the next obstetric hospital 2019 (min)") %>% 
        addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
    })
  
  gc(full = TRUE)
  
  output$mymap4 <- leaflet::renderLeaflet({
    if(input$RB3 == "Differences in driving times above 30 minutes for 2014 and 2019")
      {leaflet::leaflet(Sample2019,
                        options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(data = mindistanceFZ2014, lng = ~lon, lat = ~lat, radius = 1.5,
                         color = ~binpal2(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.2,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = mindistanceFZ2019, lng = ~lon, lat = ~lat, radius = 1.5,
                         color = ~binpal3(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.1,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = Sample2019,
                         lng = ~lon, 
                         lat = ~lat,
                         label = lapply(Sample2019$Label, HTML),
                         opacity = 0.4,
                         radius = 0,5,
                         group = "Hospitals",
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLegend(data = mindistanceFZ2019, pal = binpal3, values = ~mintime, group = "Legend",
                  title = "Driving times to the next obstetric hospital 2019 (min)") %>%
        addLegend(data = mindistanceFZ2014, pal = binpal2, values = ~mintime, group = "Legend",
                  title = "Driving timess to the next obstetric hospital 2014 (min)") %>% 
        addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
      
    }
    else if(input$RB3 == "Driving times above 30 minutes for 2014")
      {leaflet::leaflet(Sample2014,
                        options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(data = mindistanceFZ2014, lng = ~lon, lat = ~lat, radius = 1.5,
                         color = ~binpal.new.2014(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.1,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = Sample2014,
                         lng = ~lon, 
                         lat = ~lat,
                         label = lapply(Sample2014$Label, HTML),
                         opacity = 0.4,
                         radius = 0.5,
                         group = "Hospitals",
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLegend(data = mindistanceFZ2014, pal = binpal.new.2014, values = ~mintime, group = "Legend",
                  title = "Driving time to the next obstetric hospital in 2014 (min)") %>% 
        addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
    }
    else if(input$RB3 == "Driving times above 30 minutes for 2019")
      {leaflet::leaflet(Sample2019,
                        options = leafletOptions(preferCanvas = TRUE)) %>%
        addProviderTiles(providers$CartoDB.Positron) %>% 
        addPolygons(data = BundL_simple,
                    color = "black",
                    fill = FALSE,
                    weight = 1) %>%
        addCircleMarkers(data = mindistanceFZ2019, lng = ~lon, lat = ~lat, radius = 1.5,
                         color = ~binpal.new.2019(mintime), popup = ~mintime_str,
                         group = "Driving times",
                         opacity = 0.1,
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addCircleMarkers(data = Sample2019,
                         lng = ~lon, 
                         lat = ~lat,
                         label = lapply(Sample2019$Label, HTML),
                         opacity = 0.4,
                         radius = 0.5,
                         group = "Hospitals",
                         options = leafletOptions(preferCanvas = TRUE)) %>%
        addLegend(data = mindistanceFZ2019, pal = binpal.new.2019, values = ~mintime, group = "Legend",
                  title = "Driving time to the next obstetric hospital in 2019 (min)") %>% 
        addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
    }
  })
  
  gc(full = TRUE)
  
  output$mymap5 <- leaflet::renderLeaflet({
  leaflet(Sample2019_Pediatrics,
          options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = BundL_simple,
                  color = "black",
                  fill = FALSE,
                  weight = 1) %>%
      addCircleMarkers(data = mindistances2019_pediatrics, lng = ~lon, lat = ~lat, radius = 1.5,
                       color = ~binpal.new.2019.pediatrics(mintime), popup = ~mintime_str,
                       group = "Driving times",
                       opacity = 0.1,
                       options = leafletOptions(preferCanvas = TRUE)) %>%
      addCircleMarkers(data = Sample2019_Pediatrics,
                       lng = ~lon, 
                       lat = ~lat,
                       label = lapply(Sample2019_Pediatrics$Label, HTML),
                       opacity = 0.4,
                       radius = 0,5,
                       group = "Hospitals",
                       options = leafletOptions(preferCanvas = TRUE)) %>%
      addLegend(data = mindistances2019_pediatrics, pal = binpal.new.2019.pediatrics, values = ~mintime, group = "Legend",
                title = "Driving time to the next pediatrics and obstetrics hospital in 2019 (min)") %>% 
      addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
  })
    
  output$mymap6 <- leaflet::renderLeaflet({
  leaflet(Sample2019_births600,
          options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addPolygons(data = BundL_simple,
                  color = "black",
                  fill = FALSE,
                  weight = 1) %>%
      addCircleMarkers(data = mindistances2019_births600, lng = ~lon, lat = ~lat, radius = 1.5,
                       color = ~binpal.new.2019.births600(mintime), popup = ~mintime_str,
                       group = "Driving times",
                       opacity = 0.1,
                       options = leafletOptions(preferCanvas = TRUE)) %>%
      addCircleMarkers(data = Sample2019_births600,
                       lng = ~lon, 
                       lat = ~lat,
                       label = lapply(Sample2019_births600$Label, HTML),
                       opacity = 0.4,
                       radius = 0,5,
                       group = "Hospitals",
                       options = leafletOptions(preferCanvas = TRUE)) %>%
      addLegend(data = mindistances2019_births600, pal = binpal.new.2019.births600, values = ~mintime, group = "Legend",
                title = "Driving time to next hospital with > 600 births in 2019 (min)") %>% 
      addLayersControl(overlayGroups = c("Hospitals", "Driving times", "Legend"))
    
                         
    })




gc(full = TRUE)
  
}
  
# Run the application 
shinyApp(ui = ui, server = server)
