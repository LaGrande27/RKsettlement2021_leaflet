packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
 }

packages(dplyr)
packages(rgdal)
packages(purrr)
packages(ggplot2)
packages(lemon) #for function facet_rep_wrap()
packages(lubridate) #time stuff
packages(viridis) #viridis color scale
packages(shiny) #shiny app
packages(ggmap) #if using maps in shiny output
packages(leaflet) #map making
packages(sf) #transforming data into WGS84 coordinates
packages(htmltools) #if using browsable in leaflet

library(dplyr)
library(rgdal)
library(purrr)
library(ggplot2)
library(lemon) #for function facet_rep_wrap()
library(lubridate) #time stuff
library(viridis) #viridis color scale
library(shiny) #shiny app
library(ggmap) #if using maps in shiny output
library(leaflet) #map making
library(sf) #transforming data into WGS84 coordinates
library(htmltools) #if using browsable in leaflet

kde.percent = 90
days.stationary = 5             # moving window approach with a X day window
minimum.datapoints.per.daylight = 2  # minimum data points during the day. I chose 2 because in winter there is only 1 point every 6h, so at 12:00 and 18:00
first.GPS.point = 9             # GPS points considered should be between 9:00:00
last.GPS.point = 20             # and 20:00:00 (to avoid getting only roosting points -> falsely small home range)
kde.cutoff.CH = 6               # using country-dependent home range cut-offs to account for different habitat qualities
kde.cutoff.CZ = 30              # cutoff.CH used for FR, CH, AT, DE; cutoff.CZ for CZ
radius = 1500                   #1.5 km radius around nest
make.visible = 2                #value to add to size of the points acc. to KDE size, but 1km2 would be too small to see well, so all values get +2

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis





setkde <- read.table("KDEsettlers_per_5days_2ppd.csv", header=T, dec=".", sep=" ")
settlers.nest <- read.table("list_of_nests_in_SettYear.csv", header=T, dec=".", sep=" ")

setkde$START_DATE <- as.Date(setkde$START_DATE, format="%Y-%m-%d")
setkde$DATE <- as.Date(paste0("2020-", substr(setkde$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
setkde$YEAR <- substr(setkde$START_DATE, 1, 4)
setkde$YEAR <- factor(setkde$YEAR, levels=c("2015","2016","2017","2018","2019","2020"))
levels(setkde$settlement.year) <- list("3y before settling"="3y_prior_sett",
                                       "2y before settling"="2y_prior_sett",
                                       "1y before settling"="1y_prior_sett",
                                       "settlement year"="sett_year", 
                                       "1y after settling"="1y_after_sett",
                                       "2y after settling"="2y_after_sett")#, "<br>Color = ", color)
setkde$julian <- julian(setkde$DATE, origin=as.Date("2020-01-01")) #as workaround for color legend in leaflet map

#turn into sf object with WGS84 coordinates. Doesn't change the x & y values, since those are already
#in longitude & latitude, but it might help with the scale bar etc.
#If EPSG 3035 should be used, add: %>% st_transform(CRS(EU.CRS))
setkde_sf <- setkde %>% 
  st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)

settlers.nest_sf <- settlers.nest %>% 
  st_as_sf(coords=c("nest.x","nest.y")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)


# function to display logarithmic scales with necessary decimal places only and with comma as thousands separator
plain <- function(x,...) format(x, ..., scientific = FALSE, drop0trailing = TRUE, big.mark=",")

  

########### 1 - create used interface ###########

ui <- fluidPage(
  navbarPage(     #also possible: tabsetPanel(
    "Red Kite Kernel home ranges",
    # Two tabs, one for a specific individual, the other as overview of home range development for all kites
    tabPanel("Individuals",
             # App title
             #titlePanel(""),
             
             fluidRow(
               # Build selection tool for changing the individual to be displayed
               column(8,
                      wellPanel(
                        selectInput(inputId = "ID", label = "Red Kite", choices = unique(setkde$ID),
                                    selected = "KISW01", multiple = F))
                      ),
               # Change length of moving window
               #column(4,
                #      sliderInput(inputId = "window.length",
                 #                 label = "Length of moving window",
                  #                min = 4,
                   #               max = 10,
                    #              step = 1,
                     #             value = 5)
                      #),
               # Change cutoff/threshold value of home range. Only home ranges below that value will be considered for settlements and potatoes
               column(4,
                     sliderInput(inputId = "cutoff",
                                label = "Max. home range size for map display",
                               min = 3,
                              max = 50,
                             step = 1,
                            value = 6)
                     )
             ),
             
             # Displaying outputs
             # Output for selected individuals: Date vs. Home Range
             fluidRow(
               column(12,
                      h3("Home range sizes throughout the year")
               )
             ),
             # Sidebar layout with input and output definitions
             fluidRow(
               column(3,
                      wellPanel(
                        selectInput(inputId = "colouration", label=h5("Colour coding"), choices = c("Longitude"="Longitude",
                                                                                                    "Latitude"="Latitude",
                                                                                                    "Settlement Year"="Settlement Year"),
                                    selected = "Settlement Year", multiple = F),
                        h5("Outline migration dates"),
                        checkboxInput(inputId = "checkbox", 
                                      label = HTML("grey contour = on migration,<br />black outline = departure/arrival day with sufficient data"), 
                                      value=TRUE)
                      )),
               column(9,
                      plotOutput(outputId = "plot.Individual",
                                 width = "auto", height = "350", 
                                 click = "plot_click_Ind",
                                 brush = "plot_brush_Ind")
               )
             ),
             # Show data of points when clicking on them
             fluidRow(
               column(12, offset=0,
                      verbatimTextOutput("plot.Ind.click_brush")
               )
             ),
             
             # Output for selected individuals: Plotting home range centers on a map
             # Plots with overview graph left and zoom-graph right
             fluidRow(
               column(width = 12, #class = "well",
                      h3("Map of home ranges below threshold"),
                      fluidRow(
                        column(width = 12,
                               leafletOutput("zoomplot", height = 500
                               )
                        )
                      )
               )
             )
    ),
    tabPanel("All Red Kites",
             fluidRow(
               column(6,
                      wellPanel(
                        selectInput(inputId = "colouration.All", label=h5("Colour coding"), choices = c("Longitude"="Longitude",
                                                                                                        "Latitude"="Latitude",
                                                                                                        "Settlement Year"="Settlement Year"),
                                    selected = "Settlement Year", multiple = F)
                        )
                      ),
               column(6,
                      wellPanel(
                        h5("Data Confidence"),
                        checkboxInput(inputId = "datasecurity",
                                      label=HTML("Highlight days with<br /><30 points (medium size, semi-transparent)<br />or <20 points (big, opaque)<br />for entire moving window length"),
                                      value = F)
                        )
                      )
               ),
             fluidRow(
               column(12,
                      plotOutput(outputId = "plot.All",
                                 width = "auto", height = "3000",
                                 click = "plot_click_All",
                                 brush = "plot_brush_All"),
                      
                      #htmlOutput("info.All")
                      )
               )
             )
  )
)


############### 2 - create server ###############

server <- function(input, output){
  ### Panel 1 ----------------------------------------------------------------------------- ###
  
  # Create a subset of data filtering for chosen bird.ID level(s)
  sub_setkde <- reactive({
    setkde[setkde$ID == input$ID,]
    #setkde[setkde$ID == input$ID & setkde$WINDOW.LENGTH == input$window.length, ]
    
    #if(input$window.length = 4) {
    #setkde <- subset(setkde$WINDOW.LENGTH == 4)
    #}else if(input$window.length == 5){
    #setkde <- subset(setkde$WINDOW.LENGTH == 5)
    #}else if(input$window.length == 6){
    #setkde <- subset(setkde$WINDOW.LENGTH == 6)
    #}else if(input$window.length == 7){
    #setkde <- subset(setkde$WINDOW.LENGTH == 7)
    #}else if(input$window.length == 8){
    #setkde <- subset(setkde$WINDOW.LENGTH == 8)
    #}else if(input$window.length == 9){
    #setkde <- subset(setkde$WINDOW.LENGTH == 9)
    #}else if(input$window.length == 10){
    #setkde <- subset(setkde$WINDOW.LENGTH == 10)
    #}
    
    })
  
  # different KDE cutoff for red kite breeding in Czech Republic (still activated for All kites plot)
  #kde.cutoff <- reactive({ if (input$ID == "KIWS95") {kde.cutoff.CZ} else {kde.cutoff.CH} })
  offset <- reactive({
    if (input$cutoff >15) {6} else {1.2}
  })
  #offset2 <- reactive({ if (input$ID == "KIWS95") {6} else {1.2} })
  
  
  ### --- Home Range during the year --- ###
  # Plot of one tagged inidivual; Date vs. Home Range
  output$plot.Individual <- renderPlot({
    
    # draw graph
    p2 <- sub_setkde() %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      xlab(NULL) +
      ylab(expression(Kernel~home~range~(km^2))) +
      ggtitle(paste0(input$ID)) + 
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      annotation_logticks(side="l", size=0.4, colour="black") +
      theme(plot.title = element_text(hjust = 0.5),
            panel.border=element_blank(), axis.line=element_blank(),
            legend.position = "bottom", legend.key.width=unit(2,"cm"), legend.box = "vertical",
            axis.text.x  = element_text(hjust=-0.5)) +
      scale_shape_manual("", values = c("2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    if(input$checkbox==TRUE){
      p2 <- p2 +
        # function to use in ggplot for subsetting specific data
        #pick <- function(condition){ function(d) d %>% filter(!!enquo(condition)) }
        #geom_point(data=pick(on.migration=="yes"), col="grey", cex=0.95) +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    # change colouration
    if(input$colouration=="Longitude"){
      p2.5 <- p2 +
        geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.9) +
        scale_alpha(range = c(0.4, 1)) +
        viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LON_X), max(setkde$CENTER.LON_X))) +
        labs(col = "Longitude")
    } else if(input$colouration == "Latitude"){
      p2.5 <- p2 +
        geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.9) +
        viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LAT_Y), max(setkde$CENTER.LAT_Y))) +
        labs(col = "Latitude")
    } else if(input$colouration == "Settlement Year"){
      p2.5 <- p2 +
        geom_point(aes(col=settlement.year,
                       shape=YEAR), cex=0.9) +
        scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                           "2y before settling"="#39568CFF", 
                                           "1y before settling"="#1F968BFF", 
                                           "settlement year"="#73D055FF", 
                                           "1y after settling"="#FDE725FF", 
                                           "2y after settling"="orange"),
                            guide = guide_legend(override.aes = list(size=2)))
    }

    p2.5
  })
  
  # Find date and home range when clicking on/hovering over/brushing at points in plots
  output$plot.Ind.click_brush <- renderPrint({
      click <- nearPoints(sub_setkde(), input$plot_click_Ind, threshold=2, addDist = F) #showing points near click
      brush <- brushedPoints(sub_setkde(), input$plot_brush_Ind) #showing points near brush
      if(nrow(click)>0) {
        click <- click[order(click[,2] ), ]
        click[,c(2:4,6:7,11)] #keep only interesting rows, and only rows without factors
      } else if(nrow(brush)>1) {
        brush <- brush[order(brush[,2] ), ]
        brush1 <- brush[, c(2:4,6:7,11)] #keep only interesting rows, and only rows without factors because of Min Max function
        brush2 <- brush[, c(2:4,6:7)] #keep only interesting rows, and only rows without factors because of Min Max function
        Min <- summarize_all(brush2, min); row.names(Min) <- "min"
        Max <- summarize_all(brush2, max); row.names(Max) <- "max"
        minmax <- rbind(Min, Max)
        print(minmax)
        print(brush1)
      } else {print("click on or brush over the plot")}
      })
  
  
  ### --- Home range centers below threshold on a map --- ###
  # Create a subset of data filtering for chosen bird.ID level(s) for this type of data
  # if necessary adjust country specific kde.cutoff (in server function, first command)
  sub_setkde_cutoff <- reactive({
    #setkde[setkde$ID == input$ID & setkde$AREA_KM2 <= kde.cutoff(),]
    #setkde[setkde$ID == input$ID & setkde$AREA_KM2 <= input$cutoff & setkde$WINDOW.LENGTH == input$window.length,]
    setkde[setkde$ID == input$ID & setkde$AREA_KM2 <= input$cutoff,]
  })
  sub_setkde_cutoff2 <- reactive({
    setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff,]
  }) #this is class "sf", it includes spatial WGS84 coordinates 
  
  #find nest coordinates for each kite
  sub_setkde_nest <- reactive({
    settlers.nest[settlers.nest$TransmGSM == input$ID,]
  })
  sub_setkde_nest2 <- reactive({
    settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID,]
  }) #this is class "sf", it includes spatial WGS84 coordinates 
  
  #for migration dates only
  sub_setkde_cutoff.m <- reactive({
    setkde[setkde$ID == input$ID & setkde$AREA_KM2 <= input$cutoff & setkde$on.migration=="yes",]
  })
  sub_setkde_cutoff.m2 <- reactive({
    setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff & setkde_sf$on.migration=="yes",]
  }) #this is class "sf", it includes spatial WGS84 coordinates
  
  #actual drawing left plot (overview plot)
  output$zoomplot <- renderLeaflet({
    
    # make colour palette for Date and Year
    pal <- colorNumeric(palette = viridis(100), domain = c(1,366), reverse=T) #color legend for date (workaround with julian dates to have numeric values)
    factpal <- colorFactor(palette = c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange'), domain = NULL)
       #order has to match level order of settlement.year
    
    ### legend for settlement.year, on.migration and nest location ###
    colors <- c(rep("white",6),"red","white") #fill inside borders of circles
    labels <- c("-3 years", "-2 years", "-1 year", "settlement", "+1 year", "+2 years", "nest", "on migration")
    #labels <- c("3y before settling", "2y before settling", "1y before settling", "settlement year", "1y after settling", "2y after settling", "on migration")
    sizes <- c(rep(12,6),2,12)
    shapes <- "circle"
    margin.top <- c(rep(5,6),8,5) #margin from circles to each other, distance from top
    margin.left <- c(rep(0,6),2,0) #margin from circles to left side (to center the "nest" circle)
    borders <- c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'red', 'gray')
    
    addLegendCustom <- function(map, labels, sizes, shapes, borders, opacity = 1, position = c("topright", "bottomright", "bottomleft", "topleft")){
      position <- match.arg(position)
      make_shapes <- function(sizes, borders, shapes) {
        shapes <- gsub("circle", "100%", shapes)
        paste0(colors, "; width:", sizes, "px; margin-top:",margin.top,"px;margin-left:",margin.left,
               "px;height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
        }
      make_labels <- function(sizes, labels) {
        paste0("<div style='display: inline-block;font-size: 10px;height: ", 
               sizes, "px;margin-top: 0px;line-height: ", 
               sizes, "px;'>", labels, "</div>")
        }
      legend_colors <- make_shapes(sizes, borders, shapes)
      legend_labels <- make_labels(sizes, labels)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
    }
    
    ### legend for KDE size  ###
    colors.Size <- "black" #fill inside borders of circles
    labels.Size <- c("1","6") #labelling for km2 home size
    sizes.Size <- c(1+make.visible,12+make.visible) #value to make small KDE areas (points) better visible
    margin.top.Size <- c(7,3) #margin from circles to each other, distance from top
    margin.left.Size <- c(6,0) #margin from circles to left side (to center the "nest" circle)
    borders.Size <- "black"
    
    addLegendCustom.Size <- function(map, labels.Size, sizes.Size, shapes, borders.Size, opacity = 1, position = c("topright", "bottomright", "bottomleft", "topleft")){
      position <- match.arg(position)
      make_shapes <- function(sizes.Size, borders.Size, shapes) {
        shapes <- gsub("circle", "50%", shapes)
        paste0(colors.Size, "; width:", sizes.Size, "px; margin-top:",margin.top.Size,"px;margin-left:",
               margin.left.Size,"px;height:", sizes.Size, "px; border:2px solid ", 
               borders.Size, "; border-radius:", shapes)
        }
      make_labels <- function(sizes.Size, labels.Size) {
        paste0("<div style='display: inline-block;font-size: 10px;height: ", 
               sizes.Size, "px;margin-top: 0px;line-height: ", 
               sizes.Size, "px;'>", labels.Size, " km<sup>2</sup>  KDE</div>")
        }
      legend_colors <- make_shapes(sizes.Size, borders.Size, shapes)
      legend_labels <- make_labels(sizes.Size, labels.Size)
      
      return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position))
    }
    
    ### legend for Date coloration ###
    myLabelFormat = function(...,dates=FALSE){ 
      if(dates){ 
        function(type = "numeric", cuts){
          as <- as.Date(cuts, origin="2020-01-01")#, format="%b-%d")
          format(as,"%b")
          #div(style="font-size: 10px;", month)
        } 
      }else{
        labelFormat(...)
      }
    }
    
    
    l1 <- leaflet() %>%
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(opacity = 0.7)) %>%  #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      #setView(lng = -71.0589, lat = 42.3601, zoom = 12)
      #fitBounds(lng1=min(sub_setkde_cutoff2[,6]-1), lng2=max(sub_setkde_cutoff2[,6]+1), 
       #         lat1=min(sub_setkde_cutoff2[,7]-1), lat2=max(sub_setkde_cutoff2[,7]+1)) %>%
      addMapPane("nest", zIndex = 430) %>%  #puts nest on top of all layers
      addCircleMarkers( #date colour-coded centers of home ranges
        data=sub_setkde_cutoff2(), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=13),
        radius = ~AREA_KM2+make.visible,
        color = ~pal(julian),
        stroke = FALSE, fillOpacity = 1,
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year)
      ) %>% 
      addCircleMarkers( #indicator for migration dates
        data=sub_setkde_cutoff.m2(),
        radius = ~AREA_KM2+5.5,
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>% 
      addCircleMarkers( #indicator for year
        data=sub_setkde_cutoff2(), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        radius = ~AREA_KM2+2,
        color = ~factpal(settlement.year),
        stroke = T, fill = F,
        opacity = 1
      ) %>% 
      addCircleMarkers( #add nest
        data=sub_setkde_nest2(), #lng=~nest.x, lat=~nest.y,
        radius = 3,
        color = "red",
        stroke = FALSE, fillOpacity = 1,
        options = pathOptions(pane = "nest")
      ) %>% 
      addCircles( #add radius of 1.5 km around nest
        data=sub_setkde_nest2(), #lng=~nest.x, lat=~nest.y,
        radius = 1500,
        color = "black",
        weight=1,
        stroke = TRUE, fill=F
      ) %>% 
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = F)) %>% 
      addLegendCustom(labels, sizes, shapes, borders, position="topleft") %>% #,title="Settlement information") %>% 
      addLegendCustom.Size(labels.Size, sizes.Size, shapes, borders.Size, position="topleft") %>% 
      addLegend(
        data = sub_setkde_cutoff2(),
        position = "bottomright", 
        pal = pal,
        values = ~julian,
        opacity = 1,
        bins=6,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      )
    l1
    
  })
  

  ### Panel 2 ----------------------------------------------------------------------------- ###
  # Plot for all Individuals
  output$plot.All <- renderPlot({
    
    p5 <- setkde %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      xlab(NULL) +
      ylab(expression(Kernel~home~range~(km^2))) +
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      annotation_logticks(side="l", size=0.4, colour="black") +
      facet_rep_wrap(~ID, scales="fixed", ncol=4, nrow=20) +
      theme(panel.border=element_blank(), axis.line=element_blank(),
            #axis.text.x  = element_text(hjust=-0.5),
            legend.position = "bottom", legend.key.width=unit(2,"cm"), legend.box = "vertical") +
      scale_shape_manual("", values = c("2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    if(input$checkbox==TRUE){
      p5 <- p5 +
        geom_point(aes(shape=YEAR), cex=ifelse(setkde$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(setkde$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(setkde$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    if(input$datasecurity==TRUE){
      if(input$colouration.All=="Longitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=1.4, data=subset(setkde, NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=4, data=subset(setkde, NR_POINTS<20)) +
          #geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=4, data=subset(setkde, NR_POINTS<=4*input$window.length)) +
          viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LON_X), max(setkde$CENTER.LON_X))) +
          labs(col = "Longitude")
      }else if(input$colouration.All == "Latitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=1.4, data=subset(setkde, NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=4, data=subset(setkde, NR_POINTS<20)) +
          #geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=4, data=subset(setkde, NR_POINTS<=4*input$window.length)) +
          viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LAT_Y), max(setkde$CENTER.LAT_Y))) +
          labs(col = "Latitude")
      }else if(input$colouration.All == "Settlement Year"){
        p5.5 <- p5 +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=0.5, alpha=0.2) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=1.4, data=subset(setkde, NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=4, data=subset(setkde, NR_POINTS<20)) +
          scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                             "2y before settling"="#39568CFF", 
                                             "1y before settling"="#1F968BFF", 
                                             "settlement year"="#73D055FF", 
                                             "1y after settling"="#FDE725FF", 
                                             "2y after settling"="orange"),
                              guide = guide_legend(override.aes = list(size=2),
                                                   color = guide_legend(nrow = 1)))
        }
      } else {
    
        if(input$colouration.All=="Longitude"){
          p5.5 <- p5 +
            geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.9, alpha=0.9) +
            #scale_size(range=c(3,0.5), trans="log") +
            viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LON_X), max(setkde$CENTER.LON_X))) +
            labs(col = "Longitude")
        }else if(input$colouration.All == "Latitude"){
          p5.5 <- p5 +
            geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.9) +
            viridis::scale_color_viridis(limits=c(min(setkde$CENTER.LAT_Y), max(setkde$CENTER.LAT_Y))) +
            labs(col = "Latitude")
        }else if(input$colouration.All == "Settlement Year"){
          p5.5 <- p5 +
            geom_point(aes(col=settlement.year,
                           shape=YEAR), cex=0.9) +
            scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        }
      }
    
    p5.5
    
  })
} 


############### 3 - start shinyApp ##############

shinyApp(ui = ui, server = server)
