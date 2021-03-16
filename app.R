# checks if package is installed - if yes loads it (library/require), if not downloads it then loads it
# from: https://stackoverflow.com/questions/4090169/elegant-way-to-check-for-missing-packages-and-install-them
if (!require("pacman")) install.packages("pacman")
# for shiny app
pacman::p_load(dplyr, # always
               ggplot2,
               lemon, #for function facet_rep_wrap()
               lubridate, #time stuff
               viridis, #viridis color scale
               shiny, #shiny app
               #ggmap, #if using maps with download tiles in shiny output
               #rgdal, 
               #purrr,
               leaflet, #map making
               sf, #transforming data into WGS84 coordinates
               htmltools) #if using browsable in leaflet to make legend size smaller

options(scipen = 999) #R avoids scientific style of numbers (options(scipen=0) reset to default)

Sys.setlocale("LC_TIME", "C")  #set English hours for correct x-axis


####################### 8 #######################
############### make a shiny app ################
#################################################-------------------------------------------------

settlers <- read.table("Settlers_list_Lara_all.csv", header=T, dec=".", sep=";")
setkde5 <- read.table("KDE_AllSettlers_per_5days_2ppd_m_allbirds_relYears.csv", header=T, dec=".", sep=";") #only relevant years
setkde5.all <- read.table("KDE_AllSettlers_per_5days_2ppd_m_allbirds.csv", header=T, dec=".", sep=";")
setkde4 <- read.table("KDE_AllSettlers_per_4days_2ppd_m_allbirds_relYears.csv", header=T, dec=".", sep=";") #only relevant years
setkde4.all <- read.table("KDE_AllSettlers_per_4days_2ppd_m_allbirds.csv", header=T, dec=".", sep=";")
setkde3 <- read.table("KDE_AllSettlers_per_3days_2ppd_m_allbirds_relYears.csv", header=T, dec=".", sep=";") #only relevant years
setkde3.all <- read.table("KDE_AllSettlers_per_3days_2ppd_m_allbirds.csv", header=T, dec=".", sep=";")
settlers.nest <- read.table("list_of_nests_in2after_SettYear_allbirds.csv", header=T, dec=".", sep=";")
locations <- read.table("Settlement_Potatoes_Locations.csv", header=T, dec=".", sep=";")

# dataset: setkde ----------------------------------------------------
list.setkde <- list(setkde5=setkde5, setkde4=setkde4, setkde3=setkde3)
res1 <- lapply(list.setkde, function(setkde) {
  setkde$START_DATE <- as.Date(setkde$START_DATE, format="%Y-%m-%d")
  setkde$DATE <- as.Date(paste0("2020-", substr(setkde$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
  setkde$YEAR <- substr(setkde$START_DATE, 1, 4)
  setkde$YEAR <- factor(setkde$YEAR, levels=c("2015","2016","2017","2018","2019","2020","2021"))
  levels(setkde$settlement.year) <- list("3y before settling"="3y_prior_sett",
                                         "2y before settling"="2y_prior_sett",
                                         "1y before settling"="1y_prior_sett",
                                         "settlement year"="sett_year", 
                                         "1y after settling"="1y_after_sett",
                                         "2y after settling"="2y_after_sett",
                                         "3y after settling"="3y_after_sett")#, "<br>Color = ", color)
  setkde$julian <- julian(setkde$DATE, origin=as.Date("2020-01-01")) #as workaround for color legend
  setkde
})
list2env(res1,.GlobalEnv) #this unnlists the listed dataframes and saves them under their original names !

list.setkde_sf <- list(setkde5_sf=setkde5,setkde4_sf=setkde4,setkde3_sf=setkde3)
res2 <- lapply(list.setkde_sf, function(setkde) {
  setkde_sf <- setkde %>% 
    st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
    st_set_crs(4326) #this uses WGS84 projected (expected by leaflet)
  setkde_sf
})
setkde_sf <- do.call(rbind, res2)
setkde_sf$window.length <- lapply(strsplit(row.names(setkde_sf), "\\."), '[[', 1) %>% substr(., 7,7)


# dataset: setkde.all ----------------------------------------------------
list.setkde.all <- list(setkde5.all=setkde5.all, setkde4.all=setkde4.all, setkde3.all=setkde3.all)
res.all1 <- lapply(list.setkde.all, function(setkde.all) {
  setkde.all$START_DATE <- as.Date(setkde.all$START_DATE, format="%Y-%m-%d")
  setkde.all$DATE <- as.Date(paste0("2020-", substr(setkde.all$START_DATE, 6, 10)))  # reformat x axis display to include 29th February
  setkde.all$YEAR <- substr(setkde.all$START_DATE, 1, 4)
  setkde.all$YEAR <- factor(setkde.all$YEAR, levels=c("2015","2016","2017","2018","2019","2020","2021"))
  levels(setkde.all$settlement.year) <- list("5y before settling"="5y_prior_sett",
                                             "4y before settling"="4y_prior_sett",
                                             "3y before settling"="3y_prior_sett",
                                             "2y before settling"="2y_prior_sett",
                                             "1y before settling"="1y_prior_sett",
                                             "settlement year"="sett_year", 
                                             "1y after settling"="1y_after_sett",
                                             "2y after settling"="2y_after_sett",
                                             "3y after settling"="3y_after_sett")
  setkde.all$julian <- julian(setkde.all$DATE, origin=as.Date("2020-01-01")) #as workaround for color legend
  setkde.all
})
list2env(res.all1,.GlobalEnv) #this unnlists the listed dataframes and saves them under their original names !

list.setkde.all_sf <- list(setkde5.all_sf=setkde5.all, setkde4.all_sf=setkde4.all, setkde3.all_sf=setkde3.all)
res.all2 <- lapply(list.setkde.all_sf, function(setkde.all) {
  setkde.all_sf <- setkde.all %>% 
    st_as_sf(coords=c("CENTER.LON_X","CENTER.LAT_Y")) %>% 
    st_set_crs(4326) #this uses WGS84 projected (expected by leaflet)
  setkde.all_sf
})
setkde.all_sf <- do.call(rbind, res.all2)
setkde.all_sf$window.length <- lapply(strsplit(row.names(setkde.all_sf), "\\."), '[[', 1) %>% substr(., 7,7)


#dataset: settlers.nest ----------------------------------------------------
settlers.nest$settlement.year <- factor(settlers.nest$settlement.year, levels=c("settlement year","1y after settling","2y after settling","3y after settling"))
settlers.nest_sf <- settlers.nest %>% 
  st_as_sf(coords=c("nest.x","nest.y")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)


#dataset: locations of Settlement + Potatoes ---------------------------------------
locations$type <- sub("\\_.*", "", locations$Type) %>% as.factor
locations$cutoff <- str_sub(locations$Type,-1,-1) %>% as.numeric
settlement_loc <- locations %>% filter(!is.na(LON)) %>%
  arrange(TransmGSM, desc(cutoff)) %>%
  st_as_sf(coords=c("LON","LAT")) %>% 
  st_set_crs(4326) #this uses WGS84 (expected by leaflet)
# for leaflet plot 2; the visualisation of all non-Spain/France-wintering, non-migration GPS points


# also activate ----------------------------------------------------
radius = 1500     #1.5 km radius around nest
make.visible = 2  #value to add to size of the points acc. to KDE size, but 1km2 would be too small to see well, so all values get +2


# function to display logarithmic scales with necessary decimal places only and with comma as thousands separator
plain <- function(x,...) format(x, ..., scientific = FALSE, drop0trailing = TRUE, big.mark=",")



########### 1 - user interface ###########

ui <- fluidPage(
  navbarPage(
    #tabsetPanel(
    "5 day-window Kernel home ranges",
    # Two tabs, one for a specific individual, the other as overview of home range development for all kites
    tabPanel("Individuals",
             # App title
             #titlePanel(""),
             
             fluidRow(
               # Build selection tool for changing the individual to be displayed
               column(5,
                      wellPanel(
                        selectInput(inputId = "ID", label = "Red Kite", choices = unique(settlers$TransmGSM),
                                    selected = "KISW01", multiple = F))
               ),
               # Change amount of data displayed
               column(3,
                      radioButtons(inputId = "AmountOfData",
                                   label = "Data",
                                   choices = list("Relevant years" = 1,
                                                  "All years" = 2),
                                   selected = 1)
               ),
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
                      # Change the moving window length
                      wellPanel(
                        h5("Outline migration dates"),
                        checkboxInput(inputId = "checkbox", 
                                      label = HTML("grey contour = on migration,<br />black outline = departure/arrival day with sufficient data"), 
                                      value=TRUE)
                      ),
                      # show migration dates
                      wellPanel(
                        radioButtons(inputId = "MovingWindowLength",
                                     label = "Moving Window Length",
                                     choices = list("3 days" = 1,
                                                    "4 days" = 2,
                                                    "5 days" = 3),
                                     selected = 3)
                      )
               ),
               column(9,
                      plotOutput(outputId = "plot.Individual",
                                 width = "auto", height = "350", 
                                 click = "plot_click_Ind",
                                 brush = "plot_brush_Ind")
               )
             ),
             # Show data of points when clicking on them
             fluidRow(
               column(3, offset=0,
                      htmlOutput("warning")
               ),
               column(9, offset=0,
                      verbatimTextOutput("plot.Ind.click_brush")
               )
             ),
             
             # Output for selected individuals: Plotting home range centers on a map
             # Plots with overview graph left and zoom-graph right
             fluidRow(
               column(width = 12, #class = "well",
                      h3("Map of home ranges below threshold"),
                      leafletOutput("zoomplot", height = 550)
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


############### 2 - server ###############

server <- function(input, output, session){
  dataInput <- reactive({input$AmountOfData})
  WindowLengthInput <- reactive({input$MovingWindowLength})
  ### Panel 1 ----------------------------------------------------------------------------- ###
  
  # Depending on the type of data chosen, 
  # create a subset of data filtering for chosen bird.ID level(s)
  sub_setkde <- reactive({
    if(dataInput()==1){
      if(WindowLengthInput()==3) {setkde5[setkde5$ID == input$ID,]}
      else if (WindowLengthInput()==2) {setkde4[setkde4$ID == input$ID,]}
      else if (WindowLengthInput()==1) {setkde3[setkde3$ID == input$ID,]}
    }
    else if (dataInput()==2){
      if(WindowLengthInput()==3) {setkde5.all[setkde5.all$ID == input$ID,]}
      else if (WindowLengthInput()==2) {setkde4.all[setkde4.all$ID == input$ID,]}
      else if (WindowLengthInput()==1) {setkde3.all[setkde3.all$ID == input$ID,]}
    }
  })
  
  # different KDE cutoff for red kite breeding in Czech Republic (still activated for All kites plot)
  offset <- reactive({
    if (input$cutoff >15) {6} else {1.2}
  }) #this regulates how far away the text annotation is away from the geom_line
  
  # insert warning if necessary to observe 2021, or if settlement debatable
  settlerinfo <- reactive({
    settlers[settlers$TransmGSM == input$ID,]
  })
  
  ### --- Home Range during the year --- ###
  # Plot of one tagged inidivual; Date vs. Home Range
  output$plot.Individual <- renderPlot({
    session$resetBrush("plot_brush_Ind") #to avoid that the blue box of brushed points stays when changing plots
    
    p2 <- sub_setkde() %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      ylab(expression(Kernel~home~range~(km^2))) +
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      xlab(NULL) +
      ggtitle(paste0(input$ID)) + 
      annotation_logticks(side="l", size=0.4, colour="black") +
      theme(plot.title = element_text(hjust = 0.5),
            panel.border=element_blank(), axis.line=element_blank(),
            legend.position = "bottom", legend.key.width=unit(1.5,"cm"), legend.box = "vertical",
            axis.text.x  = element_text(hjust=-0.5)) +
      scale_shape_manual("", values = c("2021"=18, "2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2021"="longdash", "2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    #for showing migration dates as grey/black outlines
    if(input$checkbox==TRUE){
      p2 <- p2 +
        # function to use in ggplot for subsetting specific data
        #pick <- function(condition){ function(d) d %>% filter(!!enquo(condition)) }; geom_point(data=pick(on.migration=="yes"), col="grey", cex=0.95) +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(sub_setkde()$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    p2.5 <- p2 +
      geom_point(aes(col=settlement.year,
                     shape=YEAR), cex=0.9) +
      scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                         "4y before settling"="#9900CC",
                                         "3y before settling"="#440154FF", 
                                         "2y before settling"="#39568CFF", 
                                         "1y before settling"="#1F968BFF", 
                                         "settlement year"="#73D055FF", 
                                         "1y after settling"="#FDE725FF", 
                                         "2y after settling"="orange",
                                         "3y after settling"="chocolate"),
                          guide = guide_legend(override.aes = list(size=2)))    
    p2.5
  })
  
  output$warning <- renderText({
    if (settlerinfo()$status=="observe") {
      return(paste("<span style=\"color:red\">Observe carefully in 2021.</span>"))
    } else if (settlerinfo()$status=="debatable_settlement") {
      return(paste("<span style=\"color:red\">Settlement debatable,<br>use it for now.</span>"))
    }
  })
  
  # Find date and home range when clicking on/hovering over/brushing at points in plots
  output$plot.Ind.click_brush <- renderPrint({
    click <- nearPoints(sub_setkde(), input$plot_click_Ind, threshold=2, addDist = F) #showing points near click
    brush <- brushedPoints(sub_setkde(), input$plot_brush_Ind) #showing points near brush
    if(nrow(click)>0) {
      click <- click[order(click[,2] ), ] #order by START_DATE
      click[,c(2:4,6:7,11)] #keep only interesting rows, and only rows without factors
    } 
    else if(nrow(brush)>0) {
      brush <- brush[order(brush[,2] ), ] #order by START_DATE
      brush1 <- brush[, c(2:4,6:7,11)] #keep only interesting rows (for printing all rows in brush, not just min-max)
      row.names(brush1) <- c(1:nrow(brush))
      brush2 <- brush[, c(2:4,6:7)] #keep only interesting rows, and only rows without factors because of Min Max function
      Min <- summarize_all(brush2, min); row.names(Min) <- "min"
      Max <- summarize_all(brush2, max); row.names(Max) <- "max"
      minmax <- rbind(Min, Max)
      if(nrow(brush)>2) print(minmax)
      if(nrow(brush)<21) print(brush1)
    } 
    else {cat("Click on, or brush over the plot.\nBrushed points will be highlighted in the map below.")}
  })
  
  
  
  ### --- Home range centers below threshold on a map --- ###
  
  # Create a subset of data filtering for chosen bird.ID level(s) for this type of data
  # if necessary adjust country-specific kde.cutoff (in server function, first command)
  sub_setkde_cutoff <- reactive({
    if(dataInput()==1){
      setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff,]
    } else if (dataInput()==2){
      setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$AREA_KM2 <= input$cutoff,]    }
  }) #this is class "sf", it includes spatial WGS84 coordinates 
  
  # find nest coordinates for each kite
  sub_setkde_nest.set <- reactive({
    if(dataInput()==1){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID & settlers.nest_sf$settlement.year == "settlement year",]
    } else if (dataInput()==2){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID & settlers.nest_sf$settlement.year == "settlement year",]
    }
  }) # Nests of only settlement years
  sub_setkde_nest.all <- reactive({
    if(dataInput()==1){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID,]
    } else if (dataInput()==2){
      settlers.nest_sf[settlers.nest_sf$TransmGSM == input$ID,]
    }
  }) # Nests for all years
  
  # for migration dates only (to use in leaflet plot for grey borders to indicate migration)
  sub_setkde_cutoff.m <- reactive({
    if(dataInput()==1){
      setkde_sf[setkde_sf$ID == input$ID & setkde_sf$AREA_KM2 <= input$cutoff & setkde_sf$on.migration=="yes",]
    } else if (dataInput()==2){
      setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$AREA_KM2 <= input$cutoff & setkde.all_sf$on.migration=="yes",]
    }
  })
  
  # for settlement & potato centers
  sub.sepo_loc <- reactive({
    settlement_loc[settlement_loc$TransmGSM == input$ID,]
  })
  
  # determining subset based on window length 
  windowlength <- reactive ({
    if(input$MovingWindowLength == 1) {3}
    else if(input$MovingWindowLength == 2) {4}
    else if(input$MovingWindowLength == 3) {5}
  })
  
  # highlight brushed points of ggplot above in the leaflet plot below
  activePoint <- reactiveVal()   # Create a reactive value to store the point we select
  observeEvent(input$plot_brush_Ind, {
    if(length(input$plot_brush_Ind)>0){
      near_Points <- brushedPoints(sub_setkde(), input$plot_brush_Ind)
      activePoint(as.Date(near_Points[,2])) # Extract just the start date of point and assign it to activePoint()
    }
  })   # Update the value of activePoint() when we detect an input$plotClick event
  highlightData <- reactive({
    setkde.all_sf[setkde.all_sf$ID == input$ID & setkde.all_sf$window.length == windowlength() & setkde.all_sf$START_DATE %in% activePoint(),]
  })  # Use that data in the leaflet plot
  
  # plot of KDE centers with area below cutoff (km2) on zoomable map
  output$zoomplot <- renderLeaflet({
    
    ### make colour palette for Date and Year -----------------------------------------------
    pal.date <- colorNumeric(palette = viridis(200), domain = c(0,366), reverse=T) #color legend for date (workaround with julian dates to have numeric values)
    pal.nest <- colorFactor(palette = c('#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
    pal.sepo <- colorFactor(palette = c("cyan", "#CC0066", "blue", "orange"), domain = NULL)
    # !!! order has to match level order of settlement.year and settlers.nest !!! #
    
    if(dataInput()==1){ # display only relevant years
      ### make colour palette for Date and Year -----------------------------------------------
      pal.year <- colorFactor(palette = c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
      
      ### legend for settlement.year, on.migration and nest location ### ----------------------
      colors <- c(rep("white",7),"red","white","blue","cyan","orange","#CC0066") #fill inside borders of circles
      labels <- c("-3 years", "-2 years", "-1 year", "settlement", "+1 year", "+2 years", "+3 years",
                  "nest", "on migration", "settlement", "last potato", "'spring' potato", "longest potato")
      sizes <- c(rep(12,7),2,12,rep(2,4))
      shapes <- "circle"
      margin.top <- c(rep(5,7),8,5,rep(8,4)) #margin from circles to each other, distance from top
      margin.left <- c(rep(0,7),2,0,rep(2,4)) #margin from circles to left side (to center the "nest" circle)
      borders <- c('#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate',
                   'red', 'gray', 'blue','cyan','orange','#CC0066')
    } 
    else { # display all years
      #  ### make colour palette for Date and Year -----------------------------------------------
      pal.year <- colorFactor(palette = c('#CC00CC', '#9900CC', '#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate'), domain = NULL)
      
      ### legend for settlement.year, on.migration and nest location ### ----------------------
      colors <- c(rep("white",9),"red","white","blue","cyan","orange","#CC0066") #fill inside borders of circles
      labels <- c("-5 years", "-4 years", "-3 years", "-2 years", "-1 year", "settlement", "+1 year", "+2 years", "+3 years",
                  "nest", "on migration", "settlement", "last potato", "'spring' potato", "longest potato")
      sizes <- c(rep(12,9),2,12,rep(2,4))
      shapes <- "circle"
      margin.top <- c(rep(5,9),8,5,rep(8,4)) #margin from circles to each other, distance from top
      margin.left <- c(rep(0,9),2,0,rep(2,4)) #margin from circles to left side (to center the "nest" circle)
      borders <- c('#CC00CC', '#9900CC', '#440154FF', '#39568CFF', '#1F968BFF', '#73D055FF', '#FDE725FF', 'orange', 'chocolate',
                   'red', 'gray', 'blue', 'cyan',' orange', '#CC0066')
    }
    
    ### legend for settlement.year, on.migration and nest location ### ----------------------
    addLegendCustom <- function(map, labels, sizes, shapes, borders, opacity = 1, 
                                position = c("topright", "bottomright", "bottomleft", "topleft")){
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
    
    
    ### legend for KDE size  ### ------------------------------------------------------------
    colors.Size <- "black" #fill inside borders of circles
    labels.Size <- c("1","6") #labelling for km2 home size
    sizes.Size <- c(0.5+make.visible,11+make.visible) #value to make small KDE areas (points) better visible
    margin.top.Size <- c(7,3) #margin from circles to each other, distance from top
    margin.left.Size <- c(3.5,0) #margin from circles to left side (to center the "nest" circle)
    borders.Size <- "black"
    addLegendCustom.Size <- function(map, labels.Size, sizes.Size, shapes, 
                                     borders.Size, opacity = 1, 
                                     position = c("topright", "bottomright", "bottomleft", "topleft")){
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
    
    
    ### legend for Date coloration ### ------------------------------------------------------
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
    
    
    l1 <- leaflet(options = leafletOptions(zoomControl = FALSE)) %>% #changes position of zoom symbol
      htmlwidgets::onRender("function(el, x) {
        L.control.zoom({ position: 'topright' }).addTo(this)
        }"
      ) %>%
      addProviderTiles(providers$OpenTopoMap,
                       options = providerTileOptions(opacity = 0.7) #Esri.WorldTopoMap #Stamen.Terrain #OpenTopoMap #Esri.WorldImagery
      ) %>%  
      addMapPane("nest", zIndex = 430 #puts nest on near-top of all layers
      ) %>%  
      addMapPane("settlements", zIndex = 431 #puts settlements on top of all layers
      ) %>%  
      ## -- SETTLEMENT CENTERS -- ##
      addCircleMarkers(
        data=sub.sepo_loc(),
        radius = ~(cutoff/2),
        color = ~pal.sepo(type),
        group = "settlement and potatoes",
        stroke = FALSE, fillOpacity = ~7/cutoff,
        options = pathOptions(pane = "settlements"),
        popup = ~ paste0(type," potato<br>cutoff: ", cutoff, " km<sup>2</sup><br>", Start," - ", End)
      ) %>% 
      addCircles(
        data=sub.sepo_loc(),
        radius = 1500,
        color = "black",
        group = "settlement and potatoes",
        weight=1,
        stroke = TRUE, fill=F,
        options = pathOptions(pane = "settlements")
      ) %>% 
      ## -- KDE CENTERS -- ##
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==3), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "3 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==4), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "4 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      addCircleMarkers( #date colour-coded centers of home ranges
        data=subset(sub_setkde_cutoff(), window.length==5), #lng=~CENTER.LON_X, lat=~CENTER.LAT_Y
        group = "5 day-window",
        clusterOptions = markerClusterOptions(maxClusterRadius = 10,
                                              disableClusteringAtZoom=11),
        radius = ~AREA_KM2+make.visible,
        color = ~pal.year(settlement.year),  #border: year
        fillColor = ~pal.date(julian), fillOpacity = 1,         #inside: date in the year
        stroke = TRUE, opacity = 1, weight = ~(0.05*AREA_KM2+1.5),
        popup = ~ paste0(START_DATE,"<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                         settlement.year,"<br>GPS points: ", NR_POINTS)
      ) %>%
      ## -- MIGRATION points underneath KDE CENTERS -- ##
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==3),
        group = "3 day-window",
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==4),
        group = "4 day-window",
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      addCircleMarkers( #indicator for migration dates (#grey circles)
        data=subset(sub_setkde_cutoff.m(), window.length==5),
        group = "5 day-window",
        radius = ~(1.05*AREA_KM2+3),
        color = "gray",
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      ## -- NEST points -- ##
      #addCircleMarkers( #add nest for when only nest of settlement year should be displayed (as controlled by addLayersControl)
      #  data=sub_setkde_nest.set(), #lng=~nest.x, lat=~nest.y,
      #  radius = 3,
      #  group = "nest of settlement year",
      #  color = "red",
      #  stroke = FALSE, fillOpacity = 1,
      #  options = pathOptions(pane = "nest"),
      #  popup = ~ paste0("nest: ",settlement.year,"<br>",YEAR)
      #)  %>% 
      addCircleMarkers( #add nests for when nests of all years should be displayed (as controlled by addLayersControl)
        data=sub_setkde_nest.all(),
        radius = 3,
        group = "nests of all years",
        color = "red",
        stroke = FALSE, fillOpacity = 1,
        options = pathOptions(pane = "nest"),
        popup = ~ paste0("nest: ",settlement.yearsPL,"<br>",YEARsPL)
      )  %>% 
      addCircleMarkers( # coloured border around point as indicator for year 
        data=sub_setkde_nest.all(),
        radius = 3,
        group = "nests of all years",
        color = ~pal.nest(settlement.year),
        stroke = T, fill = F,
        opacity = 1
      ) %>%
      ## -- CONTROLS -- ##
      addLayersControl( #control to define which nests are displayed
        baseGroups = c("3 day-window", "4 day-window", "5 day-window"),
        overlayGroups = c("home range points", "settlement and potatoes"), #control to define if settlements & potoatoes are displayed
        options = layersControlOptions(collapsed = F),
        position = "topleft"
      ) %>% 
      addScaleBar(position = "bottomright",
                  options = scaleBarOptions(imperial = F)) %>% 
      addLegendCustom(labels, sizes, shapes, borders, position="topleft") %>% # legend for settlement.year, on.migration and nest location
      addLegendCustom.Size(labels.Size, sizes.Size, shapes, borders.Size, position="topleft") %>%  # legend for point size (AREA_KM2)
      addLegend(     # legend for date (viridis scale)
        data = sub_setkde_cutoff(),
        position = "bottomright", 
        pal = pal.date,
        values = ~julian,
        opacity = 1,
        bins=6,
        labFormat = myLabelFormat(dates=T),
        title = NULL
      ) %>% 
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "kilometers"
      )
    
    if (length(input$plot_brush_Ind) > 0) { #points selected: highlight them in this leaflet plot
      l1 %>% 
        addCircleMarkers( #highlight the points selected in first graph
          data=highlightData(),
          radius = ~AREA_KM2+make.visible+7,
          fillColor = "yellow",
          fillOpacity = 0.75,         
          stroke = F,
          popup = ~ paste0(START_DATE, "<br>home range: ", AREA_KM2, " km<sup>2</sup><br>",
                           settlement.year,"<br>GPS points: ", NR_POINTS)
        )
    } else {l1} # No points selected; regular plot
  })
  

  
  
  ### Panel 2 ----------------------------------------------------------------------------- ###
  # Plot for all Individuals
  data.plot.All <- reactive({
    if(dataInput()==1) {
      if(WindowLengthInput()==1) {setkde5}
      else if (WindowLengthInput()==2) {setkde4}
      else if (WindowLengthInput()==3) {setkde3}
    }
    else if (dataInput()==2) {
      if(WindowLengthInput()==1) {setkde5.all}
      else if (WindowLengthInput()==2) {setkde4.all}
      else if (WindowLengthInput()==3) {setkde3.all}
    }
  })
  
  output$plot.All <- renderPlot({
    
    p5 <- data.plot.All() %>% ggplot(aes(x=DATE, y=AREA_KM2)) +
      geom_hline(yintercept=input$cutoff, size=0.1) +
      scale_y_continuous(trans='log10', breaks=c(0.1, 1, 100, 10000), labels = plain, limits = c(0.009, 1500000)) +
      ylab(expression(Kernel~home~range~(km^2))) +
      annotate(geom="text", label=paste0(input$cutoff, "~km^2"), parse=T, x=as.Date("01-05", format="%m-%d"),
               y=input$cutoff-offset(), hjust=1, size=1.5, col="darkgrey") +
      geom_line(aes(linetype=YEAR), col="grey", size=0.4) + 
      scale_x_date(breaks = c(as.Date("2020-01-01"),as.Date("2020-02-01"),as.Date("2020-03-01"),as.Date("2020-04-01"),
                              as.Date("2020-05-01"),as.Date("2020-06-01"),as.Date("2020-07-01"),as.Date("2020-08-01"),
                              as.Date("2020-09-01"),as.Date("2020-10-01"),as.Date("2020-11-01"),as.Date("2020-12-01")),
                   date_labels = "%b") +
      theme_classic() +
      xlab(NULL) +
      annotation_logticks(side="l", size=0.4, colour="black") +
      facet_rep_wrap(~ID, scales="fixed", ncol=4, nrow=20) +
      theme(panel.border=element_blank(), axis.line=element_blank(),
            #axis.text.x  = element_text(hjust=-0.5),
            legend.position = "bottom", legend.key.width=unit(1.5,"cm"), legend.box = "vertical") +
      scale_shape_manual("", values = c("2021"=18, "2020"=16, "2019"=17, "2018"=15, "2017"=18, "2016"=16, "2015"=17)) +
      scale_linetype_manual("", values = c("2021"="longdash", "2020"="solid", "2019"="dashed", "2018"="dotted", "2017"="dotdash", "2016"="longdash", "2015"="dotted"))
    
    
    if(input$checkbox==TRUE){
      p5 <- p5 +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$on.migration=="yes",2.5,0.05),
                   col="grey") +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$departure.day=="yes",1.25,0.05),
                   col="black") +
        geom_point(aes(shape=YEAR), cex=ifelse(data.plot.All()$arrival.day=="yes",1.25,0.05),
                   col="black")
    }
    
    if(input$datasecurity==TRUE){
      if(input$colouration.All=="Longitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=4, data=subset(data.plot.All(), NR_POINTS<20)) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LON_X), max(data.plot.All()$CENTER.LON_X))) +
          labs(col = "Longitude")
      }
      else if(input$colouration.All == "Latitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.5, alpha=0.2) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=4, data=subset(data.plot.All(), NR_POINTS<20)) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LAT_Y), max(data.plot.All()$CENTER.LAT_Y))) +
          labs(col = "Latitude")
      }
      else if(input$colouration.All == "Settlement Year"){
        p5.5 <- p5 +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=0.5, alpha=0.2) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=1.4, data=subset(data.plot.All(), NR_POINTS<30), alpha=0.7) +
          geom_point(aes(col=settlement.year, shape=YEAR), 
                     cex=4, data=subset(data.plot.All(), NR_POINTS<20))
        
        if(dataInput()==1){
          p5.5 <- p5.5 +
            scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        } 
        else {
          p5.5 <- p5.5 +
            scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                               "4y before settling"="#9900CC",
                                               "3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        }
        
      }
    } 
    else if (input$datasecurity==FALSE) {
      if(input$colouration.All=="Longitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LON_X, shape=YEAR), cex=0.9, alpha=0.9) +
          #scale_size(range=c(3,0.5), trans="log") +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LON_X), max(data.plot.All()$CENTER.LON_X))) +
          labs(col = "Longitude")
      }
      else if(input$colouration.All == "Latitude"){
        p5.5 <- p5 +
          geom_point(aes(col=CENTER.LAT_Y, shape=YEAR), cex=0.9) +
          viridis::scale_color_viridis(limits=c(min(data.plot.All()$CENTER.LAT_Y), max(data.plot.All()$CENTER.LAT_Y))) +
          labs(col = "Latitude")
      }
      else if(input$colouration.All == "Settlement Year"){
        if(dataInput()==1){
          p5.5 <- p5 +
            geom_point(aes(col=settlement.year,
                           shape=YEAR), cex=0.9) +
            scale_colour_manual("", values = c("3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        } 
        else {
          p5.5 <- p5 +
            geom_point(aes(col=settlement.year,
                           shape=YEAR), cex=0.9) +
            scale_colour_manual("", values = c("5y before settling"="#CC00CC",
                                               "4y before settling"="#9900CC",
                                               "3y before settling"="#440154FF", 
                                               "2y before settling"="#39568CFF", 
                                               "1y before settling"="#1F968BFF", 
                                               "settlement year"="#73D055FF", 
                                               "1y after settling"="#FDE725FF", 
                                               "2y after settling"="orange",
                                               "3y after settling"="chocolate")) +
            guides(color = guide_legend(nrow = 1, override.aes = list(size=0.9)), 
                   shape = guide_legend(override.aes = list(size=0.9)))
        }
      }
    }
    
    p5.5
    
  })
}


############### 3 - start shinyApp ##############

shinyApp(ui = ui, server = server)

