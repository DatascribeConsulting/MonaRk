# LIBRARIES ---------------------------------------------------------------
## Don't need these for package, but need to keep them to host on shinyapps.io
library(shiny)
library(shinydashboard)
library(data.table)
library(DT)
library(plotly)
library(ggplot2)
library(leaflet)
library(sf)
library(dplyr)
library(lwgeom)
library(svglite)

# External functions ------------------------------------------------------
## Don't need these for package, but need to keep them to host on shinyapps.io
source('R/AppCreateFlightgeo.R')
source('R/AppUtils.R')


# SERVER ------------------------------------------------------------------

server <- function(input, output, session) {

  # CSS to color header of route table
  jsSettingsDT <- "$(this.api().table().header()).css({'background-color': '#bcc0c4', 'color': '#00264D', 'font-size': '13px'});"
  
  # read in route summary data - show on first tab to select route
  routes <- reactive({
    return(get(load("appData/routes.RDS")))
  })
  
  # read in sigmet data
  allsigs <- reactive({
    return(get(load("appData/allsigs.RDS")))
  })
  
  # placeholders for basic data
  selRoutes <- reactiveVal(value = NULL)      # character vector of routes user selects
  selFlights <- reactiveVal(value = NULL)     # flight data for selected routes
  selFlightsSub <- reactiveVal(value = NULL)  # flight data for subset of paths showing on maps 
  geoFull <- reactiveVal(value = NULL)        # geo data for selFlights
  startDate <- reactiveVal(value = NULL)      # earliest flight date
  endDate <- reactiveVal(value = NULL)        # latest flight date
  
  # placeholders for interactive click data
  mapClickDF <- reactiveVal(value = NULL)     # leaflet click information, matrix with map element name and path click id
  oldMapClick <- reactiveVal(value = NULL)    # saves previous click information for comparison
  plotlyClick <- reactiveVal(value = NULL)    # plotly click information, contains curve number and key
  oldPlotlyClick <- reactiveVal(value = NULL) # saves previous click information for comparison
  oldClickInfo <- reactiveVal(value = NULL)   # saves previous click info to return color to unhighlight
  
  # tells map if it should plot first route 
  # for case that a user submits route selection, maps a route (not first), 
  # selects different route and then dropdown doesn't match map, error on click
  useRouteDropdown <- reactiveVal(value = NULL)
  
  # save plots so they can be exported
  plots <- reactiveValues()

  ########## Create Route DataTable ###########
  
  observeEvent(input$clear, {
    dataTableProxy('selectRoutes') %>% selectRows(NULL)
  })
  
  
  output$selectRoutes <- renderDataTable({
    
    routes <- routes()
    
    routes$percWeather <- round(routes$percWeather*100)
    routes$meanDur <- round(routes$meanDur)
    routes$meanMinIncrease <- round(routes$meanMinIncrease)
    routes$meanDist <- round(routes$meanDist/1000)
    routes$meanKmIncreaseFlight <- round(routes$meanKmIncreaseFlight)
    
    routes$origin <- unlist(lapply(routes$route, function(x){
      strsplit(x, split = " ")[[1]][1]
    })) 
    routes$destination <- unlist(lapply(routes$route, function(x){
      strsplit(x, split = " ")[[1]][2]
    }))
    
    if(input$airbusOnly){
      routes <- routes[which(airbusCount > 0),]
    }

    routes <- routes[ , c("origin", "destination", "continent",
                          "routeCount", "airbusCount",
                          "percThisWeather", 
                          "meanDur", "meanMinIncrease",
                          "meanDist", "meanKmIncreaseFlight",
                          "percRegionWeather",
                          "percStormEvent",
                          "percWeatherDelay", "percNasDelay"
                          )]
    names(routes) <- c("Origin", "Destination", "Continent", 
                       "No. Flights",  "No. Airbus Aircraft",
                       "Path Overlaps Weather (%)", 
                       "Ave. Duration (min)",
                       "Ave. Extra Duration (min)",
                       "Ave. Distance (km)", 
                       "Ave. Extra Distance (km)", 
                       "Weather in Region (%)", 
                       "Path Overlaps Storm Event (USA%)",
                       "Weather Delayed (USA%)", "NAS Delayed (USA%)"
    )

    datatable(routes,
              filter = list(position = 'top', clear = FALSE),
              extensions = c('Scroller'),
              selection = list(target = 'row'),
              options = list(sDom  = '<"top">lrt<"bottom">ip',
                             scrollX = TRUE, scrollY = '400px', defer = TRUE, scroller = TRUE,
                             autoWidth = TRUE,
                             columnDefs = list(list(width = '90px', targets = c(1:14))),
                             initComplete = JS("function(settings, json) {", jsSettingsDT, "}")
              )
    )
  })
  
  
  ####### Create data and static plots ########
  
  legendChoices = c("Sigmet Overlap" = "thisWeather",
                    "Sigmet in Region" = "regionWeather",
                    "Storm Events Overlap (US Only)" = "stormEvent", 
                    "Weather Delay (US Only)" = "weatherDelay",
                    "NAS Delay (US Only)" = "nasDelay")
  
  observeEvent(input$submit, {
    # error messages to make sure reasonable number of routes selected
    if (length(input$selectRoutes_rows_selected)  < 2){
      output$buttonMessage <- renderUI({
        h5("Please highlight at least 2 routes to compare", 
           style="color: red; font-style: italic; margin-left:15px;")
      })
    } else if (length(input$selectRoutes_rows_selected) > 25){
      output$buttonMessage <- renderUI({
        h5("Please select less than 25 routes", 
           style="color: red; font-style:italic; margin-left:15px;")
      })
    } 
    else {
      output$buttonMessage <- renderUI({
        h5("Success! Please explore other tabs to find plots", 
           style="font-style: italic; margin-left:15px; color:#FB9334;")
      })
      
      output$plotMessage <- renderUI({})
      output$mapsMessage <- renderUI({})
      output$costMessage <- renderUI({})
      
      # create a vector of routes that other parts of the app will use too
       if(input$airbusOnly){
        routes <- routes()[which(airbusCount > 0),]
       } else{
        routes <- routes()
      }
    
      selRoutes <- routes$route[input$selectRoutes_rows_selected]
      
      
      if(length(selRoutes) > 0){
        withProgress(message = 'Loading...', value = 0, {
          incProgress(.4, detail = "Reading Data")
          
          # read in folders of flight info selected routes
          dfs <- lapply(gsub(" ", "_", selRoutes), function(x){
            geoReadRDS <- get(load(file.path("appData/appRoutes", x, "geo.RDS")))
            flightsReadRDS <- get(load(file.path("appData/appRoutes", x, "df.RDS")))
            return(createFlightgeo(geoReadRDS, flightsReadRDS, 
                                   simplify = FALSE, drop = "velocity"))
          })
          # create one dataframe to hold flight info
          selFlights <- do.call(rbind, dfs)
          selFlights$route <- factor(selFlights$route, levels = selRoutes)
          
          # subset to Airbus flights if option selected
          if(input$airbusOnly){
            selFlights <- selFlights[which(grepl("Airbus", selFlights$manufacturer)),]
          }
          
          if(nrow(selFlights) > 0) {
            # save so other parts of app can use
            selFlights(selFlights)
            startDate(min(as.Date(selFlights$date)))
            endDate(max(as.Date(selFlights$date)))
            
            # read in complete geodata so plotly can use velocity and elevation data
            # since saving an RDS with a geo column only can hold 4 dimensions, we have 5:
            # lat, lon, time, vel, elev
            geoFull <- unlist(lapply(gsub(" ", "_", selRoutes), function(x){
              geoReadRDS <- get(load(file.path("appData/appRoutes", x, "geo.RDS")))
            }), recursive = FALSE)
            geoFull(geoFull)
          
            # after reading in complete geo data and 
            # possibily subsetting airbus only routes, save those routes
            selRoutes <- unique(selFlights$route)
            selRoutes(selRoutes)

            incProgress(.4, detail = "Formatting Data")
            ## save subset of flights to show on interactive plots
            ## since ploting routes that have 500 paths takes forever
            keepRows <- lapply(1:length(unique(selFlights$route)), function(x){
              onroute <- selFlights[which(selFlights$route == unique(selFlights$route)[x]),]
              subIndecies <- c(which.min(onroute$duration), 
                               which.min(onroute$distance),
                               order(onroute$duration, decreasing=TRUE)[1:min(30, nrow(onroute))],
                               order(onroute$distance, decreasing=TRUE)[1:min(30, nrow(onroute))]
                               )
              onrouteSub <- onroute[unique(subIndecies),]
              onrouteSub$id <- unique(subIndecies)
              return(onrouteSub)
            })
            selFlightsSub <- do.call(rbind, keepRows)
            selFlightsSub(selFlightsSub)
          
            useRouteDropdown(FALSE)
          }
        }) 
      }
    }
  })
  
  observe({
    selFlights <- selFlights()
    isolate({
      # version without geo for plotting
      selFlightsPlots <- selFlights
      selFlightsPlots$g <- NULL
      
      if(!is.null(selFlightsPlots) && nrow(selFlightsPlots) > 0) {
        # formating for colors - all variables used for colors need to have 3 levels:
        # "TRUE", "FALSE", "NA"
        if(!is.null(input$plotColorRadio) && input$plotColorRadio  == "weatherDelay" | input$plotColorRadio  == "nasDelay"){
          selFlightsPlots[which(selFlightsPlots[,input$plotColorRadio] > 0), input$plotColorRadio] <- TRUE
          selFlightsPlots[which(selFlightsPlots[,input$plotColorRadio] == 0), input$plotColorRadio] <- FALSE
        }
        
        legendLabel <- names(legendChoices)[legendChoices == input$plotColorRadio]
        
        compareRoutesPlot <- function(plotVar, axisLabel){
          ggplot(selFlightsPlots, aes(x=reorder(route, desc(route)),
                                      y=selFlightsPlots[,plotVar],
                                      color = as.character(as.logical(selFlightsPlots[,input$plotColorRadio])))) +
            geom_jitter(position=position_jitter(0.2)) +
            ylab(paste0("\n", axisLabel)) +
            xlab("Route\n") +
            scale_x_discrete(labels=setNames(paste0(selRoutes(), 
                                                    "\n(", 
                                                    lapply(selRoutes(), function(x){
                                                      routes()$continent[which(routes()$route == x)]
                                                    }),
                                                    ")"),
                                             selRoutes())) +
            theme_minimal() +
            coord_flip() +
            theme(panel.grid.major.y = element_blank(),
                  axis.text=element_text(size=12),
                  axis.title=element_text(size=14,face="bold")) +
            scale_color_manual(values=c("FALSE" = input$plotColorF, "TRUE" = input$plotColorT),
                               na.value = input$plotColorNA,
                               name = legendLabel)
        }
        
        ####### CREATE DURATION PLOTS AND TITLES #######
        output$plot1 <- renderPlot({
          plots$plot1 <- compareRoutesPlot("duration", "Path Duration (min)")
          plots$plot1 + theme(legend.position = "none")
        })
        output$title1 <- renderUI({
          h5("Path Duration by Route")
        })
        
        output$plot2 <- renderPlot({
          plots$plot2 <- compareRoutesPlot("minincrease", "Duration Increase (min)")
          plots$plot2 + theme(legend.position = "none")
        })
        output$title2 <- renderUI({
          h5("Minute Increase from Shortest Path")
        })
        
        output$plot3 <- renderPlot({
          plots$plot3 <- compareRoutesPlot("minpercincrease", "Percent Increase (%)")
          plots$plot3 + theme(legend.position = "none")
        })
        output$title3 <- renderUI({
          h5("Percent Duration Increase from Shortest Path")
        })
        
        
        ####### CREATE DISTANCE PLOTS AND TITLES #######
        output$plot4 <- renderPlot({
          selFlightsPlots$distance <- selFlightsPlots$distance/1000  # to KM
          plots$plot4 <- compareRoutesPlot("distance", "Path Distance (km)")
          plots$plot4 + theme(legend.position = "none")
        })
        output$title4 <- renderUI({
          h5("Path Distance by Route")
        })
        
        output$plot5 <- renderPlot({
          plots$plot5 <- compareRoutesPlot("kmincreaseFlight", "Distance Increase (km)")
          plots$plot5 + theme(legend.position = "none")
        })
        output$title5 <- renderUI({
          h5("Kilometer Increase from Shortest Path")
        })
        
        output$plot6 <- renderPlot({
          plots$plot6 <- compareRoutesPlot("kmpercincreaseFlight", "Percent Increase (%)")
          plots$plot6 + theme(legend.position = "none")
        })
        output$title6 <- renderUI({
          h5("Percent Distance Increase from Shortest Path")
        })
      }
    })
  })
            
  
  output$plotExp1 <- downloadHandler(
    filename = function() { paste0('duration-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot1, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp2 <- downloadHandler(
    filename = function() { paste0('extraMin-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot2, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp3 <- downloadHandler(
    filename = function() { paste0('extraMinPerc-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot3, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp4 <- downloadHandler(
    filename = function() { paste0('distance-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot4, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp5 <- downloadHandler(
    filename = function() { paste0('extraKM-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot5, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp6 <- downloadHandler(
    filename = function() { paste0('extraPerc-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot6, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )

  
  #### Messages on further tabs if no routes selected ######
  output$plotMessage <- renderUI({
    h5("Please highlight routes to compare first", style="font-style: italic; color: red;")
  })
  output$mapsMessage <- renderUI({
    h5("Please highlight routes to compare first", style="font-style: italic; color: red;")
  })
  output$costMessage <- renderUI({
    h5("Please highlight routes to compare first", style="font-style: italic; color: red;")
  })
  
  

  ##### Create maps and interactive plots #####
  output$mapRouteSelect <- renderUI({
    input$submit
    req(selRoutes <- selRoutes())
    selectInput("mapRouteDropdown", "Select Route to Map", 
                choices = selRoutes,
                selected = selRoutes[1])
  })
  
  mapRouteDropdownSelection <- reactive({
    input$mapRouteDropdown
  })
  
  ####### DRAW maps and plots #######
  observe({ 
    # triggers when a change in any of these items
    input$mapSubmit
    req(selRoutes <- selRoutes())
    req(selFlightsSub <- selFlightsSub())
    req(geoFull <- geoFull())
    flightCost()
      
    output$mapSubsetMessage <- renderUI({
      h5("Due to data size restrictions, 
         only showing shortest routes (one by duration, one by distance), 
         and longest routes (30 by duration, 30 by distance)", 
         style="font-style: italic;")
    })
    
    isolate({
       if(!is.null(mapRouteDropdownSelection()) && 
          !is.na(mapRouteDropdownSelection()) && 
          useRouteDropdown() &&
          mapRouteDropdownSelection() %in% selRoutes){
         route <- mapRouteDropdownSelection()
       } else {
         route <- selRoutes[1]
       }

        # focus on one route's data at a time
        selFlight <- selFlightsSub[which(selFlightsSub$route == route),]
        # filter routes by date
        # if(is.null(input$costByDate)){
        #   costByDate <- c(min(selFlight$date), max(selFlight$date))
        # } else {
        #   costByDate <- input$costByDate
        # }
        # selFlight <- selFlight[which(selFlight$date >= costByDate[1] &
        #                                 selFlight$date <= costByDate[2]),]
    
        # add cost data which may depend on input from last tab
        if(!is.null(flightCost()) && input$airbusOnly){
          routeCost <- flightCost()[which(selFlights()$route == route)]
          selFlight$cost <- routeCost[as.numeric(selFlight$id)]  
        } else if(!is.null(flightCost()) && input$airbusOnly == FALSE){
          selFlight$cost <- flightCost()[as.numeric(row.names(selFlight))]  
          #selFlight$cost <- flightCost()[selFlight$id2]  
        } else {
          selFlight$cost <- selFlight$minincrease * 2500/60
        }
        
        #route as title to separate rows of plots
        output$titlename <- renderUI({
          h4(as.character(selFlight$route[1]))
        })

        output$mapname <- renderLeaflet({
          # what user sees on map click
          if(input$costBy == "Distance"){
            states_popup <- paste0("Extra Distance Cost: $", round(selFlight$cost),
                                   "<br>Date: ", selFlight$date,
                                   "<br>Airline: ", selFlight$airline,
                                   "<br>Manufacturer: ", selFlight$manufacturer)
          } else {
            states_popup <- paste0("Extra Duration Cost: $", round(selFlight$cost),
                                   "<br>Date: ", selFlight$date,
                                   "<br>Airline: ", selFlight$airline,
                                   "<br>Manufacturer: ", selFlight$manufacturer)
          } 

          # color formatting
          selFlight$weatherDelay <- selFlight$weatherDelay > 0
          selFlight$nasDelay <- selFlight$nasDelay > 0
          factpal <- colorFactor(c(isolate({input$mapColorF}), 
                                   isolate({input$mapColorT})), 
                                 c(FALSE, TRUE), 
                                 na.color = isolate({input$mapColorNA}),
                                 alpha = T)  
          
          # draw map
          plots$map <- leaflet() %>% 
            addProviderTiles("CartoDB.Positron", group = "Carto") %>%
            addPolylines(data = st_zm(selFlight), 
                         color = ~factpal(as.character(get(isolate({input$mapColorRadio})))), 
                         weight = 1,
                         popup = states_popup, 
                         #label = ~id,
                         layerId = ~id,
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"),
                         highlightOptions = highlightOptions(
                           color = isolate({input$mapColorHighlight}), opacity = 1, weight = 2, fillOpacity = 1,
                           bringToFront = TRUE, sendToBack = TRUE)
                         ) 
          
          legendLabel = names(legendChoices)[legendChoices == isolate({input$mapColorRadio})]
          plots$mapLegend <- plots$map  %>%
            addLegend("bottomright", 
                      colors = c(isolate({input$mapColorT}), isolate({input$mapColorF}), isolate({input$mapColorNA})),
                      labels = c("TRUE", "FALSE", "NA"),
                      title = legendLabel,
                      opacity = 1
            )
          
          plots$map 
        }) 

        ##### PLOTLY FORMATTING #####
        # function from AppUtils.R to create dataframe
        # containing location and velocity info, every timepoint a row
        inflight <- makeInFlightDf(selRoutes[1], selFlight, geoFull)
        
        # convert TF's to 3 categories: N, Unknown, Y 
        inflight[, isolate({input$mapColorRadio})][which(inflight[, isolate({input$mapColorRadio})] == FALSE)] <- "No"
        inflight[, isolate({input$mapColorRadio})][which(is.na(inflight[, isolate({input$mapColorRadio})]))] <- "Unknown"
        inflight[, isolate({input$mapColorRadio})][which(inflight[, isolate({input$mapColorRadio})] == TRUE)] <- "Yes"
        
        
        # styles
        colorList <- c("No" =      isolate({input$mapColorF}),       
                       "Unknown" = isolate({input$mapColorNA}),  
                       "Yes" =     isolate({input$mapColorT}))   
        x <- list(title = "<b>Minutes From Start</b>")
        elevlab <- list(title = "<b>Elevation (m)<b>")
        speedlab <- list(title = "<b>Velocity (m/s)<b>")

        # popup text
        if(input$costBy == "Distance"){
          plotText <- paste0("Extra Distance Cost: $", round(inflight[inflight$elev < 20000,]$cost),
                             "<br>Date: ", inflight[inflight$elev < 20000,]$date,
                             "<br>Airline: ", inflight[inflight$elev < 20000,]$airline,
                             "<br>Manufacturer: ", inflight[inflight$elev < 20000,]$manufacturer)
        } else {
          plotText <- paste0("Extra Duration Cost: $", round(inflight[inflight$elev < 20000,]$cost),
                             "<br>Date: ", inflight[inflight$elev < 20000,]$date,
                             "<br>Airline: ", inflight[inflight$elev < 20000,]$airline,
                             "<br>Manufacturer: ", inflight[inflight$elev < 20000,]$manufacturer)
        }

        output$elevplotname <- renderPlotly({
          fig <- inflight[inflight$elev < 20000,] %>%  # subset since some false readings, distracting
            group_by(id) %>%                           # so path is connected
            plot_ly( x = ~minfromstart, y = ~elev, 
                     color = ~as.factor(get(  isolate({input$mapColorRadio}))),
                     colors = colorList, 
                     ids = inflight[inflight$elev < 20000,]$id,
                     key = inflight[inflight$elev < 20000,]$id,
                     text = plotText,
                     hoverinfo = 'text',
                     source= "elevplotname") %>%
            config(displaylogo = FALSE,
              modeBarButtonsToRemove = c("autoScale2d",
                                         "toggleSpikelines", 
                                         "hoverClosestCartesian",
                                         "hoverCompareCartesian"),
              toImageButtonOptions = list(format = "svg",
                                          filename = paste0('inflightElev-plot_', Sys.time()),
                                          width = 1000,
                                          height = 600
            )
          )
          fig <- fig %>% add_trace(type = 'scatter', mode = 'lines',
                                   ids = paste("line", inflight[inflight$elev < 20000,]$id),
                                   name = paste("line",inflight[inflight$elev < 20000,]$id)) %>% 
            layout(showlegend = FALSE, xaxis = x, yaxis = elevlab)
          plots$plotly1 <- fig
          return(fig)
        })
        
        output$velplotname <- renderPlotly({
          fig <- inflight[inflight$elev < 20000,] %>% 
            group_by(id) %>% 
            plot_ly( x = ~minfromstart, y = ~speed, 
                     color = ~as.factor(get(  isolate({input$mapColorRadio}))),
                     colors = colorList,
                     ids = inflight[inflight$elev < 20000,]$id,
                     key = inflight[inflight$elev < 20000,]$id,
                     text = plotText,
                     hoverinfo = 'text',
                     source= "velplotname") %>%
            config(displaylogo = FALSE,
                   modeBarButtonsToRemove = c("autoScale2d",
                                              "toggleSpikelines", 
                                              "hoverClosestCartesian",
                                              "hoverCompareCartesian"),
                   toImageButtonOptions = list(format = "svg",
                                               filename = paste0('inflightVel-plot_', Sys.time()),
                                               width = 1000,
                                               height = 600
                   )
            )
          fig <- fig %>% add_trace(type = 'scatter', mode = 'lines',
                                   ids = paste("line", inflight[inflight$elev < 20000,]$id),
                                   name = paste("line",inflight[inflight$elev < 20000,]$id)) %>% 
            layout(showlegend = FALSE, xaxis = x, yaxis = speedlab)
          plots$plotly2 <- fig
          return(fig)
        }) 
        
        useRouteDropdown(TRUE)
      })
    })
  
  
  output$mapExp <- downloadHandler(
    filename = paste0('map_', Sys.time(),'.pdf'),
    content = function(file) {
      webshot::install_phantomjs()
      htmlwidgets::saveWidget(plots$mapLegend, "temp.html", selfcontained = FALSE)
      webshot::webshot("temp.html", file = file, cliprect = "viewport")
    },
    contentType = 'image/pdf')
  
  
  ####### HIGHLIGHT plotly by map click #######
  
  # capture map click data
  observe({
    # find all records of inputs containing map
    x <- names(reactiveValuesToList(input))
    maps <- x[grepl("map", x) & grepl("shape_click", x)]

    # create a matrix of map names and click ids
    clickEventsdf <- NA
    if(length(maps) > 0){
      clickEvents <- lapply(maps, function(x){
        return(c(x, input[[x]]$id))
      })
      clickEventsdf <- do.call(rbind, clickEvents)
    }
    
    mapClickDF(clickEventsdf)
  })
  
  
  # capture plotly click data
  observe({
    x <- reactiveValuesToList(input)
    
    ## we don't know which plotly was clicked most recently
    ## find which hover and click match
    plotHover <- x[grepl("plotly_hover", names(x))]
    plotHover[sapply(plotHover, is.null)] <- NULL
    plotClicked <- x[grepl("plotly_click", names(x))]

    if(!is.null(plotClicked) && !is.na(plotClicked) && length(plotClicked) > 0 &&
       !is.null(plotHover) && !is.na(plotHover) && length(plotHover) > 0){

      plotHover <- plotHover[[1]]
      
      match <- unlist(lapply(plotClicked, function(x){
        identical(x, plotHover)
      }))

      if(!is.null(match) && !is.na(match) && length(match) > 0 && any(match)){
        plotlyClickName <- strsplit(names(plotClicked[match]), split = "-")[[1]][2]  
        
        key <- event_data("plotly_click", source = plotlyClickName)$key[[1]]
        # plotly uses a different set of ids 
        curveNumber <- event_data("plotly_click", source = plotlyClickName)$curveNumber[[1]]
        
        plotlyClick(c(plotlyClickName, key, curveNumber))
      }
    }
  })

  
  ## COMBINED PLOTLY OR LEAFLET CLICK
  observe({ 
    plotlyClick <- plotlyClick()
    mapClick <- mapClickDF()
    
    if((!is.null(plotlyClick) && !is.na(plotlyClick) && length(plotlyClick > 0)) ||
       (!is.null(mapClick) && !is.na(mapClick) && length(mapClick > 0))){
    
      isolate({
        # where (map or plotly) does our new click info come from? 
        if (!is.null(plotlyClick) && !is.na(plotlyClick) && (is.null(oldPlotlyClick()) || is.na(oldPlotlyClick()))) { 
          clickInfoSource <- "firstplot" 
        } else if (!is.null(mapClick) && !is.na(mapClick) && (is.null(oldMapClick()) || is.na(oldMapClick()))) { # first plot click
          clickInfoSource <- "firstmap"
        }  else if (!is.null(oldPlotlyClick()) && !is.null(oldClickInfo()) && !is.na(oldClickInfo()) &&
                   !identical(oldPlotlyClick(), plotlyClick)){
          clickInfoSource <- "plot" 
        } else if (!is.null(oldMapClick()) && !is.null(oldClickInfo()) && !is.na(oldClickInfo()) &&
                  !identical(oldMapClick(), mapClick)) {
          clickInfoSource <- "map" 
        }

        highlightFlight <- NULL

        ## Get new click information
        ## A few numbers we need:
        ## Row number to create object names "map1", "elevplot2"...
        ## Path ID numbers: map ID, rowname ID, plotly curve number (starts at 0)
        ## Translate between plotly and leaflet IDs by looking at "highlightFlight" - subset of selFlights()
        if(clickInfoSource == "firstplot" || clickInfoSource == "plot"){
          plotlyClickKey <- as.numeric(plotlyClick[2])
          plotlyClickCurve <- as.numeric(plotlyClick[3])
          highlightFlight <- selFlightsSub()[which(selFlightsSub()$route == input$mapRouteDropdown &
                                                     selFlightsSub()$id == plotlyClickKey), ]
          mapClickID <- as.numeric(highlightFlight$id)
        } 
        else if (clickInfoSource == "map" || clickInfoSource == "firstmap"){
          if(ncol(mapClick) > 1){
            mapClickID <- mapClick[,2]
            
            highlightFlight <- selFlightsSub()[which(selFlightsSub()$id == mapClickID &
                                                       selFlightsSub()$route == input$mapRouteDropdown),]
            plotlyClickKey <- row.names(highlightFlight)
            
            # annoying because plotly arranges things by string
            plotlydf   <- selFlightsSub()[, c(input$mapColorRadio, "id", "route")]
            plotlydf$g <- NULL
            
            plotlydf <- plotlydf[which(plotlydf$route == highlightFlight$route),]
            plotlydf[, input$mapColorRadio] <- as.character(plotlydf[, input$mapColorRadio])
            plotlydf[, input$mapColorRadio][which(plotlydf[, input$mapColorRadio] == FALSE)] <- "No"
            plotlydf[, input$mapColorRadio][which(is.na(plotlydf[, input$mapColorRadio]))] <- "Unknown"
            plotlydf[, input$mapColorRadio][which(plotlydf[, input$mapColorRadio] == TRUE)] <- "Yes"
            plotlydf <- plotlydf[order(plotlydf[, input$mapColorRadio], as.character(plotlydf$id)),]
            
            plotlyClickCurve <- which(plotlydf$id == highlightFlight$id) - 1
          }
        } 

        if(!is.null(highlightFlight) && !is.na(highlightFlight)){
          oldClickInfo <- oldClickInfo()
          ## Revert old color
          if(!is.null(oldClickInfo) && !is.na(oldClickInfo) && length(oldClickInfo > 0)){
            ## Get old click information
            oldMapClickID <- oldClickInfo[1]
            oldPlotlyClickKey <- oldClickInfo[2]
            oldPlotlyClickCurve <- oldClickInfo[3]
            oldColor <- oldClickInfo[4]
            
            ### TODO CHANGE RED TO OLD COLOR
            leafletProxy("mapname") %>%
              clearGroup("highlighted") %>%
              clearGroup("sigmet")
            
            plotlyProxy("elevplotname") %>%
              plotlyProxyInvoke("restyle",
                                list(line = list(color = oldColor)),
                                list(oldPlotlyClickCurve))
            plotlyProxy("velplotname") %>%
              plotlyProxyInvoke("restyle",
                                list(line = list(color = oldColor)),
                                list(oldPlotlyClickCurve))
          }
          
          
          ## Highlight new route and add sigmets
          regionSigs <- unique(unlist(strsplit(highlightFlight$regionWeatherID, split = " ")))
          regionSigs <- regionSigs[which(regionSigs != 0)]
          allsigs <- allsigs()
          regionSigs <- allsigs[regionSigs,]
          
          leafletProxy("mapname", data=st_zm(regionSigs)) %>%
            clearGroup("sigmet") %>%
            addPolygons(data=st_zm(regionSigs),
                        color = "#77aaee", weight = 1,
                        group = "sigmet",
                        options = pathOptions(clickable = FALSE)
            ) %>%
            addPolylines(data = st_zm(highlightFlight),
                         color = isolate({input$mapColorHighlight}), opacity = 1, weight = 2,
                         #label = ~id,
                         group = "highlighted",
                         labelOptions = labelOptions(
                           style = list("font-weight" = "normal", padding = "3px 8px"),
                           textsize = "15px",
                           direction = "auto"),
                         highlightOptions = highlightOptions(
                           color = isolate({input$mapColorHighlight}), opacity = 1, weight = 2, fillOpacity = 1,
                           bringToFront = TRUE, sendToBack = TRUE))

          plotlyProxy("elevplotname") %>%
            plotlyProxyInvoke("restyle",
                              list(line = list(color = isolate({input$mapColorHighlight}))),
                              list(plotlyClickCurve))
          plotlyProxy("velplotname") %>%
            plotlyProxyInvoke("restyle",
                              list(line = list(color = isolate({input$mapColorHighlight}))),
                              list(plotlyClickCurve))
          
          # save old click information
          colorVal <- as.character(st_drop_geometry(highlightFlight[,input$mapColorRadio]))
          if(is.na(colorVal)){ colorVal <- "NA" }
          oldColor <- switch(colorVal,
                             "FALSE" = input$mapColorF,
                             "NA" = input$mapColorNA,
                             "TRUE" = input$mapColorT)
          oldMapClick(mapClick)
          oldPlotlyClick(plotlyClick)
          oldClickInfo(c(mapClickID, plotlyClickKey, plotlyClickCurve, oldColor))
        }
       
      })
    }
  })
  
  
  observe({
    input$mapSubmit
    isolate({
      oldClickInfo(NULL)
      oldMapClick(NULL)
      oldPlotlyClick(NULL)
    })
  })
  
 
  ########### COST ESTIMATE TAB ###########
  
  ## slider placeholders
  output$continentSliders <- renderUI({
    req(selFlights <- selFlights())
    sliderCount <- ifelse((is.null(input$defaultForCont) || input$defaultForCont == "Yes"), 
                          1,
                          #length(unique(selFlights()$continent))
                          5)
    slider_output_list <- lapply(1:sliderCount, function(i) {
      slidername <- paste0("slider", i)
      uiOutput(slidername)
    })
    tagList(slider_output_list)
  })
  
  
  # draw sliders for each continent
  observe({
    req(input$costBy)

    sliderCount <- ifelse((is.null(input$defaultForCont) || input$defaultForCont == "Yes"), 
                          1, 5)
    
    # get continent for each slider (blank if only 1 continent)
    if(is.null(input$defaultForCont) || input$defaultForCont == "Yes"){
      currentCont <- ""
    } else {
      #currentCont <- unique(selFlights()$continent)
      currentCont <- c("Asia", "Europe", "North America", "Oceania", "South America")
    }
        
    if(input$costBy == "Duration"){
      for (i in 1:sliderCount) {
        local({
          my_i <- i
          slidername <- paste0("slider", my_i)
          
          output[[slidername]] <- renderUI({
            sliderInput(paste0("costSlider", currentCont[my_i]), 
                        label = paste(currentCont[my_i], "Ave $/Block Hour"), 
                        min = 0, max = 30000, value = 2500)
          })
        })
      }
    } else {
      for (i in 1:sliderCount) {
        local({
          my_i <- i
          slidername <- paste0("slider", my_i)
          
          output[[slidername]] <- renderUI({
            sliderInput(paste0("costSlider", currentCont[my_i]), 
                        label = paste(currentCont[my_i], "Ave $/Km"), 
                        min = 0, max = 50, value = 5)
          })
        })
      }
    }
  })
  
  
  output$dateSelection <- renderUI({
    req(selFlights())
    dateRangeInput("costByDate", label = "Select Date Range", 
              start = startDate(),
              end = endDate(),
              min = startDate(),
              max = endDate())
  })
  
  output$monthCount <- renderUI({
    req(input$mondayOnly)
    req(input$costByDate)
    monthDif <- 12 * (year(input$costByDate[2]) - year(input$costByDate[1])) + 
      (month(input$costByDate[2]) - month(input$costByDate[1]))
    
    if(input$mondayOnly == "Existing Monday data"){
      dayCount <- sum(wday(seq(from = input$costByDate[1], to = input$costByDate[2], by = "days")) == 2)
      return(h5(paste("Displaying data for", dayCount, "Mondays across", monthDif, "months"), style="font-style: italic;"))
    } else {
      return(h5(paste("Displaying data for all days across", monthDif, "months"), style="font-style: italic;"))
    }
    
  })
  
  output$costTableTitle <- renderUI({
    req(input$mondayOnly)
    req(input$costByDate)
    monthDif <- 12 * (year(input$costByDate[2]) - year(input$costByDate[1])) + 
      (month(input$costByDate[2]) - month(input$costByDate[1]))
    
    if(input$mondayOnly == "Existing Monday data"){
      dayCount <- sum(wday(seq(from = input$costByDate[1], to = input$costByDate[2], by = "days")) == 2)
      return(h3(paste("Total Extension Impact for Flight Data over", dayCount, "Mondays across", monthDif, "Months")))
    } else {
      return(h3(paste("Total Extension Impact for Flight Data ", monthDif, "Months")))
    }
    
  })
  
  flightCost <- reactiveVal(value = NULL)
  costInputs <- reactiveVal(value = NULL)
  
  ### need this otherwise plots and table constantly refresh
  observe({
    x <- reactiveValuesToList(input)
    costInputs <- x[grepl("costSlider", names(x))]
    costInputs(costInputs)
  })
  
  
  observe({
    # triggers with user selecting routes or pressing update button
    input$costSubmit
    req(selRoutes <- selRoutes())
    req(selFlights <- selFlights())
    
    isolate({

      req(routes <- routes())
      
      ## inputs are assigned the default if user just selected routes
      if (length(costInputs()) == 0){
        costInputs <- c("costSlider" = 2500)
      } else {
        costInputs <- costInputs()
      }
 
      if(is.null(input$costByDate)){
        costByDate <- c(min(selFlights$date), max(selFlights$date))
      } else {
        costByDate <- input$costByDate
      }

      if(length(costInputs) > 0){
    
        selFlights$g <- NULL

        if (!is.null(input$defaultForCont) && input$defaultForCont == "No"){
          costInputs <- costInputs[-which(names(costInputs) == "costSlider")]
        } else {
          costInputs <- costInputs[which(names(costInputs) == "costSlider")]
        }
   
        if(length(costInputs) > 0){
          
          if (isolate({input$costBy}) == "Duration") {
            durOrDist <- "hourincrease"
            selFlights$hourincrease <- selFlights$minincrease / 60
            #costByRadio <- "costByDuration"
          } else {
            durOrDist <- "kmincreaseFlight"
            #costByRadio <- "costByDistance"
          }

          if (!is.null(input$defaultForCont) && input$defaultForCont == "No" &&
              # length(costInputs) == length(unique(selFlights$continent))
              length(costInputs) > 1
              ) {
            flightCost <- unlist(lapply(1:nrow(selFlights), function(i){
              return(selFlights[i,durOrDist] * as.numeric(costInputs[which(grepl(selFlights$continent[i], names(costInputs)))]))
            }))
          } else {
            flightCost <- selFlights[,durOrDist] * as.numeric(costInputs)
          }
          
          
          if(!is.null(flightCost)){
            
            selFlights$cost <- flightCost
            
            flightCost(flightCost)
            
            #filter routes by date
            selFlights <- selFlights[which(selFlights$date >= costByDate[1] &
                                             selFlights$date <= costByDate[2]),]
          
            if(!is.null(input$costColorRadio) && (input$costColorRadio  == "weatherDelay" | input$costColorRadio  == "nasDelay")){
              selFlights[which(selFlights[,input$costColorRadio] > 0), input$costColorRadio] <- TRUE
              selFlights[which(selFlights[,input$costColorRadio] == 0), input$costColorRadio] <- FALSE
            }
            
            legendLabel = names(legendChoices)[legendChoices == input$costColorRadio]
            
            output$costDistribution <- renderPlot({
              plots$plot7 <- ggplot(selFlights, aes(x=reorder(route, desc(route)), 
                                          y=cost, 
                                          color = as.character(as.logical(selFlights[,input$costColorRadio])))) + 
                geom_jitter(position=position_jitter(0.2)) + 
                theme_minimal() + 
                coord_flip() +
                xlab("Route\n") +
                ylab(paste("\nPer Flight Cost Due to Extra", isolate({input$costBy}))) +
                scale_x_discrete(labels=setNames(paste0(selRoutes, 
                                                        "\n(", 
                                                        lapply(selRoutes, function(x){
                                                          routes$continent[which(routes$route == x)]
                                                        }),
                                                        ")"),
                                                 selRoutes)) +
                theme(panel.grid.major.y = element_blank(),
                      axis.text=element_text(size=12),
                      axis.title=element_text(size=14,face="bold")) +
                scale_color_manual(values=c("FALSE" = input$costColorF, "TRUE" =input$costColorT), 
                                   na.value = input$costColorNA,
                                   name = legendLabel) 
              plots$plot7 + theme(legend.position = "none")
            }) 
      
            totals <- selFlights %>%
              group_by(route) %>%
              summarize(total = sum(cost),
                        average = sum(cost) / length(route),
                        weather = round(sum(na.omit(get(input$costColorRadio))) / length(route) * 100 ),
                        weatherCost = round(sum(cost[which(as.logical(get(input$costColorRadio)))] / sum(cost)) * 100),
                        aveNotWeatherCost = sum(cost[which(!(as.logical(get(input$costColorRadio))))]) / length(which(!(as.logical(get(input$costColorRadio))))),
                        aveWeatherCost = sum(cost[which(as.logical(get(input$costColorRadio)))]) / length(which(as.logical(get(input$costColorRadio))))
              )
            
            if(input$mondayOnly == "Extrapolate to all days"){
              totals$total <- totals$total*7
              startPlot <- ggplot(selFlights, aes(fill = as.character(as.logical(selFlights[ , input$costColorRadio])),
                                                  x = reorder(selFlights$route, desc(selFlights$route)),
                                                  y = cost * 7))
              yaxisMax <- sum(selFlights$cost) * 7 * 1.4
            } else {
              startPlot <- ggplot(selFlights, aes(fill = as.character(as.logical(selFlights[ , input$costColorRadio])),
                                     x = reorder(selFlights$route, desc(selFlights$route)),
                                     y = cost))
              yaxisMax <- sum(selFlights$cost) * 1.4
            }
            
            output$extraCost <- renderPlot({
              plots$plot8 <- startPlot +
                geom_bar(stat="identity") +
                theme_minimal() +
                coord_flip() +
                xlab("Route\n") +
                ylab(paste("\nTotal Cost Due to Extra", isolate({input$costBy}))) +
                scale_y_continuous(labels = scales::comma, limits = c(0, yaxisMax)) +
                scale_x_discrete(labels=setNames(paste0(selRoutes, 
                                                        "\n(", 
                                                        lapply(selRoutes, function(x){
                                                          routes$continent[which(routes$route == x)]
                                                        }),
                                                        ")"),
                                                 selRoutes)) +
                theme(panel.grid.major.y = element_blank(),
                      axis.text=element_text(size=12),
                      axis.title=element_text(size=14,face="bold")) +
                scale_fill_manual(values=c("FALSE" = input$costColorF, "TRUE" =input$costColorT),
                                  na.value = input$costColorNA,
                                  name = legendLabel) +
                geom_text(aes(x = unique(selFlights$route), y = totals$total + yaxisMax/70,
                              label = paste0("Total: $", comprss(totals$total),
                                             #"\nAve Per Flight: $", comprss(totals$average),
                                             #"\n",ifelse(input$costColorRadio == "nasDelay", "$ from NAS Delay: ", "$ from WX Affected: "),
                                             #totals$weatherCost, "%"
                                             "\n", ifelse(input$costColorRadio == "nasDelay", "Non-NAS Delayed Ave: $", "WX Unaffected Ave: $"), 
                                             comprss(totals$aveNotWeatherCost),
                                             "\n",ifelse(input$costColorRadio == "nasDelay", "NAS Delayed Ave: $", "WX Affected Ave: $"),
                                             comprss(totals$aveWeatherCost)
                                             ),
                              fill = NULL, hjust = 0), size = 4, data = totals)
              plots$plot8 + theme(legend.position = "none")
            })
          }
          
          
        }
        useRouteDropdown(TRUE)
      }
    })
  })
  
  costData <- reactive({
    return(get(load("appData/costData.RDS")))
  })
  

  observe({
    input$costSubmit
    routes()
    
    isolate({

      if (length(costInputs()) == 0){
        costInputs <- c("costSlider" = 2500)
      } else {
        costInputs <- costInputs()
      }
      
      if(is.null(input$costByDate)){
        costByDate <- c(min(costData()$date), max(costData()$date))
      } else {
        costByDate <- input$costByDate
      }
      
       output$totalCost <- renderDataTable({
         if(length(costInputs) > 0){

           if (isolate({input$costBy}) == "Duration") { durOrDist <- "hourincrease"
           } else { durOrDist <- "kmincreaseFlight"}
    
           costData <- costData()[which(costData()$date >= costByDate[1] &
                                        costData()$date <= costByDate[2]),]
    
           if(input$airbusOnly){
             costData <- costData[grepl("Airbus", costData$manufacturer),]
           }
    
            ## multiply totalExtraCost and averageExtraCostPerRoute by correct $$
            if (!is.null(isolate({input$defaultForCont})) && isolate({input$defaultForCont}) == "No"){
              costInputs <- costInputs[-which(names(costInputs) == "costSlider")]
            } else {
              costInputs <- costInputs[which(names(costInputs) == "costSlider")]
            }
           
            if(length(costInputs) > 0){
      
              #initialize to overwrite with user inputs
              costData$cost <- 0
              
              if (!is.null(isolate({input$defaultForCont})) && isolate({input$defaultForCont}) == "No" &&
                  length(costInputs) > 1) {
                
                lapply(names(costInputs), function(i){
                  contIndex <- which(grepl(gsub("costSlider", "", i ), costData$continent))
                  costData$cost[contIndex] <<- costData[contIndex, durOrDist] * as.numeric(costInputs[i])
                })
                
              } else {
                costData$cost <- costData[,durOrDist] * as.numeric(costInputs)
              }
            }
    
           totals <- costData %>%
             group_by(continent) %>%
             summarize(totalExtraCost = sum(cost),
                       nFlights = length(continent),
                       averageExtraCostPerFlight = mean(cost),
                       percWeatherAffected = round(sum(get(input$costColorRadio), na.rm = T) / length(continent) *100, digits = 1),
                       percWeatherAffectedCost =  round(sum(cost[which(as.logical(get(input$costColorRadio)))]) / 
                                                          sum(cost) * 100, digits = 1))
           
           allRow <- c("All Continents",
                       sum(costData$cost),
                       nrow(costData),
                       mean(costData$cost),
                       round(sum(costData[,input$costColorRadio], na.rm=T) / nrow(costData) * 100, digits = 1),
                       round(sum(costData[which(costData[,input$costColorRadio]), "cost"], na.rm=T) / sum(costData$cost) *100, digits = 1)
           )
    
           extrapRow <- c("Est. for all flights worldwide",
                          as.numeric(allRow[2]) * 9,
                          sum(as.numeric(allRow[3])) * 9,
                          as.numeric(allRow[4]),
                          as.numeric(allRow[5]),
                          as.numeric(allRow[6])
                          )
    
           totals <- rbind(totals, allRow, extrapRow)
    
           if(isolate({input$mondayOnly}) == "Extrapolate to all days"){
                 totals$totalExtraCost <- as.numeric(totals$totalExtraCost) * 7
                 totals$nFlights <- as.numeric(totals$nFlights) * 7
           }
    
           totals$totalExtraCost <- paste0("$", comprss(totals$totalExtraCost))
           totals$averageExtraCostPerFlight <- paste0("$", comprss(totals$averageExtraCostPerFlight))
           totals$percWeatherAffected <- paste0(totals$percWeatherAffected, "%")
           totals$percWeatherAffectedCost <- paste0(totals$percWeatherAffectedCost, "%")
    
           names(totals) <- c("Continent", "Total Extension Cost", "No. Flights", 
                              "Ave. Extension Cost Per Flight", 
                              "% WX affected flights", "% WX affected cost")
           
           if(input$costColorRadio == "nasDelay"){
             names(totals)[c(5,6)] <- c("% NAS affected flights", "% NAS affected cost")
           }
           
           datatable(totals,
                     options = list(dom='t',
                                    defer = TRUE, ordering=F,
                                    autoWidth = TRUE,
                                    initComplete = JS("function(settings, json) {", jsSettingsDT, "}")
                     )
           )
        }
      })
    })
  })
  
  output$plotExp7 <- downloadHandler(
    filename = function() { paste0('flightCost-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot7, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$plotExp8 <- downloadHandler(
    filename = function() { paste0('totalCost-plot_', Sys.time(),'.svg') },
    content = function(file) {
      ggsave(file, plot = plots$plot8, device = "svg", width = 10, height = 5)
    },
    contentType = 'image/svg'
  )
  
  output$thirdParagraph <- renderUI(
    p(paste0("Data is from the Open Sky Network. These flightpaths occured on MONDAYS between ",
    startDate(), " and ", endDate(),
    ". Most often, location was recorded every 10 seconds.
    Only paths without a location break of more than 30 minutes are included.
    Only routes that were repeated at least 50 times are included.
    Note: This data only captures <2% of flights that took place during these dates."))
  )
}  # end server


# UI ----------------------------------------------------------------------

ui <- tagList(
  tags$head(tags$style(HTML(
    "
    body{ 
      background-color: #f5f6f7!important; 
    }
    .box{
      background-color: #ffffff!important; 
      padding: 20px;
      margin: 10px;
      box-shadow: 5px 5px 10px rgba(0,0,0,0.1);
      border-radius: 5px;
    }
    .navbar-default{
      background-color: #00456B!important; 
    }
    .btn-primary{
      background-color: #00456B!important; 
    }
    .btn-primary:hover,
    .btn-primary:focus{
      background-color: #00264D!important; 
    }
    .navbar-default .navbar-nav>li>a:hover, 
    .navbar-default .navbar-nav>li>a:focus{
      color:#FB9334!important
    }
    .navbar-default .navbar-nav>.active>a, 
    .navbar-default .navbar-nav>.active>a:hover, 
    .navbar-default .navbar-nav>.active>a:focus{
      background-color: #00264D!important; 
    }
    .shift{
    margin-left: 10px!important;
    }
    "
    ))),
  navbarPage(
    
        theme = shinythemes::shinytheme("flatly"),

        title = h3("Same Route, Different Path", style = "color: white; 
                                                          margin: -2px 100px 0 20px;
                                                          font-size: 20px;"),
        
        tabPanel(
          title = "Select Routes", icon = icon("check-circle"),
         
          fluidRow(class = "shift",
            column(12, h3("Route Summary"),
                   h5("Filter or sort columns in table below to find flights of interest, 
                      or flights that are similar but in different continents.
                      Click to highlight rows, then click the button to compare routes."))
          ),
          br(),
          fluidRow(class = "shift",
            column(2, style="margin-top:14px;",
                   checkboxInput("airbusOnly", label = "Show Airbus Aircraft Only")),
            column(2, style="margin-top:12px;",
                   actionButton("clear", "Clear Selection", class = "btn-primary")),
            column(2, offset = 2, style="margin-top:12px;",
                   actionButton("submit", "Compare Selected Routes", class = "btn-primary")),
            column(4, style="margin-top:14px;",
                   uiOutput("buttonMessage"))
          ),
          br(),
          fluidRow(
            div(style="margin:0 20px;",
              column(12, DTOutput("selectRoutes"))
            )
          )
        ),
        
        tabPanel(
          title = "Compare Routes", icon = icon("table"),
          #br(),
          
          fluidRow( class = "shift",
            column(12, h3("Compare Routes"),
                   h5("After a few seconds, six plots will appear to explore distance and duration variability. 
                      Change the inputs for color choices in the box below the plots."))
          ),
          br(),
          box(width = 12, 
            fluidRow(
              column(12, uiOutput("plotMessage"))
            ),
            fluidRow(
              column(3, uiOutput("title1")),  
              column(1, downloadButton("plotExp1", "", class = "btn-primary")),
              column(3, uiOutput("title2")),
              column(1, downloadButton("plotExp2", "", class = "btn-primary")),
              column(3, uiOutput("title3")), 
              column(1, downloadButton("plotExp3", "", class = "btn-primary"))
            ),
            fluidRow(
              column(4, plotOutput("plot1", height=300) %>% shinycssloaders::withSpinner(color="#2c3e50")),
              column(4, plotOutput("plot2", height=300) %>% shinycssloaders::withSpinner(color="#2c3e50")),
              column(4, plotOutput("plot3", height=300) %>% shinycssloaders::withSpinner(color="#2c3e50"))
            ),
            
            br(),
            fluidRow(
              column(3, uiOutput("title4")),
              column(1, downloadButton("plotExp4", "", class = "btn-primary")),
              column(3, uiOutput("title5")),
              column(1, downloadButton("plotExp5", "", class = "btn-primary")),
              column(3, uiOutput("title6")),
              column(1, downloadButton("plotExp6", "", class = "btn-primary"))
            ),
            fluidRow(
              column(4, plotOutput("plot4", height=300)),
              column(4, plotOutput("plot5", height=300)),
              column(4, plotOutput("plot6", height=300))
            ),
            br()
          ),
          br(),
          box(width = 12, 
            fluidRow(
              column(3, radioButtons("plotColorRadio", label = "Color By:", 
                                    choices = c("Path Overlaps Weather? (sigmet)" = "thisWeather",
                                                "Weather in Region? (sigmet)" = "regionWeather",
                                                "Path Overlaps Storm Event? (US Only)" = "stormEvent", 
                                                #"Any Delay (US Only)" = "anyDelay", 
                                                "Weather Delayed? (US Only)" = "weatherDelay",
                                                "NAS Delayed? (US Only)" = "nasDelay"),
                                    selected = "thisWeather")),
              column(3, colourpicker::colourInput("plotColorT", "TRUE",
                                    value = "#E74C3C", allowTransparent = TRUE)),
              column(3, colourpicker::colourInput("plotColorF", "FALSE", 
                                    value = "#19BFA363", allowTransparent = TRUE)),
              column(3, colourpicker::colourInput("plotColorNA", "Missing Data", 
                                    value = "#97A7A866", allowTransparent = TRUE))
            )
          )
        ),
        
        tabPanel(
          title = "Flight Details", icon = icon("map"),
          
          fluidRow(class = "shift", 
            column(12, h3("Explore Flight Details"),
                   h5("Due to data size restrictions, 
                      only showing shortest routes (one by duration, one by distance), 
                      and longest routes (30 by duration, 30 by distance).
                      Click on a flight to highlight across plots. 
                      Change the route showing and the color inputs in the box below the plots.
                      Map download takes a few seconds. Download plots by clicking the camera icon that appears on hover."))
          ),
          br(),
          box(width = 12, 
            fluidRow(
              column(12, uiOutput("mapsMessage"))
            ),
            fluidRow( 
              column(3, uiOutput("titlename")),
              column(1, downloadButton("mapExp", "",  class = "btn-primary"))
            ),
            fluidRow( column(4, leafletOutput("mapname", height = "350px")  
                             %>% shinycssloaders::withSpinner(color="#2c3e50") , style="margin-top:20px;"),
                      column(4, plotlyOutput("elevplotname", height = "400px")),
                      column(4, plotlyOutput("velplotname", height = "400px"))
            ),
            br()
          ),
          br(),
          box(width = 12, 
            fluidRow(
              column(3, radioButtons("mapColorRadio", label = "Color By:", 
                                     choices = c("Path Overlaps Weather? (sigmet)" = "thisWeather",
                                                 "Weather in Region? (sigmet)" = "regionWeather",
                                                 "Path Overlaps Storm Event? (US Only)" = "stormEvent", 
                                                 "Weather Delayed? (US Only)" = "weatherDelay",
                                                 "NAS Delayed? (US Only)" = "nasDelay"),
                                     selected = "thisWeather")),
              column(3, colourpicker::colourInput("mapColorT", "TRUE",
                                    value = "#E74C3C", allowTransparent = TRUE),
                     uiOutput("mapRouteSelect")),
              column(3, colourpicker::colourInput("mapColorF", "FALSE", 
                                    value = "#19BFA363", allowTransparent = TRUE),
                        colourpicker::colourInput("mapColorHighlight", "Highlight", 
                                    value = "#FB9334", allowTransparent = TRUE)),
              column(3, colourpicker::colourInput("mapColorNA", "Missing Data", 
                                    value = "#97A7A866", allowTransparent = TRUE),
                            actionButton("mapSubmit", label = "Update", class = "btn-primary",
                                         style = "margin-top:25px;"))
            )
          )
        ),
        
        tabPanel(
          title = "Cost Compare", icon = icon("dollar-sign"),
          
          fluidRow( class = "shift",
            column(12, h3("Compare Costs"),
                   h5("Decide whether the extra cost of a route should be determined by the 
                      extra duration or the extra distance from the shortest path a route contains.
                      Adjust the cost/hour or /km for every flight or by continent.
                      Change the inputs for color choices in the box below the plots."))
          ),
          br(),
          box(width = 3, 
              radioButtons("costBy", "Compare extra cost by:", 
                                     inline = TRUE,
                                     choices = c("Duration", "Distance")),
              radioButtons("defaultForCont", label = "Same Costs for Continents",
                           choices = c("Yes", "No"), inline = TRUE),
              uiOutput("continentSliders"),
              radioButtons("mondayOnly", "Show cost by:", 
                           choices = c("Existing Monday data", "Extrapolate to all days")),
              uiOutput("dateSelection"),
              uiOutput("monthCount"),
              actionButton("costSubmit", label = "Update", class = "btn-primary")
          ),
          box(width = 9, 
              fluidRow(
                column(5, uiOutput("costMessage")),
                column(1, downloadButton("plotExp7", "", class = "btn-primary")),
                column(1, offset = 5, downloadButton("plotExp8", "", class = "btn-primary"))
              ),
          
              fluidRow(
                column(6, plotOutput("costDistribution", height=500) %>% shinycssloaders::withSpinner(color="#2c3e50")),
                column(6, plotOutput("extraCost", height=500) %>% shinycssloaders::withSpinner(color="#2c3e50"))
              ),
              br(),
              fluidRow(
                    column(12,  
                           uiOutput("costTableTitle"),
                           DTOutput("totalCost"))
              )
          ),
          br(),
          br(),
          box(width = 12, 
              fluidRow(
                column(3, radioButtons("costColorRadio", label = "Color By:", 
                                       choices = c("Path Overlaps Weather? (sigmet)" = "thisWeather",
                                                   "Weather in Region? (sigmet)" = "regionWeather",
                                                   "Path Overlaps Storm Event? (US Only)" = "stormEvent", 
                                                   #"Any Delay (US Only)" = "anyDelay", 
                                                   "Weather Delayed? (US Only)" = "weatherDelay",
                                                   "NAS Delayed? (US Only)" = "nasDelay"),
                                       selected = "thisWeather")),
                column(3, colourpicker::colourInput("costColorT", "TRUE",
                                      value = "#E74C3C", allowTransparent = TRUE)),
                column(3, colourpicker::colourInput("costColorF", "FALSE", 
                                      value = "#19BFA363", allowTransparent = TRUE)),
                column(3, colourpicker::colourInput("costColorNA", "Missing Data", 
                                      value = "#97A7A866", allowTransparent = TRUE))
              )
          )
        ),
        
        tabPanel(
          title = "Help", icon = icon("question"),
          
          box(width = 12, 
              fluidRow(
                column(12, h3("What's the point?"))
              ),
              fluidRow(
                column(12, 
                  p("Planes traveling the same route won't always travel along the same path. 
                     There is variability in flight paths due to scheduling, and avoiding other planes, 
                     restricted airspace, or bad weather."),
                  p("There will always be a shortest path, and when that is not traveled, 
                     extra fuel and time is spent, resulting in money wasted. Note that the shortest path 
                     is defined not as some hypothetical path, or the direct path distance, 
                     but as the shortest path observed in the data. 
                     We can explore flight paths to find which have the most extra miles,
                     and begin to understand the reason behind longer paths."),
                  uiOutput("thirdParagraph")
                )
              ),
              br(),
              fluidRow(
                column(12, h3("Credit"))
              ),
              fluidRow(
                column(4, 
                  img(src="acubed_logo.png", width = "200px"),
                  h5("Created By Project Monark")
                )
              ),
              br(),
              fluidRow(
                column(12, h3("Tutorial"))
              ),
              fluidRow(
                column(12, 
                  tags$video(src = "MonaRk.mp4", type = "video/mp4", width = "900px", controls ="controls")
                )
              )
          )
        )
  )
)
############# APP ################
shinyApp(ui = ui, server = server)