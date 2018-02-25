library(shiny)
library(leaflet)
library(RColorBrewer)
library(rgdal)


#setwd("~/BGSE/Term 2/Data Viz/envdata")
df = read.csv("env.csv", header=TRUE, sep=",", fill=TRUE)
df <- df[,c("ISO3v10","Environmental.Health","EH...Health.Impacts","EH...Air.Quality","EH..Water.and.Sanitation")]

# read vector data: 
v <- geojsonio::geojson_read("custom.geo.json",
                             what = "sp")

# safer approach to 1:1 or 1:many (geom:atts) joins 
# add a sorting id for later use 
v@data$sorting_id <- 1:nrow(v@data) 

# make a copy of the original table 
orig.table <- v@data 

# make the table with 'joined' data 
new.table <- merge(x=orig.table, y=df, by.x='gu_a3', by.y='ISO3v10',all.x=TRUE) 

# re-order this table based on the original row order 
new.table.ordered <- new.table[order(new.table$sorting_id), ] 

# restore the origina row names 
row.names(new.table.ordered) <- row.names(orig.table) 

# replace the data table 
v@data <- new.table.ordered 





ui <- bootstrapPage(
    tags$head(
        tags$style(HTML(".leaflet-container { background: #c9e4f9; }"))
    ),
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%"),
    absolutePanel(style="padding: 10px; background: #FFFFFF; opacity: 0.92",
                  id = "sidebar", top = 10, right = 10,
                  h4("Global Environmental Indicators"),
                         sliderInput("Overall", "Overall Environmental Health", 0, 100,
                                     value = c(0,100), step = 5
                         ),
                         sliderInput("Health", "Health Impacts", 0, 100,
                                     value = c(0,100), step = 5
                         ),
                         sliderInput("Air", "Air Quality", 0, 100,
                                     value = c(0,100), step = 5
                         ),
                         sliderInput("Water", "Water and Sanitation", 0, 100,
                                     value = c(0,100), step = 5
                         ),
                         selectInput("toplot", "Indicator to visualize",
                                     choices = c("Overall Environmental Health" = "Environmental.Health", 
                                                 "Health Impacts" = "EH...Health.Impacts",
                                                 "Air Quality" = "EH...Air.Quality",
                                                 "Water and Sanitation" = "EH..Water.and.Sanitation")
                         )
    )
)
server <- function(input, output, session) {
    

    filteredSPData <- reactive({
        v[v@data$Environmental.Health >= input$Overall[1] & v@data$Environmental.Health  <= input$Overall[2] & !is.na(v@data$Environmental.Health)
          & v@data$EH...Health.Impacts >= input$Health[1] & v@data$EH...Health.Impacts  <= input$Health[2] & !is.na(v@data$EH...Health.Impacts)
          & v@data$EH...Air.Quality >= input$Air[1] & v@data$EH...Air.Quality  <= input$Air[2] & !is.na(v@data$EH...Air.Quality)
          & v@data$EH..Water.and.Sanitation >= input$Water[1] & v@data$EH..Water.and.Sanitation  <= input$Water[2] & !is.na(v@data$EH..Water.and.Sanitation),]
    })
    

    
    output$map <- renderLeaflet({
        # Use leaflet() here, and only include aspects of the map that
        # won't need to change dynamically (at least, not unless the
        # entire map is being torn down and recreated).
        proxy <- leaflet(v)
        proxy %>% 
            setView(lng=47.392998 , lat =1.978906, zoom=2) %>%
            addProviderTiles(providers$HikeBike.HikeBike,options = providerTileOptions(noWrap = TRUE, maxZoom = 6, minZoom = 1)) 

        
    })
    
    # Incremental changes to the map (in this case, replacing the
    # circles when a new color is chosen) should be performed in
    # an observer. Each independent set of things that can change
    # should be managed in its own observer.
    observe({
        if (input$toplot == "Environmental.Health"){
            pal <- colorNumeric(
                palette =  c('#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
                domain = v$Environmental.Health)
            leafletProxy("map", data = filteredSPData()) %>%
                clearShapes() %>%
                addPolygons(fillColor = ~pal(Environmental.Health),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            fillOpacity = 0.7,
                            highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                bringToFront = TRUE),
                            popup=paste("<b>",filteredSPData()$name_long, "</b>","<br>",
                                        "Environmental Health: ", filteredSPData()$Environmental.Health, "<br>",
                                        "Health Impacts: ", filteredSPData()$EH...Health.Impacts, "<br>",
                                        "Air Quality: ", filteredSPData()$EH...Air.Quality, "<br>",
                                        "Water and Sanitation: ",filteredSPData()$EH..Water.and.Sanitation))%>%
                clearControls()%>%
                addLegend(position = "bottomright",
                          pal = pal, 
                          title = "Score",
                          values = ~Environmental.Health)
        }else if (input$toplot == "EH...Health.Impacts"){
            pal <- colorNumeric(
                palette =  c('#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
                domain = v$EH...Health.Impacts)
            leafletProxy("map", data = filteredSPData()) %>%
                clearShapes() %>%
                addPolygons(fillColor = ~pal(EH...Health.Impacts),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            fillOpacity = 0.7,
                            highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                bringToFront = TRUE),
                            popup=paste("<b>",filteredSPData()$name_long, "</b>","<br>",
                                        "Environmental Health: ", filteredSPData()$Environmental.Health, "<br>",
                                        "Health Impacts: ", filteredSPData()$EH...Health.Impacts, "<br>",
                                        "Air Quality: ", filteredSPData()$EH...Air.Quality, "<br>",
                                        "Water and Sanitation: ",filteredSPData()$EH..Water.and.Sanitation))%>%
                clearControls()%>%
                addLegend(position = "bottomright",
                          pal = pal, 
                          title = "Score",
                          values = ~EH...Health.Impacts)
        }else if (input$toplot == "EH...Air.Quality"){
            pal <- colorNumeric(
                palette =  c('#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
                domain = v$EH...Air.Quality)
            leafletProxy("map", data = filteredSPData()) %>%
                clearShapes() %>%
                addPolygons(fillColor = ~pal(EH...Air.Quality),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            fillOpacity = 0.7,
                            highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                bringToFront = TRUE),
                            popup=paste("<b>",filteredSPData()$name_long, "</b>","<br>",
                                        "Environmental Health: ", filteredSPData()$Environmental.Health, "<br>",
                                        "Health Impacts: ", filteredSPData()$EH...Health.Impacts, "<br>",
                                        "Air Quality: ", filteredSPData()$EH...Air.Quality, "<br>",
                                        "Water and Sanitation: ",filteredSPData()$EH..Water.and.Sanitation))%>%
                clearControls()%>%
                addLegend(position = "bottomright",
                          pal = pal, 
                          title = "Score",
                          values = ~EH...Air.Quality)
        }else {
            pal <- colorNumeric(
                palette =  c('#d73027','#f46d43','#fdae61','#fee08b','#d9ef8b','#a6d96a','#66bd63','#1a9850'),
                domain = v$EH..Water.and.Sanitation)
            leafletProxy("map", data = filteredSPData()) %>%
                clearShapes() %>%
                addPolygons(fillColor = ~pal(EH..Water.and.Sanitation),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            fillOpacity = 0.7,
                            highlightOptions = highlightOptions(color = "white", weight = 5,
                                                                bringToFront = TRUE),
                            popup=paste("<b>",filteredSPData()$name_long, "</b>","<br>",
                                        "Environmental Health: ", filteredSPData()$Environmental.Health, "<br>",
                                        "Health Impacts: ", filteredSPData()$EH...Health.Impacts, "<br>",
                                        "Air Quality: ", filteredSPData()$EH...Air.Quality, "<br>",
                                        "Water and Sanitation: ",filteredSPData()$EH..Water.and.Sanitation))%>%
                clearControls()%>%
                addLegend(position = "bottomright",
                          pal = pal, 
                          title = "Score",
                          values = ~EH..Water.and.Sanitation)
        }
                
            
    })
    

}

shinyApp(ui, server)