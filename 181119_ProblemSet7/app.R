#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(haven)
library(fs)
library(readxl)
library(lubridate)
library(kableExtra)
library(janitor)
library(leaflet)
library(statesRcontiguous)
library(sp)
library(rgdal)
library(raster)
library(tidyverse)
library(moderndive)

## User Interface
ui <- fluidPage(
  
  ## Defines two tabs - Summary, Map & Plot
  tabsetPanel(
    tabPanel("Summary", fluid = TRUE,
    titlePanel("Polling Data from NYT's Upshot"), 
    
    ## Renders image to display NYT logo
    uiOutput("image"),
    h5("Click on any one of the districts polled by the New York Times! In the popup you will
     find information about the Republican advantage compared to the actual vote margin experienced
     by the candidates against their Democratic opponents. Also included are the titles of the winning
     candidate and their respective party. In the downbar, you will find a model comparing the turnout score
     against the final weight for voters in the final wave of polling in that district. This plot also
     has a linear model to easily identify relationships in different congressional districts.")), 
    
    tabPanel("Map & Plot", fluid = TRUE,
    titlePanel("Click a District!"),
    
    ## Renders map, tables, and link to github repo
    leafletOutput("mymap", height = "350px"),
    uiOutput("tab"), 
  plotOutput("plot", height = "200px"))
  ))

## Reads an xlsx document with information
## on NYT districts and partial call to url 
## from their github repo

districts <- read_xlsx("districts.xlsx")

## Filters out empty House districts, converts
## naming convention to that of .shp files

districts <- districts %>%
  filter(!is.na(district.name)) %>% 
  mutate(district.name = paste0("Congressional District ", district.name))

## filters .shp by the distrcits that 
## were polled by the NYT

shp <- districts %>% 
  left_join(shp_all_us_congressional_districts)

## converts resulting dataframe back to shp
shp <- st_as_sf(shp)



# Define server logic required to draw a histogram
server <- function(input, output) {
  
  ## Factor variable to color different districts
  
  pal <- colorFactor(palette = c("red", "blue", "#A0A0A0"), 
                     levels = c("Republican", "Democrat", "Undecided"))
  
  ## Renders map from shp files and defines 
  ## information for popup
  
  output$mymap <- renderLeaflet({
    leaflet(shp) %>%
      addProviderTiles(provider = "CartoDB") %>% 
      setView(lat = 40, lng = -100, zoom = 4) %>% 
      addPolygons(highlight = highlightOptions(bringToFront = TRUE), 
                  popup = paste0(shp$state.short.name, "-", shp$district.name, "<br>", "<br>", 
                        "Republican Advantage: ", shp$rep.adv*100,"%", "<br>",
                        "Actual Advantage: ", shp$actual.adv*100,"%", "<br>",
                        "Winner: ", shp$win.name),
                  color = pal(shp$win.party), 
                  layerId = shp$wave.number)
    })
  
  ## Creates actions to be taken when 
  ## district is clicked by the user
  observeEvent(input$mymap_shape_click, {
    if(is.null(click))
      return()
    
    ## If clicked, this section of code
    ## converts the partial call that serves
    ## as the click ID to a full url that 
    ## is read into a csv
    
    else
      {
      click <- input$mymap_shape_click
      path <- paste0("https://raw.githubusercontent.com/TheUpshot/2018-live-poll-results/master/data/elections-poll-", click$id,".csv")
      df <- read_csv(path)
      
      ## Plot is created that creates a 
      ## linear regression for voter turnout score
      ## against their individual weight
      
      output$plot <- renderPlot({
        file <- df %>%
          ggplot(aes(x = as.numeric(turnout_score), y = as.numeric(final_weight))) +
          geom_point() +
          labs(x = "Turnout Score", y = "Final Weight") +
          geom_smooth(method = "lm", se = FALSE)
        return(file)
        path <- NULL
      })
      
      }
  })
  
  ## Defines the image that should be rendered
  ## from its online url
  
  output$image = renderUI({
    tags$img(src = "https://www.de-simone.com/assets/new-york-times-logo-large-e1439227085840.jpg", width = "100%")})
  
  ## Creates a link to the github repository
  
  url <- a("sonyakalara", href="https://github.com/sonyakalara/problem_set_7")
  output$tab <- renderUI(tagList("Link to github repository:", url))
}


# Run the application 
shinyApp(ui = ui, server = server)