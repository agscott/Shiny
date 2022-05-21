rm(list=ls())

load("Appsilon.RData")

# Load packages

#library(shinyWidgets)
library(shiny)
library(shinythemes)
library(dplyr)
library(plotly)
library(shinycssloaders)

library(raster)
library(leaflet)
library(bslib)

# Define UI
ui <- bootstrapPage(
  theme  = bs_theme(#version = 5, 
    bootswatch = "superhero"), 
  div(class="container-fluid",
      div(class="text-center text-lg-start",
          h2("Polish Species Observations", class="d-none d-lg-block"),
          h2("Polish Species", class="d-block d-lg-none")
      ),
      div(class="row",
          div(class="col-lg-4",
              div(class="well",
                  selectizeInput(inputId = "vernaName", label = strong("1) By Vernacular Name"),
                                 choices = sort(unique(data3$vernacularName))[-c(1:6)], # Remove numeric names
                                 selected = sort(unique(data3$vernacularName))[170], # Brown Bear is default
                                 multiple = TRUE #,
                                 #width = '150px'
                  ),
                  helpText("Use dropdown to select speciies or start typing."),
                  helpText("Multiple entries can be made.")
              ),
              div(#class="col-lg-4",
                div(class="well",
                    selectizeInput(inputId = "type", label = strong("2) - By Scientific Name"),
                                   choices = sort(unique(data3$scientificName)),
                                   selected = NULL,
                                   multiple = TRUE
                    ),
                    helpText(strong("Or enter the scientific name. All selections will apply.")),
                    br(),
                )
              ),
          ),
          div(class="col-lg-7", 
              br(),
              titlePanel(textOutput("Polish Species Observations")),
              tabsetPanel(
                type="pills",
                tabPanel("Timeline",
                         isolate(plotlyOutput(outputId = "plot", height = "500px") %>% withSpinner(color="#B32400"))),
                tabPanel("Map",
                         isolate(leafletOutput(outputId = "mymap", height = "500px") %>% withSpinner(color="#B32400"))),
                selected = "Map"
              ) ,
              fluidRow(width=12,
                       tags$br(),
                       tags$b("Sourced from: "), tags$br(), tags$br(),
                       tags$a(href = "https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165", "Global Biodiversity Information Facility", target = "_blank"),
                       tags$style(type="text/css", 
                                  ".shiny-output-error { visibility: hidden; }",
                                  ".shiny-output-error:before { visibility: hidden; }")
              )
          )
      )
  )
)

# Define server function
server <- function(input, output, session) {
  
  selectedData <- reactive({
    xx<-data3[is.element(data3$scientificName,input$type)|is.element(data3$vernacularName,input$vernaName),]
    xx$vernacularName<-ifelse(is.na(xx$vernacularName),xx$scientificName,xx$vernacularName)
    xx
  })
  
  output$plot <- renderPlotly({
    
    df_count <- count(selectedData(), vernacularName, eventDate)
    
    fig <- plot_ly(
      data = df_count,
      x = ~eventDate,
      y = ~n,
      color = ~vernacularName,
      type = "bar",
      text = ~vernacularName,
      hovertemplate = paste(
        "<b>%{text}</b><br>",
        "%{yaxis.title.text}: %{y}<br>",
        "%{xaxis.title.text}: %{x}", 
        "<extra></extra>"
      )
    ) %>% layout(barmode = "stack",
                 legend=list(font=list(size=10), y=1, orientation='v'),
                 yaxis=list(title="No. of Observations", dtick=1, font = list(size=10)),
                 xaxis=list(title="Date", tickfont = list(size = 9))
    ) %>% config(displayModeBar = FALSE, displaylogo = FALSE)
    fig
  })
  
  output$mymap <- renderLeaflet({
    
    coords<-SpatialPoints(cbind(selectedData()[,"longitudeDecimal"],selectedData()[,"latitudeDecimal"]))
    p<-SpatialPointsDataFrame(coords,selectedData())
    pal <- colorFactor(c("#B32400", "#B3B300","#018FB3", "#FFC330"), domain = selectedData()$vernacularName)
    chart<-leaflet(p) %>%
      addTiles() %>%
      addCircles(lng = ~longitudeDecimal, lat = ~latitudeDecimal, radius = 235,
                 label = ~paste(vernacularName, " ",eventDate),
                 opacity = 1,
                 color = ~pal(vernacularName),
                 popup = ~paste("Lat: ",scientificName),
                 labelOptions = labelOptions(noHide = F, textOnly = FALSE,
                                             style = list(
                                               "font-family" = "serif",
                                               "font-style" = "bold",
                                               "font-size" = "14px",
                                               "border-color" = "rgba(0,0,0,0)"
                                             ))
      ) %>%
      addLegend("bottomright", pal = pal, values = ~vernacularName, title = "Selected Species")
  })
}

# Create Shiny object
shinyApp(ui = ui, server = server)
