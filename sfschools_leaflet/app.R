library(shiny)
library(leaflet)
library(RColorBrewer)

ui <- fluidPage(
  leafletOutput("mymap")
)

# Assign color
assign_color <- function(d, colors_vector) {
  if (d < 2) colors_vector[1]
  else if (d ==3) colors_vector[2]
  else if (d ==4) colors_vector[3]
  else if (d ==5) colors_vector[4]
  else if (d ==6) colors_vector[5]
  else if (d ==7) colors_vector[6]
  else if (d ==8) colors_vector[7]
  else if (d ==9) colors_vector[8]
  else if (d ==10) colors_vector[9]
}

# Define server logic required to draw a histogram


server <- function(input, output, session) {
  d <- read.csv("schools_math_eng_scores.csv")
  blues <- brewer.pal(9, "Blues")
  d$color <- lapply(d$score, assign_color, blues) 
  
  output$mymap <- renderLeaflet({
  m <- leaflet() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(data = d, lng = ~longitude, lat = ~latitude,
                     color = ~color, fillOpacity = 0.8, 
                     radius = ~1.2*score,
                     popup = ~paste("<style> div.leaflet-popup-content-wrapper { opacity: 0.7; } </style>","<h4>", school_name, "</h4>",
                                    "<h4>", grades,
                                    "<h4>", "English Percent: ", eng_percent,
                                    "<h4>", "Math Percent: ", math_percent,
                                    "<h4>", "Score: ", score,
                              sep = ""),
                     popupOptions())
  })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

