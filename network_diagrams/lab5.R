library(shiny)
library(networkD3)

data("votes.repub", package = "cluster")
music <- jsonlite::fromJSON("lady_gaga.json", simplifyDataFrame = FALSE)

ui <- shinyUI(fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css")
  ),
  
  titlePanel("Shiny networkD3 "),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("opacity", "Opacity (not for Sankey)", 0.6, min = 0.1,
                  max = 1, step = .1)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Radial Network", radialNetworkOutput("radial")),
        tabPanel("Cluster Dendrogram", dendroNetworkOutput("dendro"))
      )
    )
  )
))

#### Server ####
server <- function(input, output) {
  
  output$radial <- renderRadialNetwork({
    radialNetwork(List = music, fontSize = 15, opacity = input$opacity)
  })
  
  output$dendro <- renderDendroNetwork({
    hc <- hclust(dist(votes.repub), "ave")
    dendroNetwork(hc, treeOrientation = "vertical", linkType = "diagonal"
                  , textColour = c("red", "green", "orange")[cutree(hc, 3)], opacity = input$opacity , fontSize = 13)
  })
  
  
}

#### Run ####
shinyApp(ui = ui, server = server)