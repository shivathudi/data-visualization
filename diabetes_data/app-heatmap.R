packages <- c("ggplot2", "d3heatmap", "dplyr", "shiny")

for (i in 1:length(packages)) {
  if (!require(packages[i])) install.packages(packages[i])
}


library(d3heatmap)
library(shiny)

ui <- fluidPage(
  h1("Diabetes dataset heatmap"),
  selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
  checkboxInput("cluster", "Apply clustering"),
  d3heatmapOutput("heatmap")
)

server <- function(input, output, session) {
  output$heatmap <- renderD3heatmap({
    d3heatmap(
      scale(mtcars),
      colors = input$palette,
      dendrogram = if (input$cluster) "both" else "none"
    )
  })
}

shinyApp(ui, server)
