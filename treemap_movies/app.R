library(treemap)
library(d3treeR)
library(reshape2)

# Lab assignment
# install.packages("ggplot2movies")
library(ggplot2movies)
data(movies)
head(movies)
m <- na.omit(movies)

long_m <- melt(m, id.vars = colnames(m)[1:17])
long_m <- subset(long_m, value==1)

long_m$Genre <- factor(long_m$variable)

my_df <- subset(long_m, select = c(title, budget, rating, Genre))

my_df <-my_df[sample(1:nrow(my_df), 200, F),]

my_df$budget <- my_df$budget/100000

# Design an interactive treemap visualization for the movies dataset
# ?treemap to customize various features such as color
# Can you change the default tooltip?







ui <- fluidPage(
  
  # Application title
  titlePanel("Movies Visualization"),
  selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
  fluidRow(d3tree2Output("plot", width = "100%", height = "400px"))
)

server <- function(input, output) {
  

  output$plot <- renderD3tree2(d3tree2(treemap(my_df,
                                       index=c("Genre", "title"),
                                       vSize="budget",
                                       vColor="rating",
                                       type="value",
                                       palette = input$palette,
                                       format.legend = list(scientific = FALSE, big.mark = " ")), rootname = "Genre"))
 
}

shinyApp(ui = ui, server = server)