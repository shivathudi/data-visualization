library(d3heatmap)
library(shiny)


df <- read.csv("dataset_diabetes/diabetic_data.csv")

df_subset <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "gender", "age", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient",  "number_diagnoses"))

df_subset[] <- lapply(df_subset, function(x) if(is.factor(x)) factor(x) else x)

labs <- c("Time in Hospital", "Number of Lab Procedures", "Number of Other Procedures", "Number of Medications", "Number of Outpatients", "Number of Emergencies", "Number of Inpatients", "Number of Diagnoses")

original <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")

slice_df <- function(df, groups, cols) {
  
  df_agg <- aggregate(as.formula(paste(paste( "cbind(", paste(cols, collapse = ","), ")", sep = ""), "~", paste(groups, collapse = "+"))), data = df, mean)
  
  df_agg$group <- apply( subset(df_agg, select = groups) , 1 , paste , collapse = "-" )
  rownames(df_agg) <- df_agg$group
  
  df_agg <- subset(df_agg, select = cols)
  df_matrix <- data.matrix(df_agg)
  
  return(df_matrix)
} 

ui <- fluidPage(
  h1("Diabetes Dataset Heatmap"),
  selectInput("palette", "Palette", c("YlOrRd", "RdYlBu", "Greens", "Blues")),
  checkboxGroupInput("groups","Select Groups",
                     choiceNames =
                       list("Race", "Gender", "Age"),
                     choiceValues =
                       list("race", "gender", "age"),
                     inline = T, selected = c("race", "gender")),
  checkboxInput("cluster", "Apply clustering and get dendogram"),
  checkboxGroupInput("columns","Select Columns",
                     choiceNames = labs,
                     choiceValues = original,
                     inline = T, selected = c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")),
  d3heatmapOutput("heatmap", height = 600)
)

server <- function(input, output, session) {
  
  df_selection <- reactive(
    try(
      slice_df(df_subset, input$groups, input$columns)
      )
    )
  
  
  
  
  output$heatmap <- renderD3heatmap({
    d3heatmap(
      scale(df_selection()),
      colors = input$palette,
      dendrogram = if (input$cluster) "both" else "none",
      digits = 2,
      cellnote = df_selection(),
      labCol = labs[which(original %in% input$columns)],
      yaxis_width = 240,
      xaxis_height = 180,
      xaxis_font_size = 14
    )
  })
}

shinyApp(ui, server)