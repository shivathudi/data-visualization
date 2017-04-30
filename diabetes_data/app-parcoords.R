# not on CRAN so use devtools::install_github to try it out
# devtools::install_github("timelyportfolio/parcoords")

library(parcoords)

df_ids <- read.csv(("dataset_diabetes/IDs_mapping.csv"))
df <- read.csv("dataset_diabetes/diabetic_data.csv")

df_subset <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses"))

# Drop unused levels
df_subset[] <- lapply(df_subset, function(x) if(is.factor(x)) factor(x) else x)

labs <- c("Time in Hospital", "Number of Lab Procedures", "Number of Procedures", "Number of Medications", "Number of Diagnoses")

original <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses")

sp <- split(df_subset, list(df_subset$race))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 50, FALSE),])
out <- do.call(rbind, samples)

sample_df <- out

colnames(sample_df) <- c("Race", labs)

ui = shinyUI(
  fluidPage(fluidRow(
    column(width=11
           ,parcoordsOutput( "parcoords", width = "600px", height = "450px" )
    )
    ,column(width=12
            ,plotOutput( "diabetes_pairs", width = "700px", height = "525px" )
            , offset = 2
    )
  ))
)

server = function(input,output,session){
  output$parcoords = renderParcoords(
    parcoords(
      sample_df  # order columns so species first
      , rownames=F
      , width = 1000
      , reorderable = T
      , axisDots = T
      , brushMode="1d"
      , color = list(
        colorScale = htmlwidgets::JS(sprintf(
          'd3.scale.ordinal().range(%s).domain(%s)'
          ,jsonlite::toJSON(RColorBrewer::brewer.pal(4,'Set1'))
          ,jsonlite::toJSON(as.character(unique(sample_df$Race)))
        ))
        ,colorBy = "Race"
      )
    )
  )
  
  output$diabetes_pairs = renderPlot({
    rows <- if(length(input$parcoords_brushed_row_names) > 0) {
      input$parcoords_brushed_row_names
    } else {
      rownames(sample_df)
    }
    # example from ?pairs
    pairs(
      sample_df[rows,-1],
      labs
      , main = "Diabetes Data -- 4 Races"
      , pch = 21
      , bg = RColorBrewer::brewer.pal(4,'Set1')[unclass(sample_df[rows,]$Race)]
    )
  })
}

shinyApp(ui,server)
