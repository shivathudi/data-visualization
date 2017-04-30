packages <- c("devtools", "d3heatmap", "pairsD3", "shiny")

for (i in 1:length(packages)) {
  if(! is.element(packages[i],installed.packages()[,1])) {
    install.packages(packages[i])
  }
}

library(devtools)
devtools::install_github("timelyportfolio/parcoords")
library(parcoords)


library(pairsD3)
library(shiny)
library(d3heatmap)


##### READING AND PRE-PROCESSING DATA
df <- read.csv("dataset_diabetes/diabetic_data.csv")

df_subset <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "gender", "age", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient",  "number_diagnoses"))

df_subset[] <- lapply(df_subset, function(x) if(is.factor(x)) factor(x) else x)

labs_heatmap <- c("Time in Hospital", "Number of Lab Procedures", "Number of Other Procedures", "Number of Medications", "Number of Outpatients", "Number of Emergencies", "Number of Inpatients", "Number of Diagnoses")

original_heatmap <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")

slice_df <- function(df, groups, cols) {
  
  df_agg <- aggregate(as.formula(paste(paste( "cbind(", paste(cols, collapse = ","), ")", sep = ""), "~", paste(groups, collapse = "+"))), data = df, mean)
  
  df_agg$group <- apply( subset(df_agg, select = groups) , 1 , paste , collapse = "-" )
  rownames(df_agg) <- df_agg$group
  
  df_agg <- subset(df_agg, select = cols)
  df_matrix <- round(data.matrix(df_agg),2)
  
  return(df_matrix)
} 

df_pairs <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "gender", "age", "time_in_hospital", "num_lab_procedures", "num_medications", "number_diagnoses"))

# Drop unused levels
df_pairs[] <- lapply(df_pairs, function(x) if(is.factor(x)) factor(x) else x)

labs_pairs <- c("Time in Hospital", "Number of Lab Procedures", "Number of Medications", "Number of Diagnoses")

original_pairs <- c("time_in_hospital", "num_lab_procedures", "num_medications", "number_diagnoses")

sp <- split(df_pairs, list(df_pairs$race, df_pairs$gender, df_pairs$age))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 5, TRUE),])
out <- do.call(rbind, samples)

sample_df_pairs <- out[, c(4, 5, 6, 7, 1, 2, 3)]


df_parcoords <- subset(df, (gender %in% c("Male", "Female")) & (race %in% c("AfricanAmerican", "Asian", "Caucasian",       "Hispanic")), select = c("race", "time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses"))

# Drop unused levels
df_parcoords[] <- lapply(df_parcoords, function(x) if(is.factor(x)) factor(x) else x)

labs_parcoords <- c("Time in Hospital", "Number of Lab Procedures", "Number of Procedures", "Number of Medications", "Number of Diagnoses")

original_parcoords <- c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_diagnoses")

sp <- split(df_parcoords, list(df_parcoords$race))
samples <- lapply(sp, function(x) x[sample(1:nrow(x), 50, FALSE),])
out <- do.call(rbind, samples)

sample_df_coords <- out

colnames(sample_df_coords) <- c("Race", labs_parcoords)



ui<-fluidPage(
  tabsetPanel(
    type='tabs',
    tabPanel("Heatmap",
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
                                         choiceNames = labs_heatmap,
                                         choiceValues = original_heatmap,
                                         inline = T, selected = c("time_in_hospital", "num_lab_procedures", "num_procedures", "num_medications", "number_outpatient", "number_emergency", "number_inpatient", "number_diagnoses")),
                      d3heatmapOutput("heatmap", height = 600)),
    tabPanel("Scatterplot Matrix",
             titlePanel("Scatterplot Matrix"),
             fluidRow(
               column(3,
                      wellPanel(
                        uiOutput("varselect"),
                        uiOutput("facselect1"),
                        uiOutput("facselect2"),
                        radioButtons("theme", "Colour theme",
                                     choices = c("Colour"= "colour",
                                                 "Monochrome"="bw")),
                        #sliderInput("fontsize","Font size",12,min=6,max=24),
                        sliderInput("cex","Size of plotting symbol",3,min=1,max=10),
                        sliderInput("opacity","Opacity of plotting symbol",0.9,min=0,max=1),
                        sliderInput("width","Width and height",600,min=200,max=1200),
                        radioButtons("table_data_logical", label="Table of data?",
                                     choices = c("No" = 0,
                                                 "Yes" = 1)),
                        conditionalPanel("input.table_data_logical==1",
                                         selectInput(inputId="table_data_vars",label="Include all variables in table?",
                                                     choices=c("No" = 0,
                                                               "Yes" = 1))
                        )
                      )
               ),
               column(9,
                      uiOutput("pairsplot"),
                      br(),br(),
                      dataTableOutput(outputId="outputTable")
               )
             )
    ),
    tabPanel("Parallel Coordinates Plot",
             fluidRow(
               column(width=11
                      ,parcoordsOutput( "parcoords", width = "600px", height = "450px" )
               )
               ,column(width=12
                       ,plotOutput( "diabetes_pairs", width = "700px", height = "525px" )
                       , offset = 2
               )
             )
    )
  )

)


server <- function(input, output, session) {

  
  ### HEATMAP
  
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
      labCol = labs_heatmap[which(original_heatmap %in% input$columns)],
      yaxis_width = 240,
      xaxis_height = 180,
      xaxis_font_size = 14
    )
  })
  
  ### SCATTERPLOT MATRIX
  
  output$varselect <- renderUI({
    cols = colnames(sample_df_pairs)
    selectInput("choose_vars", "Select variables to plot",
                choices=cols, selected=cols[1:4], multiple=T)
  })
  
  
  output$facselect1 <- renderUI({
    radioButtons("factor_var_logical", label="Select a factor variable?",
                 choices = c("Yes" = 1,
                             "No" = 0),
                 selected = 1)
  })
  output$facselect2 <- renderUI({
    
    
    conditionalPanel(
      condition = "input.factor_var_logical == 1",
      selectInput(inputId="factor_var",label="Factor variable:",
                  choices=c("race", "gender", "age"), multiple=FALSE,selected = "race")
    )
  })
  
  
  groupvar = reactive({
    if(!is.null(input$factor_var_logical)){
      if(input$factor_var_logical==1){
        return(sample_df_pairs[,input$factor_var])
      }
    }
  })
  
  output$pairsplot <- renderUI({
    pairsD3Output("pD3",width = input$width,height=input$width)
  })
  
  output$export = downloadHandler(
    filename = "pairsD3.html",
    content = function(file){
      savePairs(pairsD3(sample_df_pairs,group=groupvar(), subset=subset, labels = labs_pairs,
                        theme = input$theme,
                        width=input$width,
                        opacity = input$opacity,
                        cex = input$cex),
                file=file)
    }
  )
  
  output$pD3 <- renderPairsD3({
    pairsD3(sample_df_pairs[,choices()],group=groupvar(), subset=subset, labels = labs_pairs,
            theme = input$theme, big=TRUE,
            opacity = input$opacity,
            cex = input$cex)
  })
  
  
  choices<-reactive({
    input$choose_vars
  })
  
  output$outputTable = renderDataTable({
    data = sample_df_pairs
    if(input$table_data_logical==1){
      displayDF <- as.matrix(data) # baseData$df #data sent to d3.js
      n=dim(displayDF)[1]
      dfFilter <- input$selectedobs[1:n] # passed from the web interface
      if (is.null(dfFilter)){
        # no selection has been made
        dfFilter = rep(TRUE,n)
      }
      displayDF <- as.data.frame(cbind(names=row.names(displayDF),
                                       displayDF))
      dfFilter[dfFilter==''] = TRUE
      dfFilter[dfFilter=='greyed'] = FALSE
      if(input$table_data_vars==0){
        return(as.matrix(displayDF[dfFilter == TRUE,choices(),drop=FALSE]))
      } else if(input$table_data_vars==1){
        return(as.matrix(displayDF[dfFilter == TRUE,,drop=FALSE]))
      }
    } else {
      return(NULL)
    }
  },
  options = list(dom = 't<lp>',pageLength = 20,
                 autoWidth = TRUE,
                 lengthMenu = list(c(20, 50, -1), c('20', '50', 'All')),
                 searching = FALSE)
  )
  
  output$parcoords = renderParcoords(
    parcoords(
      sample_df_coords  # order columns so species first
      , rownames=F
      , width = 1000
      , reorderable = T
      , axisDots = T
      , brushMode="1d"
      , color = list(
        colorScale = htmlwidgets::JS(sprintf(
          'd3.scale.ordinal().range(%s).domain(%s)'
          ,jsonlite::toJSON(RColorBrewer::brewer.pal(4,'Set1'))
          ,jsonlite::toJSON(as.character(unique(sample_df_coords$Race)))
        ))
        ,colorBy = "Race"
      )
    )
  )
  
  output$diabetes_pairs = renderPlot({
    rows <- if(length(input$parcoords_brushed_row_names) > 0) {
      input$parcoords_brushed_row_names
    } else {
      rownames(sample_df_coords)
    }
    # example from ?pairs
    pairs(
      sample_df_coords[rows,-1],
      labs_parcoords
      , main = "Diabetes Data -- 4 Races"
      , pch = 21
      , bg = RColorBrewer::brewer.pal(4,'Set1')[unclass(sample_df_coords[rows,]$Race)]
    )
  })
}
  
shinyApp(ui, server)