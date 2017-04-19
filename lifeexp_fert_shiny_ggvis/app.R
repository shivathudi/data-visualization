library(reshape2)
library(shiny)
library(ggvis)
library(dplyr)

## DATA PROCESSING

lifexp <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, skipNul = TRUE, check.names = FALSE)

fertility <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, skipNul = TRUE, check.names = FALSE)

population <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2.csv", skipNul = TRUE, check.names = FALSE)

drops <- c("Indicator Name", "Indicator Code", "2015", "2016", "")

lifexp <- melt(lifexp[, !(names(lifexp) %in% drops)], value.name = "Life.Expectancy", variable.name = "Year")
fertility <- melt(fertility[, !(names(fertility) %in% drops)], value.name = "Fertility.Rate", variable.name = "Year" )
population <- melt(population[, !(names(population) %in% drops)], value.name = "Population", variable.name = "Year")

region_info <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", check.names = FALSE, colClasses = "character")
region_info <- subset(region_info, select = c("Country Code", "Region"), Region!="")

data <- merge(lifexp, fertility)
data <- merge(data, population)
data <- merge(data, region_info)

names(data)[2]  <- "Country.Name"

rm(lifexp, fertility, region_info, population)

data$Region <- as.factor(data$Region)
data$Year <- as.numeric(as.character(data$Year))

regions <- append(levels(data$Region), "All")

#### UI

ui <- fluidPage(
  headerPanel("HW2 - Shivakanth Thudi"),
  
  
  sidebarPanel(sliderInput("year", "Select a year",
                           min = 1960, max = 2014, value = 1960, sep = "",
                           animate = animationOptions(interval = 100, loop = TRUE)),
               radioButtons("region", "Select a region", regions, selected = "All")
  ),
  mainPanel(
    ggvisOutput("plot"),
    uiOutput("plot_ui")
  )
)

### SERVER

server <- function(input, output) {
  plot_year <- reactive(input$year)
  plot_reg <- reactive(input$region)
  
  all_values <- function(x) {
    if(is.null(x)) return(NULL)
    row <- data[(data$Country.Name == x$Country.Name) & (data$Year == plot_year()), ]
    paste0("Country: ", row$Country.Name, "</br>", 
           "Population (in millions): ", format(row$Population/1000000, digits=2, nsmall=2), "</br>",
           "Life Expectancy: ", format(row$Life.Expectancy, digits=2, nsmall=1), "</br>",
           "Fertility Rate: ", format(row$Fertility.Rate, digits=2, nsmall=1))
  }
  
  data$radius <- data$Population / 200000
  df_sub <- reactive({data %>% 
      filter(Year==plot_year()) %>%
      arrange(desc(Population)) %>%
      mutate(opacity = 0.8) %>%
      mutate(opacity = ifelse(Region==plot_reg() | plot_reg()=="All", 0.8, 0.2))})
  
  df_sub %>% 
    ggvis(~Life.Expectancy, ~Fertility.Rate, fill = ~Region, key := ~Country.Name, stroke := 'black', strokeWidth := 0.25) %>%
    add_tooltip(all_values, "hover") %>%
    layer_points(size := ~radius, fillOpacity := ~opacity, strokeOpacity := ~opacity, size.hover := ~radius+100, 
                 strokeWidth.hover := 1.0, fillOpacity.hover := 1.0, strokeOpacity.hover := 1.0) %>%
    add_axis("x", title="Life Expectancy") %>%
    add_axis("y", title="Fertility Rate") %>%
    scale_numeric("x", domain = c(10, 90), nice = FALSE) %>%
    scale_numeric("y", domain = c(0, 9), nice = FALSE) %>%
    bind_shiny("plot", "plot_ui")
}

shinyApp(ui = ui, server = server)




