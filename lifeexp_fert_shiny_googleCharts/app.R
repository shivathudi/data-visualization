library(reshape2)
library(magrittr)
library(shiny)


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

rm(lifexp, fertility, region_info, population)

data$Region <- as.factor(data$Region)
data$Year <- as.numeric(as.character(data$Year))

#### UI

# More info:
#   https://github.com/jcheng5/googleCharts
# Install:
# devtools::install_github("jcheng5/googleCharts")
library(googleCharts)

# Use global max/min for axes so the view window stays
# constant as the user moves between years
xlim <- list(
  min = min(data$Fertility.Rate) - 0.1,
  max = max(data$Fertility.Rate) + 0.1
)
ylim <- list(
  min = min(data$Life.Expectancy),
  max = max(data$Life.Expectancy) + 3
)

ui <- fluidPage(
  # This line loads the Google Charts JS library
  googleChartsInit(),
  
  # Use the Google webfont "Source Sans Pro"
  tags$link(
    href=paste0("http://fonts.googleapis.com/css?",
                "family=Source+Sans+Pro:300,600,300italic"),
    rel="stylesheet", type="text/css"),
  tags$style(type="text/css",
             "body {font-family: 'Source Sans Pro'}"
  ),
  
  h2("Plot using googleCharts (Shiva Thudi)"),
  
  googleBubbleChart("chart",
                    width="100%", height = "475px",
                    # Set the default options for this chart; they can be
                    # overridden in server.R on a per-update basis. See
                    # https://developers.google.com/chart/interactive/docs/gallery/bubblechart
                    # for option documentation.
                    options = list(
                      fontName = "Source Sans Pro",
                      fontSize = 13,
                      # Set axis labels and ranges
                      hAxis = list(
                        title = "Life expectancy (years)",
                        viewWindow = xlim
                      ),
                      vAxis = list(
                        title = "Fertility rate",
                        viewWindow = ylim
                      ),
                      # The default padding is a little too spaced out
                      chartArea = list(
                        top = 50, left = 75,
                        height = "75%", width = "75%"
                      ),
                      # Allow pan/zoom
                      explorer = list(),
                      # Set bubble visual props
                      bubble = list(
                        opacity = 0.4, stroke = "none",
                        # Hide bubble label
                        textStyle = list(
                          color = "none"
                        )
                      ),
                      # Set fonts
                      titleTextStyle = list(
                        fontSize = 16
                      ),
                      tooltip = list(
                        textStyle = list(
                          fontSize = 12
                        )
                      )
                    )
  ),
  fluidRow(
    shiny::column(4, offset = 4,
                  sliderInput("year", "Year",
                              min = min(data$Year), max = max(data$Year),
                              value = min(data$Year), animate = TRUE)
    )
  )
)

### SERVER

library(dplyr)

server <- function(input, output, session) {
  
  # Provide explicit colors for regions, so they don't get recoded when the
  # different series happen to be ordered differently from year to year.
  # http://andrewgelman.com/2014/09/11/mysterious-shiny-things/
  defaultColors <- c("#3366cc", "#dc3912", "#ff9900", "#109618", "#990099", "#0099c6", "#dd4477")
  series <- structure(
    lapply(defaultColors, function(color) { list(color=color) }),
    names = levels(data$Region)
  )
  
  yearData <- reactive({
    # Filter to the desired year, and put the columns
    # in the order that Google's Bubble Chart expects
    # them (name, x, y, color, size). Also sort by region
    # so that Google Charts orders and colors the regions
    # consistently.
    df <- data %>%
      filter(Year == input$year) %>%
      select(2, Life.Expectancy, Fertility.Rate,
             Region, Population) %>%
      arrange(Region)
  })
  
  output$chart <- reactive({
    # Return the data and options
    list(
      data = googleDataTable(yearData()),
      options = list(
        title = sprintf(
          "Fertility Rate vs. Life Expectancy, %s",
          input$year),
        series = series
      )
    )
  })
}



shinyApp(ui = ui, server = server)