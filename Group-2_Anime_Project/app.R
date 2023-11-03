# Reading the Data and sorting out various plot types
MainData <- read.csv("../Data\ Scraping\ and\ Cleaning/Anime\ Dataframe.csv")

# Categories of the anime and their frequency
categories <- table(MainData$Source) 

library(shiny)
library(ggplot2)
# Define UI ----
ui <- fluidPage(
  # Hosts the Title of Our Anime Page
  titlePanel("Anime Analyser 4000"),
  
  # Designs the Main Page Layout
  sidebarLayout(
    # Designs the sidebar for the page (Mainly for Inputs)
    sidebarPanel(
      h4("Sidebar Panel"),
      
      sliderInput("EpiCount",
                  "Maximum Number of Episodes:",
                  min = 1,
                  max = 100,
                  value = 50)
      ),
    
    
    # Designs the main panel of the main page (Mainly for plot outputs)
    mainPanel(
      # Title of the Main Panel
      h4("main panel"),
      
      # Plot Output - 1
      plotOutput("distPlot")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    subdata <- subset(MainData, MainData$Episode.Count <input$EpiCount )
    # draw the histogram with the specified number of bins
    ggplot(subdata, mapping = aes(Episode.Count, Score))+
           geom_point(mapping = aes( col = Source))
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)