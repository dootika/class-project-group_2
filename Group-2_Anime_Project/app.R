# Reading the Data and sorting out various plot types
MainData <- read.csv("../Data\ Scraping\ and\ Cleaning/Anime\ Dataframe.csv")

# Categories of the anime and their frequency
categories <- table(MainData$Source) 

library(shiny)

# Define UI ----
ui <- fluidPage(
  # Hosts the Title of Our Anime Page
  titlePanel("Anime Analyser 4000"),
  
  # Designs the Main Page Layout
  sidebarLayout(
    # Designs the sidebar for the page (Mainly for Inputs)
    sidebarPanel(
      h4("Sidebar Panel"),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
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
    # draw the histogram with the specified number of bins
    plot(categories)
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)