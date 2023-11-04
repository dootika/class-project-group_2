# Reading the Data and sorting out various plot types
MainData <- read.csv("../Finaldata.csv")

# Categories of the anime and their frequency
categories <- table(MainData$Source) 

library(shiny)
library(ggplot2)
library(shinythemes)
shinytheme("superhero")
# Define UI ----
ui <- fluidPage(
  # Hosts the Title of Our Anime Page
  titlePanel("Anime Analyser 4000"),
  
  # Designs the Main Page Layout
  sidebarLayout(
    # Designs the sidebar for the page (Mainly for Inputs)
    sidebarPanel(
      h4("Sidebar Panel"),
      selectInput(
        "Source",
        "Source of Anime",
        unique(MainData$Source)
      ),
      selectInput(
        "YaxisMetric",
        "Comparision Parameter",
        c("Rating",
          "No. of Votes",
          "Viewers",
          "Favourites"
          )
      ),
      selectInput(
        "AgeGroup",
        "Target Audience",
        unique(MainData$Rating)
      ),
      selectInput(
        "SubDivParam",
        "Sub Division Parameter",
        c(
          "Source",
          "Season",
          "Broadcast Day",
          "Genre"
        )
      ),

      sliderInput("EpiCount",
                  "Maximum Number of Episodes:",
                  min = 1,
                  max = 100,
                  value = 50)
      ),
    
    
    # Designs the main panel of the main page (Mainly for plot outputs)
    mainPanel(
      # Title of the Main Panel
      h4("Main Panel"),
    
      tabsetPanel(
        tabPanel("Plot", plotOutput("distPlot")),
        tabPanel("Summary", verbatimTextOutput("summary")),
        tabPanel("Table", tableOutput("table"))
      )
      
    )
  ),
  theme = shinytheme("superhero")
  
)

# Define server logic ----
server <- function(input, output) {
  output$distPlot <- renderPlot({
    subdata <- subset(MainData, (MainData$Episode.Count<input$EpiCount)&(MainData$Rating == input$AgeGroup) )
    Y_Param <- switch (input$YaxisMetric,
      Rating = subdata$Score,
      `No. of Votes` = subdata$Scored.by,
      Viewers = subdata$Members,
      Favourites = subdata$Favorites
      
    )
    Subdiv_Param <- switch (input$SubDivParam,
                            `Source` = subdata$Source,
                            `Season` = subdata$Season,
                            `Broadcast Day` = subdata$Broadcast.Day,
                            `Genre` = subdata$Genre
                       
    )
    # draw the plot
    ggplot(subdata, mapping = aes(Episode.Count, Y_Param))+
           geom_point(mapping = aes( col = Subdiv_Param))
  })
  
}

# Run the app ----
shinyApp(ui = ui, server = server)