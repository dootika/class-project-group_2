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
          "Season",
          "Broadcast Day",
          "Genre"
        )
      ),
      
      selectInput(
        "XaxisMetric",
        "Indedendent Parameter",
        c("No. Of Episodes",
          "Studio",
          "Duration per Episode",
          "Broadcast Time"
        )
      ),
      # Reactive Slider Input
      uiOutput("CountInput"),
      ),
    
    
    
    # Designs the main panel of the main page (Mainly for plot outputs)
    mainPanel(
      # Title of the Main Panel
      h4("Main Panel"),
    
      tabsetPanel(
        tabPanel("Scatter Plot", plotOutput("ScatPlot")),
        tabPanel("Bar Plot", plotOutput("Barplot")),
        tabPanel("Table", tableOutput("TablePlot"))
      )
      
    )
  ),
  theme = shinytheme("superhero")
  
)

# Define server logic ----
server <- function(input, output) {
  output$ScatPlot <- renderPlot({
    subdata <- subset(MainData, 
                      switch (
                        input$XaxisMetric,
                        `No. Of Episodes` = (MainData$Episode.Count<input$Count),
                        `Studio` = (MainData$Studio == input$Count),
                        `Duration per Episode` = (MainData$Duration.per.Episode<input$Count),
                        `Broadcast Time` = TRUE
                      )&
                      (MainData$Rating == input$AgeGroup)&
                      (MainData$Source == input$Source)
                      )
    Y_Param <- switch (input$YaxisMetric,
      Rating = subdata$Score,
      `No. of Votes` = subdata$Scored.by,
      Viewers = subdata$Members,
      Favourites = subdata$Favorites
      
    )
    X_Param <- switch (input$XaxisMetric,
                       `No. Of Episodes` = subdata$Episode.Count,
                       `Studio` = subdata$Studio,
                       `Duration per Episode` = subdata$Duration.per.Episode,
                       `Broadcast Time` = subdata$Broadcast.JST
                       
    )
    Subdiv_Param <- switch (input$SubDivParam,
                            `Source` = subdata$Source,
                            `Season` = subdata$Season,
                            `Broadcast Day` = subdata$Broadcast.Day,
                            `Genre` = subdata$Genre
                       
    )
    # draw the plot
    ggplot(subdata, 
           mapping = aes(X_Param, Y_Param),
           )+
      geom_point(mapping = aes( col = Subdiv_Param))+
      labs(y= input$YaxisMetric, x = input$XaxisMetric, col = input$SubDivParam)
  })
  
  output$CountInput <- renderUI(
    switch (
      input$XaxisMetric,
      `No. Of Episodes` = sliderInput("Count",
                                      "Maximum Number of Episodes:",
                                      min = 1,
                                      max = 100,
                                      value = 50),
      `Studio` = checkboxGroupInput("Count",
                                    "Select Studios:",
                                    unique(MainData$Studio)),
      `Duration per Episode` = sliderInput("Count",
                                           "Max Duration Per Episode:",
                                           min = 1,
                                           max = 100,
                                           value = 50),
      `Broadcast Time` = ''
                                     
  ))
  
  output$Barplot <- renderPlot({
    subdata <- subset(MainData, 
                      switch (
                        input$XaxisMetric,
                        `No. Of Episodes` = (MainData$Episode.Count<input$Count),
                        `Studio` = (MainData$Studio == input$Count),
                        `Duration per Episode` = (MainData$Duration.per.Episode<input$Count),
                        `Broadcast Time` = TRUE
                      )&
                        (MainData$Rating == input$AgeGroup)&
                        (MainData$Source == input$Source)
    )
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
    ggplot(subdata, 
           mapping = aes(Y_Param),
    )+
      geom_histogram(mapping = aes(col = Subdiv_Param))+
      labs(y= "No. of Observations", x = input$YaxisMetric, col = input$SubDivParam)
  })
  
  output$TablePlot <- renderTable({
    subdata <- subset(MainData, 
                      switch (
                        input$XaxisMetric,
                        `No. Of Episodes` = (MainData$Episode.Count<input$Count),
                        `Studio` = (MainData$Studio == input$Count),
                        `Duration per Episode` = (MainData$Duration.per.Episode<input$Count),
                        `Broadcast Time` = TRUE
                      )&
                        (MainData$Rating == input$AgeGroup)&
                        (MainData$Source == input$Source)
    )
    newDF <- data.frame(
      Rank = subdata$Rank,
      Title = subdata$Anime.Title,
      Rating = subdata$Score,
      `Active Viewers` = subdata$Scored.by
    )
    newDF
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)