library(shiny)
library(DT)
source("functionsTitle.R")

# Define UI for application that draws a histogram
# names <- c("Hrishikesh Mukherjee", "Sachin Bhowmick")
# titlesKnown <- c("Anand","Jhooth Bole Kauwa Kaate")
# names <- c("Amitabh Bachchan","Jaya Bhaduri")
# titlesKnown <- c("Black","Kal Ho Naa Ho")

ui <- fluidPage(
   
   # Application title
   titlePanel("Find Similar Movies"),
   
   hr(),
   
   fluidRow(
     column(3,
            h4("Pick Names")
     ),
     column(4, offset = 1,
            textInput("nm1", "Enter Name 1", value = "Hrishikesh Mukherjee"),
            textInput("tt1", "Enter Movie 1", value = "Anand")
     ),
     column(4,
            textInput("nm2", "Enter Name 2", value = "Sachin Bhowmick"),
            textInput("tt2", "Enter Movie 2", value = "Jhooth Bole Kauwa Kaate")
     )
   ),
   
   dataTableOutput("movieList"),
   
   plotOutput("timeLine"),
   
   plotOutput("pieCharts")
   
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$movieList <- renderDataTable({
    n <- c("primaryTitle","startYear")
    get.sim.movie(c(input$nm1, input$nm2), c(input$tt1, input$tt2))[[1]][,n]
  })
  
  output$timeLine <- renderPlot({
    ggplot(timeLine.df(c(input$nm1, input$nm2), c(input$tt1, input$tt2)), aes(x=Var1, y=Freq, group=1)) +
      geom_point(stat='summary', fun.y=sum) + 
      stat_summary(fun.y=sum, geom="line") + 
      expand_limits(y = 0) + 
      labs(x = "Decade", y = "Number of co-op movies", title = "Movies Over Time")
  })
  
  output$pieCharts <- renderPlot({
    ggplot(pieCharts.df(c(input$nm1, input$nm2), c(input$tt1, input$tt2)), aes(x="", y=Freq, fill=Var1))+
      geom_bar(stat = "identity", position = position_fill()) +
      geom_text(aes(label = pct), position = position_fill(vjust = 0.5),size = 3) +
      scale_fill_brewer(palette = "RdYlGn", drop = F) +
      labs(fill = "Rate Category") +
      coord_polar("y", start=0) +
      facet_grid(.~type) + blank_theme +
      theme(axis.text.x=element_blank())
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

