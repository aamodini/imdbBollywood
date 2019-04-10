library(shiny)
library(DT)
# source("intersectionFunc.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Find Similar Movies"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        textInput("nm1", "Enter Name 1", value = "Hrishikesh Mukherjee"),
        textInput("tt1", "Enter Movie 1", value = "Anand"),
        textInput("nm2", "Enter Name 2", value = "Sachin Bhowmick"),
        textInput("tt2", "Enter Movie 2", value = "Jhooth Bole Kauwa Kaate")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        verbatimTextOutput("value")
        #tabPanel("mtcars", DT::dataTableOutput("dataTable"))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$value <- renderPrint({
    name.title.characters(c(input$nm1, input$nm2), c(input$tt1, input$tt2))
    # staff_movie[, colnames(staff_movie) == c(input$nm1, input$nm2)]
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

