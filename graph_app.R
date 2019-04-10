library(shiny)
# source("functionsGraph.R")

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   navbarPage (
     "Network Graphs",
       tabPanel("Point Network",  
                selectInput("simpleNet", "Choose Family", unique(IC$last_name)),
                visNetworkOutput("network1")),
       tabPanel("Community Detection Graph",  
                selectInput("comDet", "Choose Family", unique(IC$last_name)),
                plotOutput("plot", height = "800px")),
      tabPanel("Community Members", 
               sidebarLayout(
                 sidebarPanel(
                   selectInput("cm", "Choose Family", unique(IC$last_name))
                   ),
               mainPanel(
                 tableOutput("comMem")
                 )
               )
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$network1 <- renderVisNetwork({
    visNetwork(edges = get_edges(input$simpleNet), nodes = get_nodes(input$simpleNet), height = "1000px") %>%
      visIgraphLayout(layout = "layout_nicely") %>%
      visNodes(size = 10) %>%
      visOptions(highlightNearest = list(enabled = T, hover = T),
                 nodesIdSelection = T)
  })
  output$plot <- renderPlot({
    plot(communityDetection(input$comDet)[[1]], layout = communityDetection(input$comDet)[[2]])
  })
  output$comMem <- renderTable({
    by.community(input$cm)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


