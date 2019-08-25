#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# source("/home/wu/projects/Tower-of-Hanoi/Tower-of-Hanoi/utils.R")
source("utils.R")
ui <- fluidPage(
    
    # Application title
    # titlePanel("Tower of Hanoi"),
    # Sidebar with a slider input for number of bins
    # sidebarLayout(
    #     sidebarPanel(
    #         sliderInput("bins",
    #                     "Number of bins:",
    #                     min = 1,
    #                     max = 50,
    #                     value = 30)
    #     ),
    # 
    #     # Show a plot of the generated distribution
    #     mainPanel(
    #         plotOutput("distPlot")
    #     )
    # ),
    actionButton("hold", "Hold"),
    actionButton("move_left", "Move left"),
    actionButton("move_right", "Move right"),
    plotOutput("plot")
)


server <- function(input, output){
    v <- reactiveValues(data = tower_status)
    observeEvent(input$hold, {
        v$data = hold(v$data)
        # print(v$data)
    })
    observeEvent(input$move_left, {
        v$data = move_left(v$data)
    })
    observeEvent(input$move_right, {
        v$data = move_right(v$data)
    })
    
    output$plot <- renderPlot({
        # print("=======")
        # print(v$data)
        # if (is.null(v$data)) return()
        plot_tower_status(v$data)
    })
    
}
shinyApp(ui, server)