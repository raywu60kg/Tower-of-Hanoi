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
    titlePanel("Tower of Hanoi"),
    actionButton("hold", "Hold"),
    actionButton("move_left", "Move left"),
    actionButton("move_right", "Move right"),
    plotOutput("plot")
)


# server <- function(input, output){
#     v <- reactiveValues(data = tower_status)
#     observeEvent(input$hold, {
#         v$data = hold(v$data)
#     })
#     observeEvent(input$move_left, {
#         v$data = move_left(v$data)
#     })
#     observeEvent(input$move_right, {
#         v$data = move_right(v$data)
#     })
#     output$plot <- renderPlot({
#         plot_tower_status(v$data)
#     })
# }

server <- function(input, output){
    v <- reactiveValues(data = tower_status)
    observeEvent(input$hold, {
        v$data = hold2(v$data)
    })
    observeEvent(input$move_left, {
        v$data = move(v$data,-1)
    })
    observeEvent(input$move_right, {
        v$data = move(v$data,+1)
    })
    output$plot <- renderPlot({
        plot_tower_status(v$data)
    })
}
    
shinyApp(ui, server)