library(shiny)
source("utils.R")
ui <- fluidPage(
    titlePanel("Tower of Hanoi"),
    actionButton("hold", "Hold"),
    actionButton("move_left", "Move left"),
    actionButton("move_right", "Move right"),
    plotOutput("plot")
)

server <- function(input, output){
    v <- reactiveValues(data = tower_status)
    observeEvent(input$hold, {
        v$data = hold(v$data)
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