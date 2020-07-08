# library(ggplot2)
# library(Cairo)   # For nicer ggplot2 output when deployed on Linux
# library(shiny)
# We'll use a subset of the mtcars data set, with fewer columns
# so that it prints nicely
# mtcars2 <- mtcars[, c("mpg", "cyl", "disp", "hp", "wt", "am", "gear")]

saving_actions <- function(which, input){
  shinyalert("Okey!", paste0("New ", which, " saved."), type = "success")
  saveRDS(list(xmin = input$plot1_brush$xmin, xmax = input$plot1_brush$xmax,
               ymin = input$plot1_brush$ymin, ymax = input$plot1_brush$ymax),
          file=paste0(which,"_limits.RDS"))
}

ui <- fluidPage(
  useShinyalert(),  # Set up shinyalert
  fluidRow(
    column(width = 4,
           plotOutput("plot1", height = 300,
                      # Equivalent to: click = clickOpts(id = "plot_click")
                      # click = "plot1_click",
                      brush = brushOpts(
                        id = "plot1_brush"
                      )
           )
    )
  ),
  fluidRow(
    # column(width = 6,
    #        h4("Points near click")
    #        # verbatimTextOutput("click_info")
    # ),
    column(width = 6,
           h4("Selected area"),
           verbatimTextOutput("brush_info"),
           actionButton("background_button", "Save as background"),
           actionButton("object_button", "Save as an object")
    )
  )
)

server <- function(input, output, session) {
  output$plot1 <- renderPlot({
    ggplot(image_df, aes(column, row)) +
      geom_point(aes(color=node_value)) +
      scale_color_gradient(low="black", high="white") +
      theme(legend.position="none")
  })

  output$brush_info <- renderText({
    paste0("x min: ", input$plot1_brush$xmin, " \nx max: ", input$plot1_brush$xmax,
           "\ny min: ", input$plot1_brush$ymin, " \ny max: ", input$plot1_brush$ymax)
  })

  observeEvent(input$background_button, {
    saving_actions("background", input)
  })

  observeEvent(input$object_button, {
    saving_actions("object", input)
  })

}

# shinyApp(ui, server)
