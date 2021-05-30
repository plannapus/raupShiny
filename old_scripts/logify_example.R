library(shiny)

# logifySlider javascript function
JS.logify <-"function logifySlider (sliderId, sci = false) {
    // regular number style
    $('#'+sliderId).data('ionRangeSlider').update({
      'prettify': function (num) { return (Math.pow(10, num).toFixed(1)); }
    })
}"
JS.onload <-"
$(document).ready(function() {
  setTimeout(function() {
    logifySlider('W', sci = false)
  }, 5)})
"

ui <- fluidPage(
  tags$head(tags$script(HTML(JS.logify))),
  tags$head(tags$script(HTML(JS.onload))),
  sliderInput("log_slider", "Log Slider (numbers):",
              min = -1.31, max = 5, value = 0, step = .001),
  
  sliderInput("log_slider2", "Log Slider (sci. notation):",
              min = -5, max = 3, value = 1, step = 0.5),
  
  br(),
  
  textOutput("readout1"),
  textOutput("readout2")
)

server <- function(input, output, session) {
  output$readout1 <- reactive({
    paste0("Selected value (numbers): ", input$log_slider, " = ", 10^input$log_slider)
  })
  
  output$readout2 <- reactive({
    paste0("Selected value (sci. notation): ", input$log_slider2, " = ", 10^input$log_slider2)
  })
}

shinyApp(ui, server)