require(praise)

ui = fluidPage(# App title ----
               titlePanel("Complement Page!"),

               # An action button
               sidebarLayout(
                 actionButton("goButton", label = "Complement me"),

                 #The complement output
                 mainPanel(textOutput(outputId = "ntext"))
               ))

server = function(input, output) {
  ntext <- eventReactive(input$goButton, {
    praise::praise()
  })

  output$ntext <- renderText({
    ntext()
  })
}

shinyApp(ui = ui, server = server)
