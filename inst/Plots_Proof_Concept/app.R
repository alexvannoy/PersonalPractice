library(plotly)
library(shiny)
iris = iris
pcaObject = princomp(iris[, 1:4], cor = TRUE)
irisDf = data.frame(pcaObject$scores, "Group" = iris$Species)

ui <- fluidPage(plotlyOutput("plot"),
                verbatimTextOutput("hover"),
                verbatimTextOutput("click"))

server <- function(input, output, session) {
    p <- plot_ly(
        irisDf,
        x = irisDf$Comp.1,
        y = irisDf$Comp.2,
        z = irisDf$Comp.3,
        type = "scatter3d",
        color = irisDf$Group
    )

    p <-
        layout(
            p,
            title = "PCA of Iris Data",
            scene = list(xaxis = list(title = "PC1"),
                         yaxis = list(title = "PC2"),
                         zaxis = list(title = "PC3"))
        )
    output$plot <- renderPlotly({
        p
    })

    output$hover <- renderPrint({
        d <- event_data("plotly_hover")
        if (is.null(d))
            "Hover events appear here (unhover to clear)"
        else
            d
    })

    output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d))
            "Click events appear here (double-click to clear)"
        else
            d
    })

}

shinyApp(ui, server)
