library(shiny)
library(TTR)
library(quantmod)

if (!exists(".inflation")) {
  .inflation <- getSymbols('CPIAUCNS', src = 'FRED',
                           auto.assign = FALSE)
}

# adjusts Google finance data with the monthly consumer price index
# values provided by the Federal Reserve of St. Louis
# historical prices are returned in present values
adjust <- function(data) {

  latestcpi <- last(.inflation)[[1]]
  inf.latest <- time(last(.inflation))
  months <- split(data)

  adjust_month <- function(month) {
    date <- substr(min(time(month[1]), inf.latest), 1, 7)
    coredata(month) * latestcpi / .inflation[date][[1]]
  }

  adjs <- lapply(months, adjust_month)
  adj <- do.call("rbind", adjs)
  axts <- xts(adj, order.by = time(data))
  axts[ , 5] <- Vo(data)
  axts
}


ui <- fluidPage(

  # App title ----
  titlePanel("Stock Visualizing Example"),

  # Sidebar layout with input and output definitions ----
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      textInput(inputId = "symbol",
                label = "Stock Symbol:",
                value = "GOOG"),

      dateRangeInput(inputId = "dates",
                     label = "Date Range",
                     start = "2007-01-01",
                     end = "2019-01-01"),

      checkboxInput(inputId = "log",
                    label = "Plot y-axis on log scale",
                    value = FALSE),

      checkboxInput(inputId = "adjust",
                    label = "Adjust prices for inflation",
                    value = FALSE)

    ),

    # Main panel for displaying outputs ----
    mainPanel(

      plotOutput(outputId = "plot")

    )
  )
)

server <- function(input, output) {

  dataInput <- reactive({
    getSymbols(input$symbol, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })

  finalInput <- reactive({
    if (!input$adjust) return(dataInput())
    adjust(dataInput())
  })

  output$plot <- renderPlot({
    chartSeries(finalInput(), theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
}

shinyApp(ui = ui, server = server)
