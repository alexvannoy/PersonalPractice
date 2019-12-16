library(shiny)

logging::setLevel("DEBUG")
source("./MortgageHelpers.R")


ui <- fluidPage(# App title ----
                titlePanel("Mortgage Information"),

                # Sidebar layout with input and output definitions ----
                sidebarLayout(
                  # Sidebar panel for inputs ----
                  sidebarPanel(

                    textInput(
                      inputId = "Income",
                      label = "Income:",
                      value = "50000"
                    ),

                    textInput(
                      inputId = "Mortgage",
                      label = "Mortgage",
                      value = "160000"
                    ),

                    textInput(
                      inputId = "InterestRate",
                      label = "Interest Rate:",
                      value = "3.88"
                    ),

                    selectInput(
                      inputId = "Years",
                      label = "Years",
                      choices = c(15, 30),
                      selected = 30
                    )

                  ),

                  # Main panel for displaying outputs ----
                  mainPanel(tableOutput(outputId = "MortgageSummary"))
                ))

server <- function(input, output) {
  # Mortgage calculator ----

  MaxPayment = reactive({
    logging::logdebug("Computing maximum suggested payment")
    maxPayment = suggested_maximum_housing_cost(income = as.numeric(input$Income))
    return(maxPayment)
  })

  mortgageSummaryObject = reactive({
    mortgageSummary = list()
    logging::logdebug("Computing mortgage payment")
    mortgageSummary$payment = mortgage_payment_calculator(
      years = as.numeric(input$Years),
      principal = as.numeric(input$Mortgage),
      interestRate = as.numeric(input$InterestRate)
    )
    mortgageSummary$maxPayment = MaxPayment()
    mortgageSummary$maxMortgage = max_mortgage_calculator(years = as.numeric(input$Years),
                                                          payment = mortgageSummary$maxPayment,
                                                          interestRate = as.numeric(input$InterestRate))
    mortgageSummary$percentOfIncome = percentage_of_income(income = as.numeric(input$Income),
                                                           payment = mortgageSummary$payment)

    logging::logdebug("Computing difference in payment and maximum suggested payment")
    mortgageSummary$paymentDiff = mortgageSummary$maxPayment - mortgageSummary$payment

    mortgageSummary = as.data.frame(mortgageSummary)
    return(mortgageSummary)
  })


  output$MortgageSummary <- renderTable({
    mortgageSummaryObject()
  })

}

shinyApp(ui = ui, server = server)
