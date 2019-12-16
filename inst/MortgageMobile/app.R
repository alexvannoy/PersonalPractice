library(shiny)
library(shinyMobile)

logging::setLevel("DEBUG")
#'
#' mortgage_payment_calculator
#'
#' A function that calculates a mortgage payment on a 30 or 15 year mortgage with a supplied principal and interest rate.
#'
#' @param years Not sure yet how I wanna do this
#' @param principal A double positive real number that represents the principal of the loan
#' @param interestRate A double positive real number that represents the annual interest rate of the loan (as a percent. e.g. 3.8% = 3.8)
#'
#' @return The per-month payment for the specified mortgage
#'
mortgage_payment_calculator = function(years = 30,
                                       principal,
                                       interestRate) {
  numPayments = years * 12
  monthlyInterestRate = interestRate / 100 / 12
  monthlyPayment = principal * monthlyInterestRate * (1 + monthlyInterestRate) ^
    numPayments / ((1 + monthlyInterestRate) ^ numPayments - 1)

  return(monthlyPayment)
}

#'
#' percentage_of_income
#'
#' A function that calculates the percentage of gross income for a given monthly payment
#'
#' @param income A double that represents the gross yearly income of an individual
#' @param payment A double that represents the value of a particular payment
#'
#' @return The percentage of income for that payment
#'
percentage_of_income = function(income,
                                payment) {
  grossMonthlyIncome = income/12
  percentageOfIncome = payment/grossMonthlyIncome
  return(paste0(round(percentageOfIncome * 100, 2), "%"))
}

#'
#' suggested_maximum_housing_cost
#'
#' A function that provides the 30% suggested maximum cost of housing (including utilities, property tax, and maintenance)
#'
#' @param income A double that represents the gross yearly income of an individual
#'
#' @return 30% of the gross monthly income for the given value
#'
suggested_maximum_housing_cost = function(income) {
  return(.3*income/12)
}

#'
#' max_mortgage_calculator
#'
#' A function that calculates the mortgage payment on a 30 or 15 year mortgage with the maximum suggested payment and interest rate.
#'
#' @param years Not sure yet how I wanna do this
#' @param principal A double positive real number that represents the principal of the loan
#' @param interestRate A double positive real number that represents the annual interest rate of the loan (as a percent. e.g. 3.8% = 3.8)
#'
#' @return The per-month payment for the specified mortgage
#'
max_mortgage_calculator = function(years = 30,
                                   payment,
                                   interestRate) {
  numPayments = years * 12
  monthlyInterestRate = interestRate / 100 / 12
  principal = payment*(1-(1+monthlyInterestRate)^(-numPayments))/monthlyInterestRate
  return(round(principal, -3))
}




ui <- f7Page(
  title = NULL,
  darkmode = FALSE,
  init = f7Init(skin = "auto",
                theme = "dark"),

  f7SingleLayout(
    navbar = f7Navbar(
      title = "Mortgage App",
      hairline = FALSE,
      shadow = TRUE
    ),
    toolbar = f7Toolbar(
      position = "bottom",
      f7Link(
        label = "Zillow",
        src = "https://www.zillow.com",
        external = TRUE
      )
    ),

    #Allegedly the main content
    f7Card(
      img = "http://clipartsign.com/upload/2015/12/02/house-clipart-free-stock-photo-public-domain-pictures.jpg",
      tableOutput(outputId =  "MortgageSummary"),

      f7Text(
        inputId = "Income",
        label = "Income:",
        value = "50000"
      ),
      f7Text(
        inputId = "Mortgage",
        label = "Mortgage",
        value = "160000"
      ),
      f7Text(
        inputId = "InterestRate",
        label = "Interest Rate:",
        value = "3.88"
      ),
      f7Select(
        inputId = "Years",
        label = "Years",
        choices = c(30, 15)
      ),

      title = "Mortgage Summary",
    )
  )
)

server <- function(input, output) {
  # Mortgage calculator ----

  MaxPayment = reactive({
    logging::logdebug("Computing maximum suggested payment")
    maxPayment = suggested_maximum_housing_cost(income = as.numeric(input$Income))
    return(maxPayment)
  })

  MaxMortgage = reactive({
    logging::logdebug("Computing maximum suggested mortgage")

  })

  mortgageSummaryObject = reactive({
    mortgageSummary = list()
    logging::logdebug("Computing mortgage payment")
    mortgageSummary$maxPayment = MaxPayment()
    mortgageSummary$maxMortgage = max_mortgage_calculator(
      years = as.numeric(input$Years),
      payment = mortgageSummary$maxPayment,
      interestRate = as.numeric(input$InterestRate)
    )
    mortgageSummary$payment = mortgage_payment_calculator(
      years = as.numeric(input$Years),
      principal = as.numeric(input$Mortgage),
      interestRate = as.numeric(input$InterestRate)
    )
    mortgageSummary$percentOfIncome = percentage_of_income(income = as.numeric(input$Income),
                                                           payment = mortgageSummary$payment)

    logging::logdebug("Computing difference in payment and maximum suggested payment")
    mortgageSummary$paymentDiff = mortgageSummary$maxPayment - mortgageSummary$payment

    mortgageSummary = as.data.frame(mortgageSummary)
    mortgageSummary = t(mortgageSummary)
    return(mortgageSummary)
  })

  #TODO: Sign-in to zillow with a zwsid? or maybe look up zwsid
  #TODO: construct link to zillow based on query to zillow api
  # https://www.zillow.com/howto/api/GetSearchResults.htm

  # zillow = httr::GET(url = paste0("http://www.zillow.com/webservice/GetSearchResults.htm?zws-id=",
  #                                 input$ZillowID,
  #                                 "&address=2114+Bigelow+Ave&citystatezip=Seattle%2C+WA"),
  #                    httr::verbose())
  # output$url = paste0()

  output$MortgageSummary <- renderTable({
    mortgageSummaryObject()
  },
  rownames = TRUE,
  colnames = FALSE)

}

shinyApp(ui = ui, server = server)
