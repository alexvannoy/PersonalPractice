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

