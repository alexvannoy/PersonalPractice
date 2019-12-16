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
