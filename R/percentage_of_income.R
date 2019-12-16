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
