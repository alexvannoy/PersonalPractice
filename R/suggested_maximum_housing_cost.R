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
