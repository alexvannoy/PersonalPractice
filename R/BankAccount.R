##This is hella cool to do in R.

BankAccount <- R6::R6Class(
  "BankAccount",
  public = list(
    balance = 0,
    initialize = function(balance = 0) {
      stopifnot(is.numeric(balance), length(balance) == 1)
      self$balance = balance
    },
    print = function(...) {
      cat("Bank Account Information: \n")
      cat("Balance: ", self$balance)
    },
    deposit = function(x) {
      stopifnot(is.numeric(x), length(x) == 1)
      self$balance = self$balance + x
      invisible(self)
    },
    withdrawal = function(x) {
      stopifnot(is.numeric(x), length(x) == 1)
      self$balance = self$balance - x
      invisible(self)
    }
  )
)

BankAccountNoOverdraft = R6::R6Class(
  "BankAccountNoOverdraft",
  inherit = BankAccount,
  public = list(
    withdrawal = function(x) {
      stopifnot(is.numeric(x), length(x) == 1, self$balance - x >= 0)
      super$withdrawal(x = x)
    }
  ))

BankAccountOverdraftFee = R6::R6Class(
  "BankAccountNoOverdraft",
  inherit = BankAccount,
  public = list(
    fee = 0,
    initialize = function(fee = 5){
      stopifnot(is.numeric(fee), length(fee) ==1)
      self$fee = fee
      super$initialize()
    },
    withdrawal = function(x) {
      if(self$balance - x >= 0){
        super$withdrawal(x = x)
      } else {
        super$withdrawal(x = (x + self$fee))
      }
    }
  ))

