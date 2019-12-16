library(testthat)

context("Test Bank Account Classes")

test_that(
  "Bank Account Works",
  {
    myBankAccount = BankAccount$new(balance = 1)
    expect_equal(myBankAccount$balance, 1)

    myBankAccount = BankAccount$new()
    myBankAccount$deposit(100)
    myBankAccount$withdrawal(50)
    myBankAccount$withdrawal(50)
    myBankAccount$withdrawal(0.01)
    expect_equal(myBankAccount$balance, -0.01)
  }
)

test_that(
  "No Overdraft Bank Account Works",
  {
    myBankAccount = BankAccountNoOverdraft$new()
    myBankAccount$deposit(100)
    myBankAccount$withdrawal(50)
    myBankAccount$withdrawal(50)
    expectedError = try(myBankAccount$withdrawal(0.01), silent = TRUE)
    expect_true(grepl("Error", expectedError, ignore.case = TRUE))
  }
)


test_that(
  "Overdraft Fee Bank Account Works",
  {
    myBankAccount = BankAccountOverdraftFee$new()
    myBankAccount$deposit(100)
    myBankAccount$withdrawal(50)
    myBankAccount$withdrawal(50)
    myBankAccount$withdrawal(0.01)
    expect_equal(myBankAccount$balance, -5.01)

    myBankAccount = BankAccountOverdraftFee$new(fee = 10)
    myBankAccount$withdrawal(0.01)
    expect_equal(myBankAccount$balance, -10.01)
  }
)


