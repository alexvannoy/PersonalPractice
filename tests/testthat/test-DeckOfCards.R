library(testthat)

context("Test Deck of Cards Class")

test_that(
  "Deck of Cards",
  {
    #This runs locally. I suspect it's the special characters that's breaking it.
    set.seed(1)
    newDeckOfCards = DeckOfCards$new()
    logging::logdebug(paste0(DeckOfCards$private_fields$suit))
    expect_equal(DeckOfCards$private_fields$suit, c("S", "H", "D", "C"))
    logging::logdebug(paste0(DeckOfCards$private_fields$value))
    expect_equal(DeckOfCards$private_fields$value, c("A", 2:10, "J", "Q", "K"))
    logging::logdebug(paste0(DeckOfCards$private_fields$cards))
    expect_equal(DeckOfCards$private_fields$cards, paste0(rep(c("A", 2:10, "J", "Q", "K"), 4), c("S", "H", "D", "C")))

    newDeckOfCards$draw(1)
    logging::logdebug(paste0(DeckOfCards$public_fields$drawn_so_far))
    expect_equal(newDeckOfCards$drawn_so_far, "4C")

    newDeckOfCards$draw(1)
    logging::logdebug(paste0(DeckOfCards$public_fields$drawn_so_far))
    expect_equal(newDeckOfCards$drawn_so_far, c("4C", "AC"))

    newDeckOfCards$draw(50)
    logging::logdebug(paste0(DeckOfCards$public_fields$drawn_so_far))
    expect_equal(setdiff(newDeckOfCards$drawn_so_far, DeckOfCards$private_fields$cards), character(0))

    newDeckOfCards$reshuffle()
    logging::logdebug(paste0(DeckOfCards$public_fields$drawn_so_far))
    expect_equal(newDeckOfCards$drawn_so_far, NULL)
  }
)
