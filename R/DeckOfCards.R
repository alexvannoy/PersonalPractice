library(R6)
DeckOfCards = R6::R6Class(
  "DeckOfCards",
  public = list(
    left_in_deck = c(),
    drawn_so_far = c(),
    initialize = function() {
      self$left_in_deck = private$cards
    },
    draw = function(n = 1) {
      newCards = try(sample(self$left_in_deck, n), silent = TRUE)
      if(any(grepl("Error", newCards, ignore.case = TRUE))) {
        cat("You've selected all the cards. You must reshuffle to draw more...")
      } else {
        self$left_in_deck = setdiff(self$left_in_deck, newCards)
        self$drawn_so_far = c(self$drawn_so_far, newCards)
        cat("Drawn so far: ", self$drawn_so_far, "\n ")
        cat("Left in deck: ", self$left_in_deck, "\n ")
        invisible(self)
      }
    },
    reshuffle = function() {
      self$left_in_deck = private$cards
      self$drawn_so_far = c()
      invisible(self)
    },
    print = function() {
      cat("Deck of Cards: \n")
      cat("use $draw(n) to draw 'n' cards \n")
      cat("use $reshuffle to reshuffle all cards back into the deck \n")
      cat("Drawn so far: ", self$drawn_so_far, "\n ")
      cat("Left in deck: ", self$left_in_deck, "\n ")
    }
  ),
  private = list(
    #suit = c("♠", "♥", "♦", "♣") #This works manually but not automatically
    suit = c("S", "H", "D", "C"),
    value = c("A", 2:10, "J", "Q", "K"),
    cards = paste0(rep(c("A", 2:10, "J", "Q", "K"), 4), c("S", "H", "D", "C"))
  )
)
