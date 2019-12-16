#'
#' eigen_function
#'
#' A function that computes the eigenvectors of a randomly generated matrix.
#'
#' @param seed An integer specifying the seed to be used in generating the random entries of the matrix
#' @param n The size of the n x n matrix to create
#'
#' @return The eigen() object for an n x n matrix randomly generated with the given seed
#'
#' @examples
#' eigen_function(1, 5)
#' eigen_function(17L, 17L)
eigen_function = function(seed = 1,
                          n = 5) {
  #Artificial computation cost as this was MUCH faster than I thought it would be
  #Sys.sleep(5)

  if (length(seed) > 1) {
    warningMessage = paste0("More than 1 seed was supplied. Defaulting to only the first value supplied, ", seed[1])
    logging::logwarn(warningMessage)
    warning(warningMessage)
    seed = seed[1]
  }
  if (!(typeof(seed) %in% c("double", "integer"))) {
    seed = tryCatch(
      as.integer(seed),
      warning = function(warningMessage) {
        warningMessage = "Seed could not be successfully typed as an integer. Using default of 1 instead"
        logging::logwarn(warningMessage)
        warning(warningMessage)
        return(1L)
      },
      error = function(errorMessage) {
        warningMessage = "Seed could not be successfully typed as an integer. Using default of 1 instead"
        logging::logwarn(warningMessage)
        warning(warningMessage)
        return(1L)
      }
    )
  }

  if (length(n) > 1) {
    warningMessage = paste0("More than 1 seed was supplied. Defaulting to only the first value supplied, ", n[1])
    logging::logwarn(warningMessage)
    warning(warningMessage)
    n = n[1]
  }
  if (!(typeof(n) %in% c("double", "integer"))) {
    n = tryCatch(
      as.integer(n),
      warning = function(warningMessage) {
        warningMessage = "N could not be successfully typed as an integer. Using default of 5 instead."
        logging::logwarn(warningMessage)
        warning(warningMessage)
        return(5L)
      },
      error = function(errorMessage) {
        warningMessage = "N could not be successfully typed as an integer. Using default of 5 instead"
        logging::logwarn(warningMessage)
        warning(warningMessage)
        return(5L)
      }
    )
  }
  if (n <= 1) {
    warningMessage = paste0(n,
                            "is an invalid input for forming a suitable n x n matrix. Using default of 5 instead")
    logging::logwarn(warningMessage)
    warning(warningMessage)
    n = 5L
  }

  set.seed(seed)
  randomMatrix <- matrix(data = abs(rnorm(n ^ 2)), ncol = n)
  eigenValues <- tryCatch(
    eigen(randomMatrix),

    #This *should* be dead code
    error = function(err) {
      logging::logerror("fatal error")
      stop("fatal error")
    }
  )

  return(eigenValues)
}
