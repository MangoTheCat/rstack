
#' Stack Data Types as an 'R6' Class
#'
#' An extremely simple stack data type, implemented with 'R6' classes. The
#' size of the stack increases as needed, and the amortized time
#' complexity is O(1). The stack may contain arbitrary objects.
#'
#' @docType package
#' @name stack
NULL

#' @export
#' @importFrom R6 R6Class

stack <- R6Class(
  "stack",
  public = list(

    initialize = function(n = 100L) {
      private$data <- list(v = vector(n, mode = "list"), ptr = 0L)
      self
    },

    pop = function() {
      if (private$ptr == 0) stop("Nothing to pop from empty stack")
      res <- private$data[[private$ptr]]
      private$data[private$ptr] <- list(NULL)
      private$ptr <- private$ptr - 1L
      res
    },

    push = function(elem) {
      ## Allocate more storage if needed
      if (private$ptr == length(private$data)) {
        private$data <- append(
          private$data,
          vector(length(private$data), mode = "list")
        )
      }
      private$ptr <- private$ptr + 1L
      private$data[private$ptr] <- list(elem)
      list(data = data, res = private$data[[private$ptr]])
    },

    peek = function() {
      if (private$ptr == 0) stop("Nothing to peek at empty stack")
      private$data[[private$ptr]]
    },

    size = function() {
      private$ptr
    },

    is_empty = function() {
      private$ptr == 0L
    }
  ),

  private = list(
    data = NULL,
    ptr = 0L
  )
)
