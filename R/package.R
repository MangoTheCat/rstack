
#' A stack data type, implemented as an R6 class
#'
#' Methods: \itemize{
#' \item \code{push(elem)} Puts an R object on the top of the stack.
#'   This operation might trigger the allocation of memory, if the
#'   currently allocated memory cannot hold the new element. Still, the
#'   amortized time complexity of the stack is constant.
#'   The stack may contain arbitrary R objects.
#' \item \code{pop()} Removes and returns the top element of the stack.
#'   It throws an error if the stack is empty.
#' \item \code{peek()} Returns the top element of the stack. (Without
#'   removing it.) It throws an error if the stack is empty.
#' \item \code{size()} Returns the number of elements in the stack.
#' \item \code{is_empty()} Returns wheather the stack is empty.
#' }
#'
#' @export
#' @importFrom R6 R6Class
#' @examples
#' S <- stack$new()
#' S$push(1L)
#' S$peek()
#' S$pop()
#' S$size()
#'
#' S$push(NULL)
#' S$push(iris)
#' colnames(S$peek())

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
      self
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
