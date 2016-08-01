
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

stack <- function(n = 100L) {
  data <- list(v = vector(n, mode = "list"), ptr = 0L)
  list(
    pop  = function() {
      res <- stack_pop(data)
      data <<- res$data
      res$res
    },
    push = function(elem) {
      res <- stack_push(data, elem)
      data <<- res$data
      res$res
    },
    peek = function() {
      res <- stack_peek(data)
      data <<- res$data
      res$res
    },
    size = function() {
      res <- stack_size(data)
      data <<- res$data
      res$res
    },
    is_empty = function() {
      res <- stack_is_empty(data)
      data <<- res$data
      res$res
    }
  )
}

stack_pop <- function(data) {
  if (data$ptr == 0) stop("Nothing to pop from empty stack")
  res <- data$v[[data$ptr]]
  data$v[data$ptr] <- list(NULL)
  data$ptr <- data$ptr - 1L
  list(data = data, res = res)
}

stack_push <- function(data, elem) {
  ## Allocate more storage if needed
  if (data$ptr == length(data$v)) {
    data$v <- append(data$v, vector(length(data$v), mode = "list"))
  }
  data$ptr <- data$ptr + 1L
  data$v[data$ptr] <- list(elem)
  list(data = data, res = data$v[[data$ptr]])
}

stack_peek <- function(data) {
  if (data$ptr == 0) stop("Nothing to peek at empty stack")
  list(data = data, res = data$v[[data$ptr]])
}

stack_size <- function(data) {
  list(data = data, res = data$ptr)
}

stack_is_empty <- function(data) {
  list(data = data, res = data$ptr == 0)
}
