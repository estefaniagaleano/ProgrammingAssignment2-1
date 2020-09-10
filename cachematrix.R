## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
            set <- function(y) {
              x <<- y
              mat_inv <<- NULL
            }
            get <- function() x
            set_inv <- function(inv) mat_inv <<- inv
            get_inv <- function() mat_inv
            list(set = set, 
                 get = get,
                 set_inv = set_inv,
                 get_inv = get_inv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          mat_inv <- x$get_inv()
          if (!is.null(mat_inv)) {
            message("getting cached data")
            return(mat_inv)
          }
          data <- x$get()
          mat_inv <- solve(data, ...)
          x$set_inv(mat_inv)
          mat_inv
}
