##set and get values of the matrix and inverse in the higher environment 

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<-NULL
      }
      get <- function() x
      set_inverse <- function (inverse) i <<- inverse
      get_inverse <- function() i
      list(set = set,
           get = get,
           set_inverse = set_inverse,
           get_inverse = get_inverse)
}


## checks to see if the inverse exists and retrieves it if it does

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
i <- x$get_inverse()
      if(!is.null(i)){
            return(i)
      }
      value <- x$get()
      i <- solve(value,...)
      x$set_inverse(i)
      i
}
