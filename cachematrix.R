# ASSIGNMENT 2 - LEXICAL SCOPING ####

# makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      im <- NULL # inverse matrix is a null vector
      set <- function(y) {
            x <<- y
            im <<- NULL
      }
      get <- function() x
      setim <- function(inverse) im <<- inverse
      getim <- function() im
      list(set = set, get = get,
           setim = setim,
           getim=getim)
}

# cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix
# if the inverse has already been calculated (and the matrix has not changes), 
# then cachSolve will retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
      # return a matrix that is the inverse of x
      im <-x$getim()
      if(!is.null(im)) { # if im is not null (i.e. inverse of matrix has already been caches)
            message("getting cached inverse matrix")
            return(im) #returns cached inverted matrix
      }
      data <- x$get()
      im <- solve(data, ...)
      x$setim(im)
      im
}

