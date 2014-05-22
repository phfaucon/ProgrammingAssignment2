## We are trying to eliminate repetitive matrix inversions.
## For this, instead of using directly matrices, we are using 
## an object-like list, that contains among other things the
## results of the solve calculation if it has already been done

## This function create a list corresponding to the matrix.
## at this point, we are not calling solve, so the cache will be empty

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function verifies if the matrix has already been solved
## if it has, it returns the cache solution, else, it calls solve

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached matrix")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}

## results
# > mdat <- matrix(c(1,2,3, 11,12,13,21,22,25), nrow = 3, ncol = 3, byrow = TRUE,
#                  dimnames = list(c("row1", "row2","row3"),
#                  c("C.1", "C.2", "C.3")))
# > x <- makeCacheMatrix(mdat)        ## create the list
# > cacheSolve(x)                     ## the first time, there is no inverse cached
# row1 row2 row3
# C.1 -0.7 -0.8  0.5
# C.2  0.1  1.9 -1.0
# C.3  0.5 -1.0  0.5
# > cacheSolve(x)
# getting cached matrix               ## the second time, get the cached version
# row1 row2 row3
# C.1 -0.7 -0.8  0.5
# C.2  0.1  1.9 -1.0
# C.3  0.5 -1.0  0.5
# > 
