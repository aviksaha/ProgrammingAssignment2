## 
## The makeCacheMatrix function takes a matrix object as formal argument 
## and returns a list of four functions set(), get(), setinverse() 
## and getinverse().
##
## The cacheSolve function takes the list objet returned by 
## makeCacheMatrix and gets the matrix inverse if available in the
## cache. If not, it calculates the matrix inverse using R "solve" 
## function and stores it in the cache for subsequent retrival. The
## cacheSolve function returns the inverse matrix - either calculated 
## or from the cache.
##

## makeCacheMatrix function creates a special "matrix" object that 
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m.inv <- NULL
  list (
      set = function (y) { ## first element of list : set
        x <<- y
        m.inv <<- NULL
      },
      get = function () { ## second element of list : get
        x
      },
      setinverse = function(minv) { ## third element of list : setinverse
        m.inv <<- minv
      },
      getinverse = function () { ## fourth element of list : getinverse
        m.inv
      }
      )

}


## cacheSolve function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cacheSolve  retrieves the inverse 
## from the cache.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'
  m.inv <- x$getinverse()
  if (!is.null(m.inv)) {  ## if inverse is available in cache, return it
    message ("getting cached data")
    return (m.inv)
  }
  m <- x$get()            ## if inverse is not available in cache, get the matrix,
  minv <- solve (m, ...)  ## calculate the inverse and 
  x$setinverse(minv)      ## store the inverse in the cache.
  minv                    ## return the matrix inverse
}
