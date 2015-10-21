## Matrix inversion can be a time-consuming computation. 
## Caching the inverse of a matrix after computing once
## rather than computing it in each run can conserve 
## resources and enhance efficiency. These 2 function
## portray solution to this problem.

## Function makeCacheMatrix creates a special 
## "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      # declaring matrix object
      invM <- NULL
      
      # set:   set the value of the matrix      
      set <- function(y) {
        x <<- y
        invM <<- NULL
      }
      
      # get:   get the value of the matrix
      get <- function() x
      
      # setmatrix: set the value of inverse matrix
      # getmatrix: get the value of inverse matrix
      setmatrix <- function(solve) invM <<- solve
      getmatrix <- function() invM
      # makeCacheMatrix returns a list of function arguments
      list(set = set, get = get,
           setmatrix = setmatrix,
           getmatrix = getmatrix)
}


## Function cacheSolve computes the inverse of the  
## special "matrix" returned by makeCacheMatrix above
## The function assumes that the matrix is invertible
## First, it checks if inverse has already been computed.
## If so, it retrieves the value from the cached data.

cacheSolve <- function(x, ...) {
      # Retrieves a matrix that is the inverse of 'x'
      invM <- x$getmatrix()
      
      #Checks if inverse matrix exists already
      # If so, inverse matrix is returned
      if(!is.null(invM)) {
        message("getting cached data")
        return(invM)
      }
      
      #Else it creates the matrix and its inverse
      data <- x$get()
      invM <- solve(data, ...)
      x$setmatrix(invM)
      invM
}


## Test run:
## > xy <- rbind(c(2, 7, -4, 0), c(3, 12, 16, -1), 
##              c(5, 10, 0, 6), c(-2, 1, 13, 4))
## > m <- makeCacheMatrix(xy)
## > m$get()
##      [,1] [,2] [,3] [,4]
## [1,]    2    7   -4    0
## [2,]    3   12   16   -1
## [3,]    5   10    0    6
## [4,]   -2    1   13    4

## No cache in the first run
##
## > cacheSolve(m)
##              [,1]         [,2]         [,3]          [,4]
## [1,] -0.436906377  0.111261872  0.199457259 -0.2713704206
## [2,]  0.220081411 -0.007598372 -0.052645862  0.0770691995
## [3,] -0.083310719  0.042333786  0.007598372 -0.0008141113
## [4,] -0.002713704 -0.080054274  0.088195387  0.0976933514

## The second run retrieves the previous 
## inverse matrix from the cahed data
##
## > cacheSolve(m)
## getting cached data.
##              [,1]         [,2]         [,3]          [,4]
## [1,] -0.436906377  0.111261872  0.199457259 -0.2713704206
## [2,]  0.220081411 -0.007598372 -0.052645862  0.0770691995
## [3,] -0.083310719  0.042333786  0.007598372 -0.0008141113
## [4,] -0.002713704 -0.080054274  0.088195387  0.0976933514
## > 
