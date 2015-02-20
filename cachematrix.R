## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This fuction takes matrix as an input and create four member functions
##   get <- returns the input matrix
##   set <- saves given matrix into cache
##   getInverse <- gets the cached inverse of matrix and 
##   setInverse <- set the inverse of a matrix to cache

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y){
          x <<- y
          m <<- NULL
     }
     ## get the matrix
     get <- function() x
     ## cache the given matrix, here inverse
     setInverse <- function (solve) m <<- solve
     ## return cached matrix, here inverse of the given matrix
     getInverse <- function () m
     list (set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Write a short comment describing this function
## This function takes a matrix and check if the inverse is already calculated/cached
## if so, simply display what's in cache or else calculate, display inverse and cache the result
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
     ## see if the result is in cache
     m <- x$getInverse()
     ## if so display and return
     if(!is.null(m)){
          message("getting cached data")
          return(m)
     }
     ## else calculate, cache, and diplay result
     data <- x$get()
     m <- solve(data, ...)
     x$setInverse(m)
     m
}
