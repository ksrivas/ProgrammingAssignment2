#Assignment 2: Caching data


#Two functions makeCacheMatrix and cacheSolve are implemented.
# makeCacheMatrix creates a matrix
#This is the desired function makeCachematrix that creates a special
#matrix which has information about whether its inverse has been cached through
#"m".

#The function returns a matrix "x" of functions
#which can be accessed by the function cacheSolve
# on the matrix returned by the makeCacheMatrix
# to know if the inverse needs to be recomputed
#or whether it could be taken from the cache.

makeCacheMatrix <- function(x = matrix()) {
		 m<-NULL
  		 set<-function(y){
    		 x<<-y
    		 m<<-NULL
  	}		 
    		 get<-function() x
  		 setinv<-function(solve) m<<-solve
  		 getinv <- function() m
  		 list(set = set, get = get,
       		 setinv = setinv,
       		 getinv = getinv)

}


## Write a short comment describing this function

#The cacheSolve function tests if the
# matrix x has already been inverted. If not
#then it inverts it otherwise it returns
# the inverse of x from cache.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("Info: getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}


