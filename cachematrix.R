#Similar to the makeVector function, here we "add" functions to our special 
#matrix.  We will later call on these functions so that we don't have to
#repeat calculations.

#As in the example we define the get and set functions, and assign the solve 
#function.



 makeCacheMatrix <- function(x = numeric()) {
     m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
     }
     get <- function() x
     setsolve<-function(solve) m <<- solve
     getsolve<- function() m
     list(set = set, get = get,
          setsolve=setsolve,
          getsolve=getsolve)
 }
 
#The cacheSolve allows us to either perform the calculation or recall it from 
#memory in case it has already been calculated.

 cacheSolve <- function(x,...) {
     m <- x$getsolve()
     if(!is.null(m)) {
         message("getting cached data")
         return(m)
     }
     data <- x$get()
     m <- solve(data,...)
     x$setsolve(m)
     m
 }