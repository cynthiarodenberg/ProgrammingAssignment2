## Overview of this program is provided here.  Details regarding the function of individual lines of code are 
##  provided below.  Thanks goes to Leonard Greski for his article "Demystifying makeVector()" in Week 3 Forum.
## 
##  The 2 functions here are used to enable one to cache an object (in this case the inverse of a square 
##  invertible matrix) that may be computationally intensive to compute so that, once computed, any further needs  
##  for the value can be retrieved rather than computed again.


# makeCacheMatrix will create a special matrix that stores a numeric matrix and cache's its inverse.
# 4 functions are built (set(), get(), setinverse(), getinverse()) and  returned within a list to the 
# parent environement.  

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # the above initializes x to an empty matrix and m to NULL
    
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # the set() function assigns input argument to the x object and Null to the m object in the parent env. If 
    # x is reset then the value of m cached is cleared.
    
    get <- function() x
    # the get() function gets the value of x from the parent environment of makeCacheMatrix.
    
    setinverse <- function(inversematrix) m <<- inversematrix
    # m is assigned the value of the inverse in the parent environment so that it is available after 
    # setinverse() completes. 
    
    getinverse <- function() m
    # retrieves value for m
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
    # creates a list of the 4 functions, each of which is named. 
}

# The following function calculates or retrieves the inverse from the special "matrix" created above. 
# Tt first checks to see if the inverse has already been calculated. If so, it retrieves it from the 
# cache and skips the computation. Otherwise, it calculates the inverse of the matrix and sets the value 
# of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
            message("getting cached inverse")
            return(m)
        }
        # If the inverse exists then m is not null and the message above is provided and the value for 
        # m is retrieved.
        
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        # Otherwise, the matrix is retrieved from the input object, the inverse is computed, and the 
        # value is set.
}

# Trying it out:

aMatrix <-matrix(1:4, ncol = 2,nrow = 2)
myMatrix<-makeCacheMatrix(aMatrix)
cacheSolve(myMatrix) # returns the inverse of aMatrix
myMatrix$get() #returns the original aMatrix
myMatrix$getinverse() #returns the inverse of aMatrix

myMatrix$set(matrix(5:8,ncol=2,nrow = 2)) # changes the input matrix
myMatrix$get() # returns the new input matrix
myMatrix$getinverse() # returns NULL because the input matrix is new
cacheSolve(myMatrix) # returns the inverse of the new input matrix
myMatrix$getinverse() #returns the inverse of the new input matrix


