## Programming Assignment 2
## by Eduardo Magalhaes Barbosa

## This script creates two functions as below:
## 1) makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.



## Function makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {     # input x will be a matrix
    
    im <- NULL    #  im will be our 'Inverse Matrix' and it's reset to NULL every 
                  #  time makeCacheMatrix is called
    
    ## The next functions are meant to be object 'methods'. They are: get, setsolve and getsolve
    
    get <- function() { x }       # this function returns the value of the original matrix
        
    setsolve <- function(solve)   # this is called by cachesolve() during the first cachesolve()
    { im <<- solve }              # access and it will store the value using superassignment
    
    getsolve <- function() { im } # this will return the cached value to cachesolve() on
                                  #  subsequent accesses
    
    #  This list is returned with the newly created object. It lists all the functions ("methods") that are part of      
    #   the object.  If a function is not on the list then it cannot
    #   be accessed externally.
    
    list(get = get, setsolve = setsolve, getsolve = getsolve)  
        
}

##  Function cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {  # the input is an object created by makeCacheMatrix
                                  ## Return a matrix that is the inverse of 'x'

    im <- x$getsolve()            # accesses the object 'x' and gets the value of the inverse matrix
    
    if(!is.null(im)) {            # if inverse was already cached (not NULL) ...
      
      message("getting cached data")  # ... send this message to the console
      return(im)                      # ... and end the function by returning the inverse matrix
            
      }
    
    data <- x$get()        # only if x$getsolve() returned NULL *** No inverse yet
    im <- solve(data, ...) # if im was NULL then we have to solve matrix inverse
    x$setsolve(im)         # store the calculated inverse matrix value in x (see setsolve() in makeCacheMatrix)
    im                     # return the inverse matrix where this function was called
  
}
