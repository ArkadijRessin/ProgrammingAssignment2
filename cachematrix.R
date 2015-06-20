## These functions cache&compute an invers matrix
##
## This funchtion creates a special matrix object which cache an invers matrix
## This functions returns of a list of the following functions:
##              -  set the matrix
##              -  get the matrix
##              -  set the inverse
##              -  get the inverse


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        
## The sing "<<" ist to assign value different from current environment         

        x <<- y
        m <<- NULL
    }

    get <- function() x
    setinvers <- function(invers) m <<- invers
    getinvers <- function() m

## this is a list functions to return:

    list(set = set, get = get, setinvers = setinvers, getinvers = getinvers)
}


## This funchtion compute an invers matrix 
## wich was returned from function makeCacheMatrix 

cacheSolve <- function(x, ...) {

## This function returns a matrix that is the inverse of 'x' 
    
        m <- x$getinvers()
        
## The functions returns a inverse matrix if it was just calculated

        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
## The inverse calculated here if it does not exist  
        
        data <- x$get()
        m <- solve(data, ...)

## ... and sets the value to inverse in the cache 

        x$setinvers(m)
        
## return a inverse matrix
        
        m
    }    
