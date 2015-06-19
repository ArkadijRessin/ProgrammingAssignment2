## These functions cache&compute an invers matrix
## 

## This funchtion creates a special matrix object which cache an invers matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinvers <- function(invers) m <<- invers
    getinvers <- function() m
    list(set = set, get = get, setmean = setinvers, getmean = getinvers)
}


## This funchtion compute an invers matrix 
## wich was returned from function makeCacheMatrix 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
        m <- x$getinvers()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinvers(m)
        m
    }    
    
}
