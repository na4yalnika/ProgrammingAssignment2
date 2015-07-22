## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        s <- NULL # restores to null the value of the inverse matrix
        set <- function(y) { # stores the matrix in the main function
                x <<- y
                set <<- NULL
        }
        get <- function() x # recieves the stored matrix
        setSolve <- function(slv) s <<- slv # stores the inverse matrix(actual result)
        getSolve <- function() s # recieves the inverse matris
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)
        # list ensures that function result contains all the fiedls
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        s <- x$getSolve() # recieves the inversed matrix from the previous function
        if(!is.null(s)) { # if it is not null, then it takes the precviously stored result
                message("getting cached data")
                return(s)
        }
        data <- x$get() #otherwise it callculates the inverse matrix
        s <- solve(data)
        x$setSolve(s) #stores result in cache
        s #prints the result
}
