
# Matrix inversion is usually a costly computation and there may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly. The following code
# caches the inverse of a matrix.

## 'makeCacheMatrix' function creates a special "matrix" object that can cache the inverse of the matrix
## that is passed as an argument.
## The function returns a list of functions:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix


    makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
            x <<- y
            i <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
    }


## `cacheSolve` function calculates the inverse matrix of the special "matrix" created with the above function.
##  It first checks to see if the inverse matrix has already been calculated. 
##  In such a case,it returns the  value from the cache and skips the computation. 
##  Otherwise, it calculates the inverse matrix  and stores it via the 'setinv' function.
##
##  It's assumed that the matix is invertible.

    cacheSolve <- function(x, ...) {
        # retrieves the cache
        i <- x$getinv()
        
        # if not NULL, returns the value in the cache
        if(!is.null(i)) {
            message("getting cached data")
            return(i)
        }
        
        #otherwise, the invese matrix is calculated
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
    }
