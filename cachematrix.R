## This program is a pair of functions, (i) makeCacheMatrix and 
## (ii) cacheSolve, that will cache the inverse of a non singular matrix.  
## - Kyle Billings


# This function creates a matrix object that can cache it's inverse,
#  - it is really just a list of 4 functions

makeCacheMatrix <- function(x=matrix()){ # defines function with input x
        D <- NULL # Sets D=Null for some future value
        set <- function(y) { # defines a function set
                x <<- y #set x to y
                D <<- NULL #reset D to null
        }
        get <- function() x # returns x
        setinv <- function(solve) D <<- solve # sets D to inv of inputed matrix
        getinv <- function() D # returns the D
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv) # set value of matrix, get value of matrix, set 
        # inv of matrix, get value of inv of matrix
}


# This function computes the inverse of the matrix returned by 
# make cache inverse. 
# Note that if the inverse has already been calculated, then cacheSolve
# simply retrieves the inverse from the cache.

cacheSolve <- function(x=matrix(), ...) { #defines function with input matrix x
        D <- x$getinv() # retrieve inv from above
        if(!is.null(D)) { # retrive inv from cache
                message("getting cached data") # displayed message
                return(D) # outputs D to the console
        }
        matrix <- x$get() #set matrix to x from above
        D <- solve(matrix, ...) #invert matrix
        x$setinv(D) # set x to above
        D # display D to console
}




