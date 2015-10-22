## These functions will store a matrix into cache, create an inverse of that matrix and
## store that matrix as well.  It will also allow the user to get or set either matrix


## Function that creates a special matrix that caches the matrix and its inverse
## with the ability to set the matrix, get the matrix, 
## set the inverse of the matrix and get the inverse of the 
## matrix
makeCacheMatrix <- function(x = matrix()) {
        #initialize inv matrix to NULL
        inv <- NULL
        #create set function that caches the matrix and sets cache
        #of inverse matrix to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        #create get function that returns the matrix
        get <- function() x
        #create setInverse which sets the Inverse function into cache
        setInverse <- function(invFuct) inv <<- invFuct
        #create getInverse which pulls the current value of the inverse function
        getInverse <- function() inv
        #create list of functions
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function that takes a special matrix as an argument created by 
## makeCacheMatrix and then creates
## the inverse of the matrix.  If the inverse has already been
## calculated, it will returned the calculated cached inverse
cacheSolve <- function(x, ...) {
        #Pull inverse value of the passed special function
        inv <- x$getInverse()
        #determine if the inverse value.  If not NULL, return
        #the value, if NULL calculate the inverse
        if(!is.null(inv)){
                message("inverse pulled from cache")
                return(inv)
        }
        #retrieve matrix for inverse calculation
        data <- x$get()
        #calculate inverse function
        inv <- solve(data)
        #cache inverse function
        message("caching inverse function")
        x$setInverse(inv)
        #return inverse function
        inv
}