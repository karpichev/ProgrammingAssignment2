## A set of functions that can store inverse of a matrix cached so it does
## not have to be recalculated every tiem

## Init function that creates an object-like list with data and access methods

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL #Inverse matrix initialization
    set <- function(y){ #store original matrix
        x <<- y
        inv <<- NULL
    }
    get <- function() x #return original matrix
    setinv <- function(inv3) inv <<- inv3 #store(cache) inverse matrix
    getinv <- function() inv #return cached inverse matrix
    list( set = set, get= get, setinv = setinv, getinv = getinv)
}


## Sovle function that that either calculates an inverse for a new matrix
## or return a cached value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv2 <- x$getinv() #get chached inversed matrix if available
     if (!is.null(inv2)){
         message("getting cached data")
         return(inv2) #return cahced inverse matrix
     }
     data <- x$get() #no cached inverse matrix exists
     inv2 <- solve(data, ...) #calculate inverse for the first time
     x$setinv(inv2) #cache it
     inv2 #return inverse
}
