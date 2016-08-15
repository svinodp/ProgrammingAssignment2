## The function Caches the inverse of a matrix
## It stores a matrix and caches it's inverse

makeCacheMatrix <- function(x = matrix()) {
    #initialiase the inverse
    inv<- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL ## setting the value of inverse to NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    
    ##making the list consisting of set, get, setInverse & getInverse
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}




## This function computes the inverse of the matrix created by
##function makeCacheMatrix above. If the matrix has not changed and 
##inverse is already available in the cache, the function retrieves
##the inverse from the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    matrix <- x$get()
    inv <- solve(matrix, ...)##compute the input
    x$setInverse(inv)
    inv
}

