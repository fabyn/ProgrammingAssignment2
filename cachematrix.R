## Caching the Inverse of a Matrix
## Functions that creates a matrix, checks if there is a inverse in cache and returns it
## or, if there isn't, computes it inverse

makeCacheMatrix <- function(x = matrix()) {
        ## The function creates a matrix 
        ## set the matrix
        ## get the matrix
        ## set the inversed matrix
        ## get the inversed matrix
        inv = NULL
        set = function(y) {
                x <<- y
                inv <<- NULL
        }
        get = function() x
        setInv = function(inverse) inv <<- inverse
        getInv = function() inv
        list(set = set, get = get, 
             setInv = setInv, 
             getInv = getInv)        
}


cacheSolve <- function(x, ...) {
        ## Computes the inverse of the matrix, 
        ## if it has been calculated before, it retrieves
        ## the inverse matrix from the cache.
        ## Return a matrix that is the inverse of 'x'
        inv = x$getInv()
        
        #check if the inverse has been calculated before
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        #If there isn't any data in cache, it will compute the inverse matrix
        mat.data <- x$get()
        inv = solve(mat.data, ...)
        
        x$setInv(inv)
        return(inv)
}
