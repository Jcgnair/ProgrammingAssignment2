## makeCacheMatrix function creates a special "matrix" object that can cache its inverse
## calculates the inverse of the matrix.
## If the matrix inverse has already been calculated, it will instead 
## find it in the cache and return it, and not calculate it again. 


makeCacheMatrix <- function(x = matrix()) {
        inverse_x <- NULL
        set function(y) {
                x<<- y
                inverse_x <<- NULL 
        }
        get <- function () x
        setinverse <- function (inverse) inverse_x <<- inverse
        getinverse <- function () inverse_x
        list (set = set,
              get = get,
              setinverse = setinverse,
              getinverse = getinverse) 

}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix function. 
## If the inverse has already been calculated (and the matrix has not changed) 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse_x <- x$getinverse()
        if (!is.null(inverse_x)) {        
                message("Getting cached inverse matrix")        
                return(inverse_x)
        } 
        else {        
                inverse_x <- solve(x$get())        
                x$setinverse(inverse_x)        
                return(inverse_x)
        }
}