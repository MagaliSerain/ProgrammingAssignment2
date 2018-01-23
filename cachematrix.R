## The functions I wrote will first allow me to cache the inverse of my matrix,
## then pull the cache value if it exists, otherwise run the solve function to inverse the matrix and cache the result.

## This function will first set my inverse_matrix to NULL, then inverse and cache it.

makeCacheMatrix <- function(x = matrix()) {
        inverse_matrix <- NULL
      set <- function(y) {
            x <<- y
             inverse_matrix <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse_matrix <<- solve
        getinverse <- function() inverse_matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}



## This second function will check to see if the inverse already exists for my inverse_matrix.
## If it exist, it pulls that value. Else, it calculates the inverse of inverse-matrix and caches it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                inverse_matrix <- x$getinverse()
                if(!is.null(inverse_matrix)) {
                        message("getting cached inverse")
                        return(inverse_matrix)
                }
                matrix <- x$get()
                inverse_matrix <- solve (matrix, ...)
                x$setinverse(inverse_matrix)
                inverse_matrix
}
