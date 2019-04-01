
## Creates a special matrix object that can cache its inverse

makeCacheMatrix <- function( m = matrix() ) {
        
    i <- NULL
        
    set <- function( matrix ) {
            
            m <<- matrix
            
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        i
    }

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getInverse()

    ## Return the inverse if its already set
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    data <- x$get()
    m <- solve(data) %*% data
    x$setInverse(m)
    m
}
