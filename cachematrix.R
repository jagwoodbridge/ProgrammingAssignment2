## Creates a procedure for calculating the inverse of a matrix
## and storing it in memory to reference later instead of
## recalculating each time it's needed.

## makeCacheMatrix creates a special matrix from one that is input
## that allows you to set and retrieve the matrix (get) from a cache
## along with setting and getting the results of a calculation
## (in this case calculating the inverse of a matrix)

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## cacheSolve checks if the calculation has already been saved
## in the cache. It and stores it in the cache if not, or retreives
## it if already in the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check if calculation already in cache
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Calculate if NOT already in cache.
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
        

}