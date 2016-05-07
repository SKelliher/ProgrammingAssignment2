## given matrix, e.g. x <- matrix(c(1,2,3,0,1,4,5,6,0),3,3,byrow=TRUE)
## cacheSolve(makeCacheMatrix(x)) returns the inverse of x
## CacheSolve requires matrix parameter that is invertible

## returns a list of functions for call by cacheSolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## returns cached value of matrix inverse of makeCacheMatrix(x) else calculates inverse using solve()

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
