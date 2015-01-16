## cache matrix's reverse. we can get and set the matrix and its reserve


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setreverse <- function(reverse) m <<- reverse
        getreverse <- function() m
        list(set = set, get = get,
             setreverse = setreverse,
             getreverse = getreverse)
        
}


## returns cached reverse if there is, otherwise compute it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getreverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setreverse(m)
        m
}
