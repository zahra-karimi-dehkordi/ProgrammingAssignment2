## This function makes a matrix with cache. Note that this function does not have reverse capability just cache it
## input:  a raw matrix
## output: the cache matrix: a new type with set, get, setreverse and getreverse services
##         get: returns raw matrix
##         set: assigns a new raw matrix
##         getreverse: if the reverse exists in the cache, returns it otherwise returns null
##         setreverse: assigns the reverse of matrix


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


## This function returns the reverse of cachematrix 
## input: cachematrix
## output: a reversed raw matrix


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
