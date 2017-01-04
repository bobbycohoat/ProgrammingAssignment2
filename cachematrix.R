## Function that creates a matrix object that cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y = matrix()) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## function uses makeCacheMatrix to check if a matrix is inverted. If not then it inverts the mattrix

cacheSolve <- function(x, ...) {
        m <-x$getinv()
        if(!is.null(m)) {
                message("Inverting")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        print(m)
        ## Return a matrix that is the inverse of 'x'
}
