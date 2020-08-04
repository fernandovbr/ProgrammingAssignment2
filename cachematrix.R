

makeCacheMatrix <- function(x = matrix()) {#set the matrixes
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInv <- function(invert) m <<- invert
    getInv <- function() m
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}




cacheSolve <- function(x, ...) {#compute the solve of the matrix
    m <- x$getInv() 
    if(!is.null(m)) { #check if the matrix int the getINv() is not NULL
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setInv(m)#set the inverse matrix 
    m ## Return a matrix that is the inverse of 'x'
}
