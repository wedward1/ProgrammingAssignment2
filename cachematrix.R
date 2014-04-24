## Function set() assigns y to x and null to m
## Function get() returns x
## Function setinv() assigns inverse matrix to m
## Function getinv() returns m
## This function creates a list of functions (set, get, etc.) for the input

makeCacheMatrix <- function(x = matrix()) {
    #Assign null to variable m
    m <- NULL
        set <- function(y) {
        x <<- y
        m <<- NULL
    }
        get <- function() x
    
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    #Command to create a list of the functions created
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}



## This function determines the inverse matrix m
## It does this by checking to see if m has already been calculated
## If m has been calculated then it returns m that was calculated
## If m has not already been calculated then it will find the inverse matrix m

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
