makeCacheMatrix <- function(x = matrix()) {
        m <- null
        set <- function (y){
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setinverematrix <- function (solve) m <<-solve 
        getinversematrix <- function () m
        list (set = set, get = get, 
              setinverematrix = setinverematrix,
              getinversematrix = getinversematrix)
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinversematrix()
        if (!is.null (m)){
                message ("getting cached inverse matrix")
                return (m
                )        }
        data <- x$get()
        m<-solve(data, ...)
        x$setinverematrix(m)
        
}