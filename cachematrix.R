# Creates a special matrix object that can cache its inverse
# Argument parameter: Matrix, Output: List
makeCacheMatrix <- function(x = matrix()){
    # Initialisig the inverse value
    m <- NULL
    
    # Method to set the matrix
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    
    # This is a method to get the matrix.
    get <- function() x
    
    # This is the method to set the inverse of the matrix
    setinverse <- function(inverse) m <<- inverse
    
    # This is the method to get the inverse of the matrix
    getinverse <- function() m
    
    # Output list
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# To compute the inverse of the special matrix
# Argument parameter: Matrix, Output: Inverse Matrix
cacheSolve <- function(x, ...){
    # Initialize a matrix that is the inverse of x matrix
    m <- x$getinverse()
    
    # Returning a matrix if it is the inverse of x matrix
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    # Get the matrix from the object
    data <- x$get()
    
    # Method to solve the inverse using matrix multiplication
    m <- solve(data, ...)
    
    # Set the inverse of inverse matrix
    x$setinverse(m)
    
    # Returning the matrix
    m
}
