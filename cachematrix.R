## The following pair of functions work together.
##
## The first of the pair, makeCacheMatrix, constructs
## a wrapper object that can hold a matrix and cache
## its inverse.
## The second function is used to get the cached inverse
## of the matrix in the object created with the first
## function. If an inverse value in not yet cached, it is
## computed, then cached.

# Function 1:
# Creates a wrapper object capable of caching
# the inverted matrix. The object also exposes
# a getters and a setters for the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Initialize variable to hold the inverted matrix.
    xinv = NULL;
    
    # Matrix setter.
    set <- function(matrix) {
        x <<- matrix;
        xinv <<- NULL;
    }
    
    # Matrix getter.
    get <- function() {
        # Variable x is defined in the environment
        # in which this function is declared and is
        # therefore captured in the closure.
        x;
    }
    
    # Inverse setter.
    setinv <- function(inversematrix) {
        xinv <<- inversematrix;
    }
    
    # Inverse getter.
    getinv <- function() {
        xinv;
    }
    
    # Returns wrapper object.
    list(set = set, get = get, setinv = setinv, getinv = getinv);
}


# Takes a wrapper object created by makeCacheMatrix()
# as an argument and returns the inverse of the matrix 
# within that object. If a cached inverse is not found,
# one is computed and stored.
cacheSolve <- function(x, ...) {
    # Get the cached inverse matrix.
    xinv <- x$getinv();
    
    # Check if the value in non-null.
    if( ! is.null(xinv) ) {
        # If so, return it (with a message!).
        message("getting cached data");
        return(xinv);
    }
    
    # If the value was null, get the matrix.
    matrix <- x$get();
    
    # Invert it.
    inversematrix <- solve(matrix, ...);
    
    # Cache the inverse value and return it.
    x$setinv(inversematrix);
    inversematrix;
}
