## Defines 2 function objects. One returns a list object which
## returns a list object which stores a matrix and its inverse.
## The other caches inverse of matrix stored in the list object
## if not existing already and returns that cached value if 
## it does previously exist.

## Function creates and returns a list object with 4 function
## entries 2 that set and get an invertible matrix 
## get/set its inverse.  Optional constructor for initial
## value of matrix when list object is created. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Functon takes list object created by makeCacheMatrix
## If said list object has matrix inverse computed already
## it simply returns value of inverse, if not computes inverse
## with solve() function and stores it in list object.
## Return value is inverse of matrix stored in list object's 
## environment.
 
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        print("no previously computed inverse")
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
