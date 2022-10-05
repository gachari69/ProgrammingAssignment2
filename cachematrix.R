## 2 Functions that are used to create a special object  that stores, both a matrix, and caches its inverse.
## Benefit to caching the inverse of a matrix rather than repeatedly computing it.
## Caching the Inverse of a Matrix.
## Create a special matrix object that can cache its inverse 
##makeCacheMatrix consists of set, get , setInv, getInv

 makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL                ## initialize inverse 
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x    ##function to get matrix x
+     setInverse <- function(inverse) inv <<- inverse
+     getInverse <- function() inv
+     list(set = set,
+          get = get,
+          setInverse = setInverse,
+          getInverse = getInverse)
+ }


## This function computes  the inverse of the special MATRIX created.
##  If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
 


