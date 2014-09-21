##aching the Inverse of a Matrix
##to write a pair of functions that cache the inverse of a matrix.
##Write the following functions:
##1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

##Computing the inverse of a square matrix can be done with the solve function in R. 
##For example, if X is a square invertible matrix, then solve(X) returns its inverse.

##assume that the matrix supplied is always invertible.

makeCacheMatrix <- function (m = matrix() ) {
             i <- NULL
             set <- function (matrix)  {
                  m <<- matrix
                  i <<- NULL
             }
             get <- function() {
                    m
             }
             setInverse <- function(inverse) {
                      i <<- inverse
             {
             getInverse <- function () {
                      i
             }
             list (set = set, 
                   get = get, 
                   setInverse = setInverse
                   getInverse = getInverse
                   )
}
      

## 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has 
## already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x, ...) {
         m <- x$getInverse()
         if (!is.null(m)) {
                 message ("getting cached data")
                 return (m)
         }
         data <- x$get ()
            m <- solve(data) %*%data
            x$setInverse(m)
            m
}

