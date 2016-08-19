#second programming assignment Caching the Inverse of a Matrix:

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
#using cacheSolve to use the data CacheMatrix and solve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("retrieve the cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

#source test data  (uncomment single # lines to run and test)


#x = rbind(c(1, -10), c(--10, 1))
# m = makeCacheMatrix(x)
# m$get()
##produces a 2x2 row bound matrix
#[,1] [,2]
#[1,]    1  -10
#[2,]   10    1
## creates inverse of 2x2 matrix from cashe
# cacheSolve(m)
#[,1]       [,2]
#[1,]  0.00990099 0.09900990
#[2,] -0.09900990 0.00990099
#ran cacheSolve(m) again to verify
#cacheSolve(m)
#retrieve the cached data
#[,1]       [,2]
#[1,]  0.00990099 0.09900990
#[2,] -0.09900990 0.00990099
