#Below Functions demonstrate the caching functionality 
#avaialble in R. Caching is useful in reusing the computations
#performed. Below example caches the matrix inversion computation
#and retreives the inverse from cache, when the same operation is 
#performed again.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    matrix_inv <- NULL
    set <- function(y) {
        x <<- y
        matrix_inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) matrix_inv <<- inverse
    getinverse <- function() matrix_inv
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Below function determines the inverse of the matrix. It first checks whether
# the inverse has already been computed. If yes, it reuses the result from cache 
# and skips the# computation, the inverse is computed

cacheSolve <- function(x, ...) {
    matrix_inv <- x$getinverse()
    if(!is.null(matrix_inv)) {
        message("getting cached data.")
        return(matrix_inv)
    }
    data <- x$get()
    matrix_inv <- solve(data)
    x$setinverse(matrix_inv)
    matrix_inv
}

#Steps to run
#Create a matrix
# > x <- matrix(c(4,3,2,1),nrow = 2, ncol = 2, byrow = true,)
# Call makeCacheMatrix() passing x as argument and the store the result to obj
# > obj <- makeCacheMatrix(x)
# Let's check whether inverse is calculated..we can get this gtom getinverse function
# > obj$getinverse()
# Above statement returns NULL as the inverse is not computed yet, So, if we run cacheSolve
# the inverse get computed.
# > cacheSolve(obj)
#     [,1] [,2]
#[1,] -0.5  1.5
#[2,]  1.0 -2.0
#Now, let's call obj$getinverse()
#> obj$getinverse()
# [,1] [,2]
#[1,] -0.5  1.5
#[2,]  1.0 -2.0
#Finally, Lets try calling cacheSolve again
#>cacheSolve(obj)
#getting cached data.
#     [,1] [,2]
#[1,] -0.5  1.5
#[2,]  1.0 -2.0



