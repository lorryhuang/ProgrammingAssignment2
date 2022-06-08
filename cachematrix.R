## Two functions created to calculate the matrix inversion, 
## or save the result cache for next call


# makeCacheMatrix to initial the processes:
# 1) initial set; 2) get input variable; 3) func inverse, 4) get cache;
makeCacheMatrix <- function(x = matrix()) {
    r <- NULL
    set <- function(y) {
        x <<- y
        r <<- NULL
    }
    get <- function() x
    set_mat_inv <- function(inverse) r <<- inverse
    get_mat_inv <- function() r
    # return list of 4 functions
    list(set = set, get = get, 
         set_mat_inv = set_mat_inv, 
         get_mat_inv = get_mat_inv)
}


## function cache to get saved value or calculate if no cache
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    r <- x$get_mat_inv()
    if (!is.null(r)){
        message("getting cached data")
        return(r)
    }
    data <- x$get()
    # if no cache, calculate it and return
    r <- solve(data, ...)
    x$set_mat_inv(r)
    r
}
