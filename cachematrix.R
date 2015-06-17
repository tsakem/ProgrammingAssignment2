## The concept is to create a mechanism that will be able to cache calculated results in order to speed up the computations
## In order to do this 2 main functions are needed
## The first one (makeCacheMatrix) will actually contain functions that will allow the user to handle the cached data
## (i.e. set the cached data and recall them when needed)
##The second one (cacheSolve) will actually do the work to check if the inverse of the table has been calculated in the past
##and either recall the cached data or in case they are not cached, to calculate them and cache them consiquently for future use.
##------------------------------------------

## makeCacheMatrix has no mathematical calculations in it
## it just cleans stores the cached data and recalls them when asked
## the functions that start with set are used to assign values to be cached
## the functions that start with get are used to recall values that are cached

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    #the line below stores all the functions in a list in order to have all of them to the assigned object
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


##------------------------------------------
##cacheSolve function checks if the inverse of the matrix has been calculated in the past and has been cashed
##if the inverse is cached then it returns the cached data
##if the inverse has never been cached it calculates the inverse with solve function AND caches the result for future use


cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    ##Checks if the returned data have any value. In case cached data exists, they are returned
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ##Since no data is cached the inverse is calculated
    data <- x$get()
    inv <- solve(data, ...)
    
    x$setinv(inv)   ##push the fresh calculation of the inverse matrix to be cached for future use
    inv
}
