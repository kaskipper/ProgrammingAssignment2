## Put comments here that give an overall description of what your
## functions do. 
## The makeCacheMatrix function can be thought of as the scaffold that cacheSolve is built on. Here the setters and getters called in cacheSolve are defined, as is the list that creates the special "matrix" object that caches the inverse. cacheSolve utilizes the functions in makeCacheMatrix to 1: check the cache for already calculated data and 2: to calculate the inverse of a matrix if it is not found in the cache. 

## Write a short comment describing this function
## The makeCacheMatrix is basically a collection of setters and getters. The function takes a argument in the form of a matrix. It then sets the value of the matrix and clears any data previously stored. It then retrieves the value of the matrix, sets the value of the inversed matrix and gets the value of the inversed matrix. All from the parent environment of makeCacheMatrix. Finally, the function returns a list with each element named, thereby enabling us to use the $ operator to extract the contents of the function. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function () x
        setSolve <- function(solve) m <<- solve
        getSolve <- function() m
        list(set = set, get = get, setSolve = setSolve, getSolve = getSolve)      
}


##  Write a short comment describing this function
##  The function first checks if the inverse matrix is already stored in the cache. If so, it retrieves and returns the cached inverse matrix. If the inverse matrix is not found in the cache, the function gets the x matrix from the makeCacheMatrix object, calculate its inverse, defines the inverse in the MakeCacheMatrix (x$setSolve(m)) and returns the answer to the parent environment.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getSolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setSolve(m)
        m
}
