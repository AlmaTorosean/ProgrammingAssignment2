## Below are two functions that are used to create a special object 
# that stores a matrix and caches its inverse.  In both functions the <<- operator
# was used so that assign a value to an object from either function.

## The makeCacheMatrix function creates a list containing 4 functions:
# set which set the value of the matrix
# get which gets the value of the matrix
# setinv which sets the value of the matrix inverse
# getinv which gets the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {                    #changes the matrix stored in a main function
        x <<- y                             #substitutes matrix x with y
        i <<- NULL                          #restore to null the value of inverse i
    }
    get <- function() x                     #returns the input matrix
    setinv <- function(solve) i <<- solve   #sets inverse matrix
    getinv <- function() i                  #returns inverse matrix
    list(set = set, get = get,              #returns a list of 4 functions
         setinv = setinv,
         getinv = getinv)
}


## The cacheSolve function calculates the inverse of the matrix created in makeCacheMatrix function
# First it check if inverse has been already calculated.  If so it returnes cached inverse
# If inverse wasn't calculated before it calculates the inverse and stores it in cache.

cacheSolve <- function(x, ...) {
    i <- x$getinv()                         #get inversed matrix
    if(!is.null(i)) {                       #if this matrix is not null (has values)
        message("getting cached data")      #display a message about getting values from cache
        return(i)                           #and return cached inversed matrix
    }
    data <- x$get()                         #otherwise, if inversed matrix is null
    i <- solve(data, ...)                   #calculate inversed matrix
    x$setinv(i)                             #store it 
    i                                       #and return calculated inversed matrix
}
