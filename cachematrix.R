
## @author alvin
## @description answer to ProgrammingAssignment2 


## In this code, we introduce the <<- operator which can be used to assign a value to an object in an environment 
## that is different from the current environment. 
## Below are two functions that are used to create a special object that stores a matrix and cache's its inverse value.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## 1.set the value of the matrix
## 2.get the value of the matrix
## 3.set the value of the inverse of the matrix
## 4.get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

	## use variable inv to store the inverse of the matrix x
	inv <- NULL
	## set function: set the value of y to x, but keep inv NULL.
	## if you learned any object-oriented language, like Java, you will understand the set and get methods easily.
	set <- function(y){
		x <<- y
		inv <<- NULL
	}
	## get function: get the value of matrix x
    get <- function() x
	## setinverse function: set the inverse value to the member variable inv 
    setinverse <- function(inverse) inv <<- inverse
	## getinverse function: get the value of member variable inv 
    getinverse <- function() inv
	
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## The following function calculates the inverse of the special "matrix" created with the above function. 
## However, it first checks to see if the inverse of the matrix has already been calculated. 
## If so, it gets the inverse value from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the inverse value of the matrix in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
	
	## First, we get the inverse value of object x
	inv <- x$getinverse()
	## if inv is not null, that means we have computed the inverse of matrix x and we stored the value in variable inv, 
	## so we just use the value of inv, and needn't recompute the inverse of matrix x.
    if(!is.null(inv)) {
		
        message("getting cached data")
        return(inv)
    }
	## if inv is null, that means we haven't computed the inverse of matrix x before, 
	## so we need to get the matrix value of special "matrix" x, compute the inverse of the matrix, 
	## and store the result to member variable inv of the special "matrix" x 
	## Attention : the variable inv defined in cacheSolve function is not same as the variable inv defined in makeCacheMatrix function
	## the variable inv defined in cacheSolve function is a local variable 
	## while the variable inv defined in makeCacheMatrix function is a member variable.
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
		
}
