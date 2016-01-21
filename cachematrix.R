## Caching the Inverse of a Matrix:
## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a pair of functions that are used to create a special object that 
## stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse.

# This function has 2 variables internally: x and m  
# x is the matrix we need to deal with
# inv stores the calculated inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        #variable defined inside the function
        inv <- NULL
        
        
        # we used global assignment operator
        # so instead of creating a new variable inside the function x,
        # set function will pass the value of y to x, which is the input variable of makeVector
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        # constant function, returns the matrix we need to deal with
        get <- function() x
        
        # pass the computed inverse matrix to inv
        setInverse <- function(inverse) inv <<- inverse
        
        # constant function, returns inv, the inverse matrix
        getInverse <- function() inv
        
        #makecachematrix returns a list with 4 elements, each is a function previously defined
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.


#the input argument of this function must be a "makeCacheMatrix"
cacheSolve <- function(x, ...) {
        # x$getinverse() calls on the getinverse function inside makeCache
        # i.e. it's checking whether we have already got the inverse
        inv <- x$getInverse()
        if (!is.null(inv)) {
                # inv is not null, meaning we already computed previously,
                # so return result directly
                message("getting cached data")
                return(inv)
        }
        
        # if function proceeds here, it means we haven't computed inverse previously
        # call the get function inside x, to get the raw matrix
        mat <- x$get()
        # compute the inverse
        inv <- solve(mat, ...)
        
        # call the setInverse function inside x, store the results in x
        # so next time we call cacheSolve, x already has the cached result,
        # it will return the value in the if parts above, no need to compute again.
        x$setInverse(inv)
        # return the result
        inv
}