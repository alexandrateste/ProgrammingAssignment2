## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
## The functions makeCacheMatrix and cacheSolve enable users to compute   |
## the inverse of an input square matrix and to cache it.                 |
## When called at least twice in a row for a same matrix,                 |
## cacheSolve retrieves the already computed (and cached) inverse matrix. |
## For a new matrix, cacheSolve computes the inverse matrix from scratch. |
## - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

## The function makeCacheMatrix returns a list of functions that allow to:
##   1. Get the values of the input matrix
##   2. Set its values when the matrix is new
##   3. Get the inverse of this matrix
##   4. Set the values of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        # Initializes (or resets) the inverse matrix to an empty one
        inverse <- matrix()
        # Function that:
        #   1. Assigns the new input matrix to x that will be used below
        #   2. Sets the inverse matrix to an empty one
        #      in case one is still being cached
        #      while we changed the input matrix
        set <- function(y) {
                x <<- y
                inverse <<- matrix()
        }
        # Function that extracts the values of the input matrix
        get <- function() x
        # Function that sets the inverse matrix computed by cacheSolve (cf. below)
        setinverse <- function(invrs) inverse <<- invrs
        # Function that extracts the values of the inverse matrix
        getinverse <- function() inverse
        # makeCacheMatrix returns the list of 4 functions described above
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}

## The function cacheSolve returns the inverse of the matrix created by makeCacheMatrix.
## If this inverse matrix already exists and has been cached, this function retrieves it.
## Otherwise, cacheSolve computes the inverse matrix requested and caches it.

cacheSolve <- function(x) {
        # Assigns the inverse of matrix x to 'inverse':
        #  - If the inverse has not been computed yet, it is empty (i.e. matrix())
        #    --> nrow = 1, ncol = 1 *AND* inverse[1,1] = NA
        #  - If it is already cached, it is filled
        inverse <- x$getinverse()
        # Determines the size of 'inverse'
        nberrows <- nrow(inverse)
        nbercols <- ncol(inverse)
        if((nberrows != 1) | (nbercols != 1) | !is.na(inverse[1,1])) {
                # If the inverse matrix is found, then nrow != 1
                # (equivalent to ncol !=1, since we deal with square matrices)
                # *OR* inverse[1,1] != NA
                # In that case, returns the inverse matrix
                # and exits the function
                # This also works with 1x1 'matrices'
                message("getting cached data")
                return(inverse)
        }
        # If the inverse matrix is not found:
        #   1. gets the values of the initial matrix via x$get()
        #      and stores them in data
        #   2. computes the inverse matrix via solve(data)
        #   3. sets the inverse matrix of x via x$setinverse
        #   4. returns the inverse matrix
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}

##Examples of usage:
## Example 1:
## > xx<-matrix(c(4,5,6,10,12,15,25,81,49),3,3)
## > vv<-makeCacheMatrix(xx)
## > ww<-cacheSolve(vv)
## > ww<-cacheSolve(vv)
##   getting cached data
## > ww %*% xx   - checks that matrix * its inverse = identity matrix

## Example 2:
## > xx<-matrix(c(4),1,1)
## > vv<-makeCacheMatrix(xx)
## > ww<-cacheSolve(vv)
## > ww<-cacheSolve(vv)
##   getting cached data
## > ww %*% xx   - checks that matrix * its inverse = identity matrix
