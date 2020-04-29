##Caching the inverse of a matrix.
##Matrix inversion is usually a costly computation and there may be some benefit 
##to caching the inverse of a matrix rather than computing it repeatedly. The 
##pair of functions below create a special object that stores a matrix and 
##caches its inverse.

##the function makeCacheMatrix creates a special "matrix" object that can cache
##its inverse

makeCacheMatrix <- function(x = matrix()) {#initialises x as an empty matrix
        matrixinv <- NULL #initialises matrixinv as an object within the
        #makeCacheMatrix environment, set to NULL. Will hold the inverse of x.
        setmatrix <- function(y) { #defines function to set value of matrix and
                #clear old matrix from cache
                x <<- y #sets the value
                matrixinv <<- NULL #clears the cache
        }
        getmatrix <- function() x #defines function to get the value of the 
        #matrix from the parent environment
        setinverse <- function(inverse) matrixinv <<- inverse #defines function 
        #to set the inverse - used by getinverse only if there is no cached
        #inverse of the matrix
        getinverse <- function() matrixinv #defines function to get the inverse
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse) #returns a list with the above four
        #functions. Naming the list elements allows the $ form of the 
        #extract operator to be used in the cacheSolve function.
}


##The cacheSolve function computes the inverse of the special "matrix" created
##by makeCacheMatrix. It will retrieve the inverse from the cache if it has
##already been calculated and the matrix has not changed.

cacheSolve <- function(make.CacheMatrix.object, ...) { #sets up cacheSolve
        #to be called with the object returned by makeCacheMatrix
        matrixinv.local <- make.CacheMatrix.object$getinverse() #retrieves the 
        #cached value for the inverse
        if(!is.null(matrixinv.local)) { #checks to see if the cached value is
                #empty. If it is not, it is returned with a note to that effect.
                message("getting cached inverse matrix") 
                return(matrixinv.local)
        }
        matrixdata <- make.CacheMatrix.object$getmatrix() #if cache was empty, 
        #gets value
        matrixinv.local.calculated <- solve(matrixdata, ...) #calculates inverse
        make.CacheMatrix.object$setinverse(matrixinv.local.calculated) #caches
        #calculated inverse
        matrixinv.local.calculated #Returns a matrix that is the inverse of 'x'
}