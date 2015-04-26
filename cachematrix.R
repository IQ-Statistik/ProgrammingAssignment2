## The functions of this file allow to store a matrix in a special object that is able to cache the inverse
## of the matrix. The inverse can be calculated from the object or in case it has already been calculated can be
## retrieved from the cache. Every change to the matrix results in setting the variable inverse to NULL, every
## calculation of the matrix sets the variable inverse to the interted matrix.

## Function makeCacheMatrix can create a new matrix with $set, get a stored matrix with $get, calculate and store the
## inverse of the matrix with $setInverse an get the stored inverse with $getInverse.

makeCacheMatrix <- function(x=matrix()) {
        inverse<-NULL
        set<-function(y) {
                x<<-y
                inverse<<-NULL
        }
        get<-function() x
        setInverse<-function(inv) inverse<<-inv
        getInverse<-function() inverse
        list(set=set, get=get,
             setInverse=setInverse,
             getInverse=getInverse)
}

## Function cacheSolve calculates the inverse of the matrix if it has not yet been calculated. If it has already been
## calculated, the function retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
        inverse<-x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data<-x$get()
        inverse<-solve(data)
        x$setInverse(inverse)
        inverse
}