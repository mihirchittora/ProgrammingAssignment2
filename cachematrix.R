## These file will do the Caching of Inverse of a Matrix

## This function will crate and cache the matrix

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
          x<<-y
          m<<-NULL
        }
        get<-function() x
        setInvMatrix<-function(invMatrix){
          m<<-invMatrix
        }
        getInvMatrix<-function()m
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}


## This function will return a new/cached inverse matrix of the passed matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInvMatrix()
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setInvMatrix(m)
        m
}
