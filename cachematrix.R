## Function "makeCacheMatrix" creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # inverse matrix
    im<-NULL
    
    # set the value of 'y' to x, and set the value of im to NULL
    set<-function(y){
        x<<-y
        im<<-NULL
    }
    
    # get the value of the matrix 
    get<-function() x
    
    # set the value of the inverse matrix
    setim<-function(inverse)    im<<-inverse
    
    # get the value of the inverse matrix
    getim<-function(){
        im
    }
    
    # creat a list containing a function to abov
    list(set = set, get = get, setim = setim, getim = getim)
}


## Function "cacheSolve" computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # get inverse matrix from x and then store it into 'im'. 
    im<-x$getim()
    
    # if im is already cached, return it.
    if(!is.null(im)){
        message("getting cached inverse matrix")
        return(im)
    }
    
    # else  get matrix form x and then calculate inverse matrix 'im' by solve() 
    data<-x$get()
    im<-solve(data)
    
    # store inverse matrix into x.
    x$setim(im)

    # return inverse matrix
    return(im)
}
