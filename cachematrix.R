## The function stores the inverse of a matrix once 
##computed into the cache so repeated computation is not required

## takes matrix and calculates inverse of that matrix

makeCacheMatrix <- function(x = inverse()) {
  i<-NULL
  set<-function(y){
    x<<-y
    i<<-NULL
  }
  get<-function()x  
  setinverse<-function(inverse) i<<-inverse
  getinverse<-function() i
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## It checks if the inverse of the matrix is in the cache and if not present 
##it will calculate the inverse of the matrix and set it in cache 

cacheSolve <- function(x, ...) {
  i<-x$getinverse()## Return a matrix that is the inverse of 'x'
  if(!is.null(i)){
    message("getting cached data")
    return (i)
  }
  data<-x$get()
  i<-inverse(data,...)
  x$setinverse(i)
  i
}



