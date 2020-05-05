## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is used as a function to create a matrix that can cache its 
## inverse for the input

makeCacheMatrix <- function(x = matrix()) {
  j<-NULL
  set<-function(y){
    x<<-y
    j<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) j<<-inverse
  getInverse<-function()j
  list(set=set,get=get,
       setInverse=setInverse,
       getInverse=getInverse)

}


## Write a short comment describing this function
## CacheSolve is a function which computes the inverse of the matrix returned as 
## the cacheMatrix above. If inverse has already been calculated then cacheSolve 
## gives the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  j<-x$getInverse()
  if(!is.null(j)){
    message("getting cached data")
    return(j)
  }
  mat<-x$get()
  j<-solve(mat,...)
  x$setInverse(j)
  j
}
