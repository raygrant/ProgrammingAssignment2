## Function to cache the inverse of a matrix
## 1. Set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

## Function design uses the makeVector function provided in
## the R programming course as a template

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setmatrix<-function(solve) m<<- solve
  getmatrix<-function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)

}


## Function to calculate the inverse of the matrix returned by makeCacheMatrix
## It first checks to see if the inverse of the matix has already been 
## calculated. If so it gets the inverse of the matrix from the cache and skips
## the computation. Otherwise it calculates the inverse of the matrix and sets
## it in the cache. 

## Function design uses the cachemean function provided in
## the R programming course as a template

cacheSolve <- function(x, ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
