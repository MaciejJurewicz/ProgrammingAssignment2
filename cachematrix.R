

## Function create chache matrix.

makeCacheMatrix <- function(x = matrix()) {
  myMatrix <- NULL
  set<- function(y){
    x<<-y
    myMatrix<<-NULL
  }
  get<-function()x
  setMyMatrix <- function(solve) myMatrix<<-solve
  getMyMatrix <- function() myMatrix
  list(set=set, get=get, setMyMatrix=setMyMatrix, getMyMatrix=getMyMatrix)
}

## Function put entity into matrix.

cacheSolve <- function(x, ...) {
       myMatrix <- x$getMyMatrix()
       if(!is.null(myMatrix)){
         return(myMatrix)
       }
       matrix<-x$get()
       myMatrix<-solve(matrix, ...)
       x$setMyMatrix(myMatrix)
       myMatrix
}
