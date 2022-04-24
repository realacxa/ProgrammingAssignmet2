makeCacheMatrix <- function(x = matrix()) {
z<-NULL
  setter<-function(y){
  x<<-y
  a<<-NULL
}
getter<-function() x
setter0<-function(solve) z<<- solve #inverse matrix calculating
getter0<-function() z
list(setter=setter, getter=getter,
   setter0=setter0,
   getter0=getter0)
}

cacheSolve <- function(x, ...) {
     z<-x$getter0()
    if(!is.null(z)){
      message("inverse matrix getting cached")
      return(z)
    }
    ma<-x$get()
    z<-solve(ma, ...)
    x$setter0(z)
    z
}
