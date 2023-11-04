library(MASS)
makeCacheMatrix <- function(x= matrix()) {
  inv<-NULL
  
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<- function()x
  
  setinv<-function (inverse) inv<<-inverse
  getinv<- function(){
    inver<-ginv(x)
    inver%*%x
    
  }
  list (set= set, get = get,
        setinv= setinv,
        getinv = getinv)
}


cachesolve <- function(x, ...) {
  inv<-xSgetinv()
  if(!is.null(inv)){
    message("getting cached data!")
    return(inv)
  }
  data<-xiget ()
  inv<-solve(data,...)
  x$setinv(inv)
  inv
  }