## This is Coursera JHU R-programming assignment 2 Xiaoyi Wang's work. Aug, 23, 2015

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setinverse<-function(Matrix)m<<-Matrix  ##give "Matrix" to mean, which has been saved
  getinverse<-function()m
  list(set=set,                           ##list all functions which has been defined
       get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}

cacheSolve <- function(x, ...) {          ## Return a matrix that is the inverse of 'x'
  m<-x$getinverse()
  if (!is.null(m)){                       ## 'x' has been saved, and give notification
    message("m has been saved, getting cached data")
    return(m)
  }
  message("m hasn't been saved, need to be inverse")         ##'x' has not been saved, give notification
  data<-x$get()
  m<-solve(data,...)                      ## solve unsaved data from beginning, and then save it
  x$setinverse(m)
  m
}
