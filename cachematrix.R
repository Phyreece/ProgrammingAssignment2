# The makeCacheMatrix() takes a matrix, calculates its inverse and 
# and stores its inverse. The cacheSolve() takes a matrix, attempts
# to retrieve the the stored inverse if it exists in cache, or 
# calculates it otherwise.

# The function makeCacheMatrix takes a matrix and returns a list 
# of functions:
# 1. The function set() returns a matrix;
# 2. The function get() returns the input matrix;
# 3. The function setinv() calculates the inverse of the input matrix;
# 4. The function getinv() stores/returns the inverse of the input matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y) {
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setinv<-function(solv) inv<<-solv
  getinv<-function() inv
  list(set=set,get=get,getinv=getinv,setinv=setinv)
}

# The cacheSolve() function takes a matrix, first attempts to retrieve 
# the stored inverse, and checks for its existence. If it hasn't been 
# cached, the function calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv<-x$getinv()
  if(!is.null(inv)) {
    message('getting cached inverse...')
    return(inv)
  }
  dat<-x$get()
  inv<-solve(dat)
  dat$setinv(inv)
  inv
}
