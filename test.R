makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
   inv <<- NULL
  }
  get <- function() x
  setmean <- function(solve) inv <<- solve
  getmean <- function() inv
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cacheSolve <- function(x,...) {
  inv = x$getmean()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat.data = x$get()
  inv <- solve(mat.data,...)
  x$setmean(inv)
  return(inv)
}

Cachetest<-function(mat){
  #mat assumed to be invertible square matrix 
  # the function is used to measure the time saved when retrieved directly from cache
  #creating and object in environment and finding the inverse
  temp<-makeCacheMatrix(mat)
  ptm <- proc.time()
  cacheSolve(temp)
  time<-proc.time() - ptm
  print(time)
  #retrieving directly from the cache
  ptm1 <- proc.time()
  print("Retrieving directly from cache")
  cacheSolve(temp)
  time2<-proc.time() - ptm1
  print(time2)
  
}

set.seed(1)
mat<-matrix(rnorm(1000000),1000,1000)
Cachetest(mat)


