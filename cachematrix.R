## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mtx <- NULL
  set <- function(y){
    x <<- y
    mtx <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) mtx <<- solve
  getinverse <- function() mtx
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  mtx <- x$getinverse()
  if(!is.null(mtx)){
    message("getting cached data")
    return(mtx)
  }
  data <- x$get()
  mtx <- solve(data, ...)
  x$setinverse(mtx)
  mtx
  ## Return a matrix that is the inverse of 'x'
}


# Test inverse matrix assignment

m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
m1

I2 <- matrix(c(1,0,0,1), nrow = 2, ncol = 2)
I2

n1 <- matrix(c(6,2,8,4), nrow = 2, ncol = 2)
n1

m1 %*% n1
n1 %*% m1

solve(m1)
solve(n1)

obj <- makeCacheMatrix(m1)

cacheSolve(obj)

n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)

obj$set(n2)

cacheSolve(obj)