# > m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
# > myMatrix_object <- makeCacheMatrix(m1)
# should return exactly the matrix n1
# > cacheSolve(myMatrix_object)
#
# [,1] [,2]
# [1,]    6    8
# [2,]    2    4


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(is.null(inv)) {
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
  }
  inv
}