makeCacheMatrix <- function(x = matrix()) {##cACHE
  +     inv <- NULL ##ZERO MATRIX
  +     get <- function() x ##FUNCTION
  +     set <- function(y) {
    +         x <<- y
    +         inv <<- NULL ##INVERSE MATRIX
    +     }
  +     getinv <- function() inv ##SETFUNCTION AS
  +     setinv <- function(inverse) inv <<- inverse
  +     list(get=get, set=set, getinv=getinv, setinv=setinv)
  +     ##MTX
      + }
cacheSolve <- function(x, ...) { ##SOLVING FOT THE INVERSE
  
  +     inv <- x$getinv() ##FUNCTION FOR INVERSE
  +     if (!is.null(inv)) {
    +         message("inverse is cached") ##RESULT
    +         return(inv) ##RETURN TO INVERSE MATRIX
    +     }
  +     m <- x$get()
  +     inv <- solve(m, ...) ##SOLVE FOR THE MATRIX
  +     x$setinv(inv)
  +     return(inv) ##RETURN TO INVERSE FUNCTION
  + }
##USED FOR SOLVING INVERSE MATRIX USING RSTUDIO