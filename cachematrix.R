## The functions in this module are to support caching of 
## computation of inverse matrix that is potentially a
## time-consuming computation.
##
## To try these functions, source this file in R and call
##      cacheSolveDemo()
##      cacheSolveExample()
##
## Usage
## 
##      a1 <- makeCacheMatrix(m1)
##      b1 <- cacheSolve(a1)
##
## Where variable a1 is special cached matrix (so, regular
## matrix operations can not be allpied to it.) And,
## the variables m1 and b1 are regualr matricies: m1 is
## the source matrix and b1 is the inverse matrix of m1.
##
## Limitations
##
## The functions assume that the imput matrix are inversible
## 

#' makeCacheMatrix
#' 
#' create a special object that caches result of computatios
#' perfromed by the standard 'solve' function.
#' 
#' method '$get' of a chach matrix allows to recall original
#' matrix wrapped insed the cache matrix
#'
#' @param x regular matrix to cache
#' 
#' @return cache matrix
#' 
#' @seealso 'cacheSolve'
#'
#' @examples
#' \dontrun{
#'     m1 <- round(matrix(runif(n * n, 0, 10), nrow = n, ncol = n))
#'
#'     a1 <- makeCacheMatrix(m1)
#'     b1 <- cacheSolve(a1)
#'     print( round(b1 %*% a1$get()) ) # prints an identity matrix of m1 
#'
#' }
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_imatr <- function(imatr) m <<- imatr
        get_imatr <- function() m
        list(set = set, get = get,
             set_imatr = set_imatr,
             get_imatr = get_imatr)
}

#' cacheSolve
#'
#' @param x special cache matrix created by 'makeCacheMatrix'
#'          functon
#' @param ... the params to pass to the 'solve' function 
#'
#' @return  inverse matrix of cache matrix (regular R matrix)
#' 
#' @seealso 'makeCacheMatrix' 'solve
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_imatr()
        if(!is.null(m)) {
                # message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_imatr(m)
        m
}

cacheSolveExample <- function(n = 4) {
    #creating a sample matrix
    message("cahceSolveExample:")
    m1 <- round(matrix(runif(n * n, 0, 10), nrow = n, ncol = n))
    a1 <- makeCacheMatrix(m1)
    b1 <- cacheSolve(a1)
    message("input matrix:")
    print( a1$get() )
    message("inverse matrix:")
    print( cacheSolve(a1))
    message("identity matrix:")
    print( round(b1 %*% a1$get()) )
    invisible()
}

cacheSolveDemo <- function(n = 4) {
    message("# cacheSolveDemo")
    message("# create a n*n matrix initialized with random numbers")
    message("# m1 <- matrix(runif(n * n, 0, 10), nrow = n, ncol = n)")
    m1 <- round(matrix(runif(n * n, 0, 10), nrow = n, ncol = n))

    message("# ---m1:")
    print(m1)

    message('# a1 <- makeCacheMatrix(m1)')
    a1 <- makeCacheMatrix(m1)

    message("# ---a1:")
    print(a1$get())

    message('# b1 <- cacheSolve(a1)')
    b1 <- cacheSolve(a1)

    message("# ---b1:")
    print(b1)

    message("# multiplication of a1 and b1 must be an identity matrix")
     
    message("# b1 %*% a1$get() :")
    print( round(b1 %*% a1$get()) )
    invisible()
}

# cacheSolveDemo(7)
# cacheSolveExample(5)
