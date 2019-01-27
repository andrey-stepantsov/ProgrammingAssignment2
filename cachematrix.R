## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_imatr()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_imatr(m)
        m
}

cacheSolveExample <- function(n = 4) {
    #creating a sample matrix
    m1 <- round(matrix(runif(n * n, 0, 10), nrow = n, ncol = n))
    
    a1 <- makeCacheMatrix(m1)
    b1 <- cacheSolve(a1)
    print( round(b1 %*% a1$get()) )
}
cacheSolveExample()

cacheSolveDemo <- function(n = 4) {

    P<-function(x)print(paste(x, sep="\n"))

    P(c("# create a n*n matrix initialized with random numbers",
        "m1 <- matrix(runif(n * n, 0, 10), nrow = n, ncol = n)"))
    m1 <- round(matrix(runif(n * n, 0, 10), nrow = n, ncol = n))

    P("---m1:")
    print(m1)

    P('a1 <- makeCacheMatrix(m1)')
    a1 <- makeCacheMatrix(m1)

    P("---a1:")
    print(a1$get())

    P('b1 <- cacheSolve(a1)')
    b1 <- cacheSolve(a1)

    P("---b1:")
    print(b1)

    P(c("","multiplication of a1 and b1 must be an identity matrix",""))
     
    P("b1 %*% a1$get()")
    print( round(b1 %*% a1$get()) )
    b1
}

#cacheSolveDemo()
