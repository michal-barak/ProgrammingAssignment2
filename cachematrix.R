## This is an implementation of finding invers of a matrix and saving it 
## to avoid unnecessary redundant calculations

## makeCachMAtrix creates a mtrix with a list of commands that get and set
## the matrix, and get and set the invers


makeCacheMatrix <- function(x = matrix()) {
    invrs <- NULL
    set <- function (y) {
        x <<- y
        invrs <<-NULL
    }
    
    get <- function () x
    setinvers <- function(invers){        
        invrs <<- invers        
    }
    getinvers <- function() {        
        invrs
    }
    list(set = set, get = get,
         setinvers = setinvers,
         getinvers = getinvers)
}


## cacheSolve returns the invers of the input matrix, if cashed, from the 
## cashed value, else - or if the matrix was changed, it is calculated again

cacheSolve <- function(x, ...) {
        invrs <- x$getinvers()        
        if (!is.null(invrs)){
            print("from cashed data")
            return (invrs)
        }
        
        data <- x$get()
        invrs <- solve(data, ...)
        x$setinvers(invrs)
        invrs        
}
