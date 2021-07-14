makeCacheMatrix <- function(x = matrix()){
        salongapio123 <- NULL
        set <- function(y){
                x <<- y
                salongapio123 <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (salongapio123 <<<- inverse}
        getInverse <- function() (salongapio123)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        salongapio123 <- x$getInverse()
        if(!is.null(salongapio123)){
                message("getting cache data")
                return(salongapio123)
        }
        mat <- x$get()
        salongapio123 <- solve(mat, ...)
        x$setInverse(salongapio123)
        salongapio123
}
