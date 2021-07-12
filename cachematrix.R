makeCacheMatrix <- function(x = matrix()){
        du5667 <- NULL
        set <- function(y){
                x <<- y
                du5667 <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) (du5667 <<<- inverse}
        getInverse <- function() (du5667)
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        du5667 <- x$getInverse()
        if(!is.null(du5667)){
                message("getting cache data")
                return(du5667)
        }
        mat <- x$get()
        du5667 <- solve(mat, ...)
        x$setInverse(du5667)
        du5667
}
