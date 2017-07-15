## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Mise en cache de la matrice inverse d'une matrice x
##  passée en paramètre.
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## Fonction permettant d'obtenir à partir du cache
##  la matrice inverse d'une matrice fournie en paramètre.
## Si la matrice inverse n'est pas définie dans le cache, on la calcule.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                ## Récupération de la matrice inverse dans le cache.
                message("getting cached data")
                return(inverse)
        }
        ## Matrice inverse absente du cache, on la calcule
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse
}
