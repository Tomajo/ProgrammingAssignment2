##Serveix per calcular la inversa d'una matriu només si no s'ha calculat abans.
##fa us del <<- que serveix per fer l'assignació amb un altre scope.
##Primer has de cridar la funció makeCache matrix passant una matriu
##DEsprés pots demanar la inversa (CacheSolve) tantes vegades com vulguis, només et calcularà la primera vegada o si 
##ha canviat

## Es un objecte que retorna una llista de funcions on hi ha gets i sets tant de la matriu en si com de la inversa
##Se li passa una matriu, retorna una llista de funcions

makeCacheMatrix <- function(x = matrix()) {
  
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(inve) inverse <<- inve
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}        



##Mira si la matriu ja està calculada.
## Calcula la inversa de la matriu si no ho està
##Se li passa una llista creada a makeCacheMatrix, et retorna una matriu inversa.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        #print('Abans del if')
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
