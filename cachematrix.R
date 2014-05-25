##Tarea 2 en las que hay que programar funciones que generen una matriz y almacene en caché su valor inverso

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL ##Guarda la matriz en cache
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve ##Invierte Matriz
        getInverse <- function() m  ##Tiene la Matriz inversa
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)  ##Generamos las funciones
}

cacheInverse <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)) {                         ##Comprobación de si fue calculado
                message("COMPROBANDO CACHE")    ##Mensaje de la operación
                return(m)
        }
        data <- x$get()                           ##Consigue la matriz calculada antes
        m <- solve(data, ...)                     ##La invierte dicha matriz
        x$setInverse(m)                           ##Almacena en caché
        m
}
