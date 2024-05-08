## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL  # Variable pour stocker l'inverse mis en cache
  
  # Définir une nouvelle matrice et réinitialiser l'inverse mis en cache
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # Réinitialiser l'inverse mis en cache
  }
  
  # Obtenir la matrice
  get <- function() x
  
  # Définir l'inverse mis en cache
  setInverse <- function(inv) {
    inverse <<- inv
  }
  
  # Obtenir l'inverse mis en cache
  getInverse <- function() inverse
  
  # Retourner la liste des fonctions
  list(
    set = set,
    get = get,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Vérifier si l'inverse est déjà mis en cache
  inverse <- x$getInverse()
  if (!is.null(inverse)) {
    # Si l'inverse est dans le cache, le renvoyer
    message("Getting cached data")
    return(inverse)
  }
  
  # Sinon, obtenir la matrice
  matrix <- x$get()
  
  # Calculer l'inverse de la matrice
  inverse <- solve(matrix, ...)
  
  # Mettre en cache l'inverse
  x$setInverse(inverse)
  
  # Renvoyer l'inverse calculé
  return(inverse)
}
