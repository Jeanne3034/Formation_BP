decennie_a_partir_annee <- function(annee) {
  return(annee - annee %%
           10)
}




#' Fonction de statistiques agrégées
#'
#' @inheritParams mean 
#' @param stat La statistique qu'on désire calculer sous forme de 
#' caractère (défaut : "moyenne")
#'
#' @return La statistique agrégée sélectionnée sur la variable x
#'
#' @examples
#' calcul_stat_desc(rnorm(10))
#' calcul_stat_desc(rnorm(10), "ecart-type")
#' calcul_stat_desc(rnorm(10), "variance")

calcul_stat_desc <- function(a, stat = "moyenne", ...) {
  if (stat == "moyenne") {
    resultat <- mean(a, na.rm = TRUE, ...)
  } else if (stat == "ecart-type" || stat == "sd") {
    resultat <- sd(a, na.rm = TRUE, ...)
  } else if (stat == "variance") {
    resultat <- var(a, na.rm = TRUE, ...)
  }
  return(resultat)
}