Comment utiliser ce package
================

Le Tidy Tuesday est un projet hebdomadaire, issu de la communauté `R4DS
Online Learning Community` et du livre `R for Data Science` (accessible
gratuitement : <https://r4ds.had.co.nz/>). Les données proposées sont un
prétexte pour s’entraîner à manipuler, nettoyer et visualiser des jeux
de données. Certaines visualisations sont notamment postée sur Twitter
(<https://twitter.com/hashtag/tidytuesday?lang=en>).

## Objectifs

  - partager du code pour les tâches récurentes :
    
      - chargement des données
      - nettoyage
      - statistiques
      - visualisation

  - alimenter une base de connaissance commune (Github) et mettre en
    place des bonnes pratiques

  - faire travailler en équipe les R-istes

## Installation du package (pour utiliser les fonctions)

Il faut au préalable installer le package `devtools` si besoin :

``` r
if(!require(package = "devtools", character.only = TRUE)){install.packages(pkgs = "devtools", character.only = TRUE)}

devtools::install_github(repo = "ND-open/nd.tidytuesday")
```

## Comment réutiliser une fonction

Il y a pour le moment (au 16-01-2020) 3 fonctions réutilisables. Il est
possible de lire la doc des fonctions et exécuter les exemples :

``` r
library(nd.tidytuesday)

if(interactive()){ ?base_summary }

example(base_summary)
```

    ## 
    ## bs_smm> base_summary(mtcars)
    ## # A tibble: 11 x 9
    ##    variable   mean       var     sd   min   `1q` median   `3q`    max
    ##    <chr>     <dbl>     <dbl>  <dbl> <dbl>  <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 mpg       20.1     36.3     6.03 10.4   15.4   19.2   22.8   33.9 
    ##  2 cyl        6.19     3.19    1.79  4      4      6      8      8   
    ##  3 disp     231.   15361.    124.   71.1  121.   196.   326    472   
    ##  4 hp       147.    4701.     68.6  52     96.5  123    180    335   
    ##  5 drat       3.6      0.290   0.53  2.76   3.08   3.70   3.92   4.93
    ##  6 wt         3.22     0.96    0.98  1.51   2.58   3.32   3.61   5.42
    ##  7 qsec      17.8      3.19    1.79 14.5   16.9   17.7   18.9   22.9 
    ##  8 vs         0.44     0.25    0.5   0      0      0      1      1   
    ##  9 am         0.41     0.25    0.5   0      0      0      1      1   
    ## 10 gear       3.69     0.54    0.74  3      3      4      4      5   
    ## 11 carb       2.81     2.61    1.62  1      2      2      4      8

et inspecter/améliorer le code :

``` r
base_summary
```

    ## function(df){
    ##         
    ##         if(sum( unlist( lapply(df, is.numeric) ) ) < ncol(df)){stop("Some variable is not numeric.")}
    ##         
    ##         lapply(df, function(x) c("mean" = round(mean(x), 2), 
    ##                                 "var" = round(stats::var(x), 2),
    ##                                 "sd" = round(stats::sd(x), 2),
    ##                                 "min" = min(x),
    ##                                 "1q" = stats::quantile(x, .25, names = FALSE),
    ##                                 "median" = stats::quantile(x, .5, names = FALSE),
    ##                                 "3q" = stats::quantile(x, .75, names = FALSE),
    ##                                 "max" = max(x)
    ##         )
    ##         ) %>% 
    ##                 cbind.data.frame() %>% 
    ##                 t() %>% tibble::as_tibble(rownames = "variable")
    ## }
    ## <bytecode: 0x000000001803aff8>
    ## <environment: namespace:nd.tidytuesday>

## Comment réutiliser un morceau de code

Si un morceau de code doit être copié-collé une fois, une bonne pratique
est d’écrire une fonction pour la tâche concernée. Cette fonction peut
ensuite être partagée à l’aide d’un package (avec éventuellement une
image `Docker`) pour assurer la reproductibilité de ce qui est attendu.
On évite ainsi les problèmes d’OS, de versions (de R, des packages, …).

## Contribuer au projet

Toute contribution est vivement appréciée, que ce soit pour proposer des
fonctions, des vizs ou de l’optimisation de code \!

Pour cela il suffit d’installer `Git`, de cloner le projet, effectuer
**dans une nouvelle branche git** les modifications/ajouts voulus puis
de pousser les changements sur le repository. La génération du site web
statique est automatique.
