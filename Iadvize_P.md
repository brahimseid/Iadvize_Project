Iadvize\_Project
================

Presentation
------------

Depuis ces 5 dernières decenies, l'emission du CO2 et des autres gaz à effet de serre ne cesse d'augmenter à travers le monde. Pour certains, cela est necessaire pour maintenir son niveau de developpement, pour d'autres, on ne peut l'éviter pour esperer rentrer dans un processus de developpement.

Nous allons, à partir des données issues de la banque mondiale, essayer de voir l'évolution de cette emission.

Packages
--------

``` r
library(rvest)
```

    ## Loading required package: xml2

``` r
library(WDI)
```

    ## Loading required package: RJSONIO

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(jsonlite)
```

    ## 
    ## Attaching package: 'jsonlite'

    ## The following objects are masked from 'package:RJSONIO':
    ## 
    ##     fromJSON, toJSON

``` r
library(gridExtra)
```

    ## 
    ## Attaching package: 'gridExtra'

    ## The following object is masked from 'package:dplyr':
    ## 
    ##     combine

``` r
library(FactoMineR)
library(factoextra)
```

    ## Loading required package: ggplot2

``` r
library(wesanderson)
library(explor)
```

``` r
var <- WDIsearch('CO2.* emissions')
# Recupération de la base avec tous les indicateurs
dat<- WDI(indicator= var[1:25], country= "all", start=1967, end= 2014)
```

    ## Warning in WDI(indicator = var[1:25], country = "all", start = 1967, end
    ## = 2014): Unable to download indicators EN.CO2.BLDG.MT ; EN.CO2.ETOT.MT ;
    ## EN.CO2.MANF.MT ; EN.CO2.OTHX.MT ; EN.CO2.TRAN.MT

Constitution de notre base de travail
-------------------------------------

Dans un premier temps, je prendrai en compte que quelques pays pour essayer de voir les différentes trajectoires de cette évolution. Le choix de ces pays tiennent à leur positionnement géographique et aussi de leur ressemblance en terme de developpement. Nous avons donc pris en compte des pays industrialisés (France, USA), en phase d'industrialisation(Chine, Inde, Brésil) et des pays en voie de developpemnt (Tchad,Cameroun,Gabon).

``` r
tada <-  WDI(indicator=c('EN.ATM.CO2E.GF.KT','EN.ATM.CO2E.PC',
                      'EN.ATM.CO2E.KT','EN.CO2.BLDG.ZS', 'EN.CO2.MANF.ZS',
                      'EN.CO2.TRAN.ZS','EN.CO2.ETOT.ZS'),country=c("FR","US","CN","BR","DE",
                                                            "CI","CA","CM","CD","GA","GB",
                                                                   "IN","JP","NG",
                                                                   "NO","RU","TD"), start=1967, end=2014)
```

Nous allons renommer les colonnes pour une bonne compréhension
--------------------------------------------------------------

``` r
names(tada) <- c("iso2c","country","year","ECO2_gaz","ECO2_par_hab","CO2_emission",
                 "ECO2_resid_comm_public","ECO2_manufac_construc","ECO2_transp","ECO2_electr")

names(tada)
```

    ##  [1] "iso2c"                  "country"               
    ##  [3] "year"                   "ECO2_gaz"              
    ##  [5] "ECO2_par_hab"           "CO2_emission"          
    ##  [7] "ECO2_resid_comm_public" "ECO2_manufac_construc" 
    ##  [9] "ECO2_transp"            "ECO2_electr"

Vérification des données/ Tendances centrales et de dispersion
--------------------------------------------------------------

``` r
str(tada)
```

    ## 'data.frame':    816 obs. of  10 variables:
    ##  $ iso2c                 : chr  "BR" "BR" "BR" "BR" ...
    ##  $ country               : chr  "Brazil" "Brazil" "Brazil" "Brazil" ...
    ##  $ year                  : num  1967 1968 1969 1970 1971 ...
    ##  $ ECO2_gaz              : num  257 231 235 154 249 ...
    ##  $ ECO2_par_hab          : num  0.751 0.855 0.907 0.984 1.05 ...
    ##  $ CO2_emission          : num  66193 77421 84315 93762 102636 ...
    ##  $ ECO2_resid_comm_public: num  NA NA NA NA 7.04 ...
    ##  $ ECO2_manufac_construc : num  NA NA NA NA 28 ...
    ##  $ ECO2_transp           : num  NA NA NA NA 48.5 ...
    ##  $ ECO2_electr           : num  NA NA NA NA 14.5 ...

``` r
summary(tada)
```

    ##     iso2c             country               year         ECO2_gaz        
    ##  Length:816         Length:816         Min.   :1967   Min.   :      0.0  
    ##  Class :character   Class :character   1st Qu.:1979   1st Qu.:    196.2  
    ##  Mode  :character   Mode  :character   Median :1990   Median :  11899.4  
    ##                                        Mean   :1990   Mean   : 131646.4  
    ##                                        3rd Qu.:2002   3rd Qu.: 105373.1  
    ##                                        Max.   :2014   Max.   :1432766.6  
    ##                                                       NA's   :49         
    ##   ECO2_par_hab       CO2_emission      ECO2_resid_comm_public
    ##  Min.   : 0.01073   Min.   :      66   Min.   : 0.000        
    ##  1st Qu.: 0.47051   1st Qu.:    5601   1st Qu.: 8.556        
    ##  Median : 3.43528   Median :  307401   Median :11.986        
    ##  Mean   : 5.76069   Mean   :  828342   Mean   :13.195        
    ##  3rd Qu.: 9.49444   3rd Qu.:  814017   3rd Qu.:18.485        
    ##  Max.   :22.51058   Max.   :10291927   Max.   :30.056        
    ##  NA's   :49         NA's   :49         NA's   :103           
    ##  ECO2_manufac_construc  ECO2_transp     ECO2_electr   
    ##  Min.   : 0.00         Min.   : 0.00   Min.   : 0.00  
    ##  1st Qu.:12.16         1st Qu.:17.27   1st Qu.:18.42  
    ##  Median :19.38         Median :26.69   Median :32.95  
    ##  Mean   :21.16         Mean   :29.84   Mean   :30.86  
    ##  3rd Qu.:27.34         3rd Qu.:37.40   3rd Qu.:43.51  
    ##  Max.   :81.25         Max.   :96.78   Max.   :64.98  
    ##  NA's   :103           NA's   :103     NA's   :103

Visualisation des données: la fonction compGrNorm regroupe trois fonctions (boxplot, densité, qq-norm)
------------------------------------------------------------------------------------------------------

``` r
tada[is.na(tada)]<- 0

CompGrNorm(tada$ECO2_gaz) # normalité rejté et defaut de symétrie 
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-1.png)

``` r
CompGrNorm(tada$ECO2_par_hab)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-2.png)

``` r
CompGrNorm(tada$CO2_emission)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-3.png)

``` r
CompGrNorm(tada$ECO2_resid_comm_public)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-4.png)

``` r
CompGrNorm(tada$ECO2_manufac_construc)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-5.png)

``` r
CompGrNorm(tada$ECO2_transp)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-6.png)

``` r
CompGrNorm(tada$ECO2_electr)
```

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-7-7.png) Nous avons donc ici une reprensation de,la distribution de nos variables

Nous allons procedé a quelques tests de corrélations

``` r
cor.test(tada$ECO2_gaz, tada$ECO2_par_hab, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_gaz and tada$ECO2_par_hab
    ## t = 28.164, df = 814, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  0.6721073 1.0000000
    ## sample estimates:
    ##       cor 
    ## 0.7025227

``` r
cor.test(tada$ECO2_gaz, tada$ECO2_electr, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_gaz and tada$ECO2_electr
    ## t = 14.617, df = 814, p-value < 2.2e-16
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  0.4090834 1.0000000
    ## sample estimates:
    ##       cor 
    ## 0.4559588

``` r
cor.test(tada$ECO2_gaz, tada$ECO2_transp, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_gaz and tada$ECO2_transp
    ## t = -0.8697, df = 814, p-value = 0.8076
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  -0.08793805  1.00000000
    ## sample estimates:
    ##         cor 
    ## -0.03046882

``` r
cor.test(tada$ECO2_electr, tada$ECO2_transp, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_electr and tada$ECO2_transp
    ## t = -4.2527, df = 814, p-value = 1
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  -0.2033232  1.0000000
    ## sample estimates:
    ##        cor 
    ## -0.1474269

``` r
cor.test(tada$ECO2_gaz, tada$ECO2_manufac_construc, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_gaz and tada$ECO2_manufac_construc
    ## t = -3.7306, df = 814, p-value = 0.9999
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  -0.1858882  1.0000000
    ## sample estimates:
    ##        cor 
    ## -0.1296534

``` r
cor.test(tada$ECO2_gaz, tada$ECO2_resid_comm_public, alternative = "greater", method = "pearson")
```

    ## 
    ##  Pearson's product-moment correlation
    ## 
    ## data:  tada$ECO2_gaz and tada$ECO2_resid_comm_public
    ## t = 2.802, df = 814, p-value = 0.0026
    ## alternative hypothesis: true correlation is greater than 0
    ## 95 percent confidence interval:
    ##  0.04034396 1.00000000
    ## sample estimates:
    ##        cor 
    ## 0.09774037

Répresentation graphique de l'évolution de l'émission en fonction des variables
-------------------------------------------------------------------------------

``` r
ggplot(tada, aes(year, ECO2_gaz, color=country)) +geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-1.png)

``` r
ggplot(tada, aes(year, ECO2_par_hab, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-2.png)

``` r
ggplot(tada, aes(year,CO2_emission, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-3.png)

``` r
ggplot(tada, aes(year,ECO2_resid_comm_public, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-4.png)

``` r
ggplot(tada, aes(year,ECO2_manufac_construc, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-5.png)

``` r
ggplot(tada, aes(year,tada$ECO2_transp, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-6.png)

``` r
ggplot(tada, aes(year,tada$ECO2_electr, color=country)) + geom_point()+geom_smooth()
```

    ## `geom_smooth()` using method = 'loess'

![](Iadvize_P_files/figure-markdown_github/unnamed-chunk-9-7.png)

Si l'on regarde l'émission du CO2 par habitant, nous constatons que les USA et la Chine domine largemen le debat, et ce, depuis les années 90. La Chine depasse les USA à partir de 2005 et devient ainsi le premier pays qui emet le plus. La Russie et le Japon viennent compléter le podium. Nous remarquons également que les pays en voie de developpement ont un comportement similaire en cette matière et emettent nettement moins par rapport aux autres pays.

les trajjectoires ne sont pas les memes si ont prend en compte les autres variables.

Nous pouvons dire, qu'en fonction de la variable choisie, l'évolution n'est pas la meme.Par exemple, si on considère les emissions liées au gaz, la Russie depasse largement la Chine meme s'il ya un petit decrochage depuis 2010.

Analyse Fcatorielle
-------------------

Nous allons maintenant essayer de faire une acp sur nos données. Nous avons decidé de rajouter d'autres pays pour la réalisation de l'acp

``` r
DPCA <-  WDI(indicator=c('EN.ATM.CO2E.GF.KT','EN.ATM.CO2E.PC',
                         'EN.ATM.CO2E.KT','EN.CO2.BLDG.ZS', 'EN.CO2.MANF.ZS',
                         'EN.CO2.TRAN.ZS','EN.CO2.ETOT.ZS'),country=c("FR","US","CN","BE","BI","BJ","BR","BY","CF","DE","CZ","CH",
                                                        "CI","DJ","CA","CM","DZ","CD","CU","EG","GA","GB","GH","GN",
                                                                      "GQ","GR","IN","IT","JP","KE","KR","MA","ML","MZ","NG","NE",
                                                                      "NO","RU","SD","SE","ZA","TD"), start=1967, end=2014)

DPCA[is.na(DPCA)]<- 0
DPCA<- as.data.frame(DPCA)

names(DPCA) <- c("iso2c","country","year","ECO2_gaz","ECO2_par_hab","CO2_emission",
                 "ECO2_resid_comm_public","ECO2_manufac_construc","ECO2_transp","ECO2_electr")
```

``` r
acp <- PCA(DPCA[,-c(1:4)],scale.unit = TRUE,  ncp = 5, graph = FALSE)
val.prop<- get_eigenvalue(acp) 
```

``` r
fviz_eig(acp, addlabels = TRUE, ylim = c(0, 50)) #Nous pouvons garder 3 axes, representant 76% de l'info 
```

Nous pouvons retenir 3 axes.

``` r
var <- get_pca_var(acp)
fviz_pca_var(acp, axes=c(2,3),col.var = "co2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))
```

Voici une representaion des variables. Nous avons constater un effet de taille sur l'axe 1. Raison pour laquelle, on s'interesse à ce qui se passe sur l'axe 2 et 3.

``` r
explor(acp)
```

Avec explor, nous avons representation dynamqiue de nosn resultats.

``` r
res <- explor::prepare_results(acp)
explor::PCA_var_plot(res, xax = 2, yax = 3,
                     var_sup = FALSE, var_lab_min_contrib = 0,
                     col_var = NULL, labels_size = 16, scale_unit = TRUE,
                     transitions = TRUE, labels_positions = NULL,
                     xlim = c(-1.1, 1.1), ylim = c(-1.1, 1.1))

explor::PCA_ind_plot(res, xax = 2, yax = 3,
                     ind_sup = TRUE,
                     ind_lab_min_contrib = 0, col_var = NULL, 
                     symbol_var = NULL,
                     opacity_var = NULL, size_var = NULL, 
                     size_range = c(10, 300),
                     lab_var = NULL, zoom_callback = NULL, 
                     in_explor = FALSE)
```

Clustering
----------

Nous pouvons pousser l'analyse avec du clustering. on va faire une CAH à partir des resultat de l'acp

``` r
class <- HCPC(acp, graph = FALSE)
```

Conclusion
----------

L'émission du CO2 a augmenté au cours de ces 50 dernières années. Nous l'avons constaté, que des effots doivent etre faits dans tous les secteurs pour reduire ces emissions. L'émission par habitant illustre mieux la position que prend chaque pays. Cela peut etre un levier sur lequel les differents pays doivent ajuster pour reduire les emissions.

Les pays avec des caractéristiques de developpement similiares, emettent des quantités plus ou moins similaires par habitant.
