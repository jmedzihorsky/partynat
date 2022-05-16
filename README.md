# partynat

`partynat` is an `R` package for computing indices used in the context of party nationalization research.
It provides more than a dozen different indices,
including standard errors and confidence intervals 
via nonparametric bootstrap, subsampling and jackknife
with optional bias correction.


## Installation

You can install it using `devtools`

`library(devtools)`

`install_github("jmedzihorsky/partynat")`


##  Usage

The main function is 

`partynat(mat, 
          statistic)`




## Indices

|Index|Source|`partynat` call|
|-----|------|---------------|
|Index of Party Aggregation|(Allik 2006)|`"IPA2"`|
|Weighted Party (System) Nationalization Score|(Bochsler 2010)|`"PNSW"`|
|Standardized Party (System) Nationalization Score|(Bochsler 2010)|`"PNS10"`|
|Territorial Coverage Index|(Caramani 2004)|`"TCI"`|
|Index adjusted for Party size and number of Regions|(Caramani 2004)|`"IPR2"`|
|Indicator of Party Aggregation|(Chhibber and Kollman 1998)|`"IPA1"`|
|Inflation Score|(Cox 1999)|`"IS"`|
|Variability Coefficient|(Ersson, Janda, and Lane 1985)|`"VC"`|
|Standardized and Weighted Variability Coefficient|(Ersson, Janda, and Lane 1985)|`"SWVC"`|
|Coefficient of Party Regionalization|(Golosov 2016)|`"CPR"`|
|Index of Party (System) Nationalization|(Golosov 2016)|`"IPN"`|
|Normalized Party (System) Nationalization Score|(Golosov 2016)|`"NPNS"`|
|Normalized Coefficient of Variation|(Golosov 2016)|`"NVC"`|
|Index of Party Regionalization|(Golosov and Ponarin 1999)|`"IPR1"`|
|Party Nationalization Score|(Jones and Mainwaring 2003)|`"PNS"`|
|Lee index|(Lee 1988)|`"Lee"`|
|Inflation Index|(Moenius and Kasuya 2004)|`"II"`|
|Index of variation/Mean Absolute Deviation|(Rose and Urwin 1975)|`"MAD"`|
|Cumulative Regional Inequality|(Rose and Urwin 1975)|`"CRI"`|
|Mean Standard Deviation of row shares| |`"MSD"`|
|Variance of row shares| |`"Var"`|

