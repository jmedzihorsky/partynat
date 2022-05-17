# partynat

`partynat` is an `R` package for computing indices used in the context of party nationalization research.
It provides more than a dozen different indices,
including standard errors and confidence intervals 
via nonparametric bootstrap, subsampling and jackknife
with optional bias correction.


It's a companion package to
[Medzihorsky, Juraj. (2022). Unifying the Measurement of Variation in Electoral Support]()

## Installation

You can install it using `devtools`

`library(devtools)`

`install_github("jmedzihorsky/partynat")`


##  Usage

The main function is 

`partynat(mat, 
          statistic, 
          weight_choice,
          weight_territory,
          boot,
          jack,
          subsample,
          n_rep,
          bias,
          size,
          confidence_level)`

- `mat` 
    is the matrix with vote counts, its 
    columns correspond to choices (parties, lists, candidates, etc)
    rows correspond to voter groups (constituencies, territories etc).

- `statistic`
    specifies which index should be computed (see table below).
    Defaults to `"PNS"`.

- `weight_choice` and `weight_territory` 
    (logical) indicate whether for indices where this is an option 
    choice and territory weight should be applied.
    Both default to `FALSE`.

- `boot`, `jack`, and `subsample` (logical)
    specify whether nonparametric bootstrap, jackknife, or subsampling
    (leave-*k* out jackknife) should be applied. 
    In each call of the function at most one of them can be set to `TRUE`.    
    All default to `FALSE`.

- `n_rep` 
    specifies the number of replicates if either boot or subsample are set to `TRUE`.
    Defaults to `10`.

- `bias` 
    indicates whether bias corrections for the resampling estimates should be applied.
    Defaults to `TRUE`.

- `size` 
    specifies the size of the sample if `subsample = TRUE`. 
    Defaults to half the number of the observed votes.

- `confidence_level`
    sets the level for the confidence intervals if a resampling procedure is applied.
    The level must be on [0, 1].
    Defaults to 0.95.


The function outputs an object of S3 class `"partynat"`, a list composed of

- `call` The matched call.
- `stat` The `statistics` argument in the call.
- `name` The name of the index.
- `total` The value of the index for the whole table.
    If resampling is applied, includes standard errors and confidence intervals.
- `choices` The values of the index for each choice.
    Equals to `NA` for indices that are not defined on the choice level.
    If resampling is applied, includes standard errors and confidence intervals.
- `confidence_level` The requested confidence levels.    

The S3 class `"partynat"` further has a `summary()` and a `plot()` method attached.


## Indices

|Index|Source|`partynat` call|
|-----|------|---------------|
|Index of Party Aggregation|Allik (2006)|`"IPA2"`|
|Weighted Party (System) Nationalization Score|Bochsler (2010)|`"PNSW"`|
|Standardized Party (System) Nationalization Score|Bochsler (2010)|`"PNS10"`|
|Territorial Coverage Index|Caramani (2004)|`"TCI"`|
|Index adjusted for Party size and number of Regions|Caramani (2004)|`"IPR2"`|
|Indicator of Party Aggregation|Chhibber and Kollman (1998)|`"IPA1"`|
|Inflation Score|Cox (1999)|`"IS"`|
|Variability Coefficient|Ersson, Janda, and Lane (1985)|`"VC"`|
|Standardized and Weighted Variability Coefficient|Ersson, Janda, and Lane (1985)|`"SWVC"`|
|Coefficient of Party Regionalization|Golosov (2016)|`"CPR"`|
|Index of Party (System) Nationalization|Golosov (2016)|`"IPN"`|
|Normalized Party (System) Nationalization Score|Golosov (2016)|`"NPNS"`|
|Normalized Coefficient of Variation|Golosov (2016)|`"NVC"`|
|Index of Party Regionalization|Golosov and Ponarin (1999)|`"IPR1"`|
|Party Nationalization Score|Jones and Mainwaring (2003)|`"PNS"`|
|Lee index|Lee (1988)|`"Lee"`|
|Inflation Index|Moenius and Kasuya (2004)|`"II"`|
|Index of variation/Mean Absolute Deviation|Rose and Urwin (1975)|`"MAD"`|
|Cumulative Regional Inequality|Rose and Urwin (1975)|`"CRI"`|
|Mean Standard Deviation of row shares| |`"MSD"`|
|Variance of row shares| |`"Var"`|
|Mutual Information|Frankel and Volij (2011)|`"MI"`|
|Dissimilarity index for choice-group independence|Medzihorsky (2022)|`"Delta"`|
