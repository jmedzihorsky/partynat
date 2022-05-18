# partynat

`partynat` is an `R` package for computing indices used in the context of party nationalization research.
It provides more than a dozen different indices of static party nationalization,
including standard errors and confidence intervals 
via nonparametric bootstrap, subsampling and jackknife
with optional bias correction.


It's a companion package to
[Medzihorsky, Juraj. (2022). Unifying the Measurement of Variation in Electoral Support](https://github.com/jmedzihorsky/partynat/blob/master/PartyNat-WP.pdf)

The manual is [here](https://github.com/jmedzihorsky/partynat/blob/master/partynat-manual.pdf).

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
- `stat` The `statistics` argument in the call. See **Indices** below.
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
|Weighted Party (System) Nationalization Score|[Bochsler (2010)](https://doi.org/10.1016/j.electstud.2009.06.003)|`"PNSW"`|
|Standardized Party (System) Nationalization Score|[Bochsler (2010)](https://doi.org/10.1016/j.electstud.2009.06.003)|`"PNS10"`|
|Territorial Coverage Index|[Caramani (2004)](http://hdl.handle.net/1814/22507)|`"TCI"`|
|Index adjusted for Party size and number of Regions|[Caramani (2004)](http://hdl.handle.net/1814/22507)|`"IPR2"`|
|Indicator of Party Aggregation|[Chhibber and Kollman (1998)](https://doi.org/10.2307/2585667)|`"IPA1"`|
|Inflation Score|[Cox (1999)](https://doi.org/10.1146/annurev.polisci.2.1.145)|`"IS"`|
|Variability Coefficient|[Ersson, Janda, and Lane (1985)](https://doi.org/10.1177%2F0010414085018002002)|`"VC"`|
|Standardized and Weighted Variability Coefficient|[Ersson, Janda, and Lane (1985)](https://doi.org/10.1177%2F0010414085018002002)|`"SWVC"`|
|Coefficient of Party Regionalization|[Golosov (2016)](https://doi.org/10.1177%2F1354068814549342)|`"CPR"`|
|Index of Party (System) Nationalization|[Golosov (2016)](https://doi.org/10.1177%2F1354068814549342)|`"IPN"`|
|Normalized Party (System) Nationalization Score|[Golosov (2016)](https://doi.org/10.1177%2F1354068814549342)|`"NPNS"`|
|Normalized Coefficient of Variation|[Golosov (2016)](https://doi.org/10.1177%2F1354068814549342)|`"NVC"`|
|Index of Party Regionalization|[Golosov and Ponarin (1999)](https://cadmus.eui.eu/bitstream/handle/1814/1630/1999_EUI%20WP_RSCAS_025.pdf?sequence=1)|`"IPR1"`|
|Party Nationalization Score|[Jones and Mainwaring (2003)](https://doi.org/10.1177%2F13540688030092002)|`"PNS"`|
|Lee index|Lee (1988)|`"Lee"`|
|Inflation Index|[Moenius and Kasuya (2004)](https://doi.org/10.1177%2F1354068804045387)|`"II"`|
|Index of variation/Mean Absolute Deviation|Rose and Urwin (1975)|`"MAD"`|
|Cumulative Regional Inequality|Rose and Urwin (1975)|`"CRI"`|
|Mean Standard Deviation of row shares| |`"MSD"`|
|Variance of row shares| |`"Var"`|
|Mutual Information|[Frankel and Volij (2011)](https://doi.org/10.1016/j.jet.2010.10.008)|`"MI"`|
|Dissimilarity index for choice-group independence|[Medzihorsky (2022)](https://github.com/jmedzihorsky/partynat/blob/master/PartyNat-WP.pdf)|`"Delta"`|


##  Usage

The required data format is a matrix with a cross-tabulation of the vote counts by
territory (rows) and party (columns). 
The counts can be either the official election results, or estimates from a survey.
The `partynat()` function requires integer counts,
and implements resampling procedures for survey data.
If for some reason you do not have the counts and have instead the constituency percentages,
you can simply multiply those by the constituency sizes and round them. 
Alternatively, you can apportion the votes with
[`seatdist::giveseats()`](https://github.com/jmedzihorsky/seatdist),
to make sure the counts add up to the desired margins.


##  Background

What political scientists mean under _party nationalization_ is that a political party's
presence is the same in all territories (such as constituencies or federal states)
that comprise a nation. By presence they usually mean electoral support,
and by it being the same everywhere they mean at least three different things 
(see [Caramani (1996)](https://doi.org/10.1080/01402389608425131)):
1. That the party receives the same vote share everywhere. This is called _static nationalization_.
2. That the party's electoral support changes the same way everywhere. This known as _dynamic nationalization_.
3. That the effect of something, say a campaign, on the party's support is the same everywhere.
<!--This package deals with the first.-->
<!--The starting point is a cross-tab that tabulates the votes by party (columns) and territory (rows).-->

While party scholars agree on what perfect static nationalization should look 
like --- within-territory party shares are identical to their national shares ---
they don't agree on how to measure deviations from this baseline.
This shouldn't come as a surprise if we rethink the problem as one of measuring association.
From that perspective, static nationalization is party-territory independence.
And there is an infinite number of measures of association for categorical variables.

Party scholars have invested considerable effort in developing indices of static nationalization
that have the propeties they desire of such indices.
Perhaps the most sophisticated effort to date is an index based on the Gini coefficient of inequality,
proposed by [Bochsler (2010)](https://doi.org/10.1016/j.electstud.2009.06.003).
This index, called `"PNSW"` here, has a host of appealing formal properties.
However, it's numeric values lack clear substantive interpretation.

As an alternative that has an easy substantive interpretation, 
my [working paper](https://github.com/jmedzihorsky/partynat/blob/master/PartyNat-WP.pdf) proposes an index
that gives the smallest fraction of the votes that would need to change
for static nationalization to happen.
The cost of this easy substantive interpretation is that unlike
the PNSW, this index is only sensitive to transfers under the weak version of Dalton's principle.
While this rids it of some formal elegance, 
the working paper shows that it nevertheless still correlates 0.99 with the PNSW in over a thousand elections 
from over two centuries.
The index, called `"Delta"` here,
is a special cases of Gini's dissimilarity index,
just like 
[Pedersen's (1979) electoral volatility index](https://doi.org/10.1111/j.1475-6765.1979.tb01267.x).

The dissimilarity index also has a long track record in the study of segregation
(see [Duncan and Duncan (1955)](https://doi.org/10.2307/2088328)).
From this perspective, we can look on party _de_-nationalization as a case of voter segregation
that can also happen along gender, ethnic, occupational etc. lines.
Another successful index of segregation is _mutual information_.
[Frankel and Volij (2011)](https://doi.org/10.1016/j.jet.2010.10.008) and
[Mora and Ruiz-Castillo (2011)](https://journals.sagepub.com/doi/abs/10.1111/j.1467-9531.2011.01237.x)
show that it has a host of appealing properties, including strong decomposability.
However, its substantive interpretation is a bit less direct than that of the dissimilarity index.

The working paper also discusses the measurement of _dynamic nationalization_, and _electoral continuity_ 
with the dissimilarity index and log-linear models, but these are not currently implemented in `partynat`.
