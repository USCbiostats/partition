
<!-- README.md is generated from README.Rmd. Please edit that file -->

[![Travis build
status](https://travis-ci.org/malcolmbarrett/partition2.svg?branch=master)](https://travis-ci.org/malcolmbarrett/partition2)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/malcolmbarrett/partition2?branch=master&svg=true)](https://ci.appveyor.com/project/malcolmbarrett/partition2)
[![Coverage
status](https://codecov.io/gh/malcolmbarrett/partition2/branch/master/graph/badge.svg)](https://codecov.io/github/malcolmbarrett/partition2?branch=master)

# partition2

partition2 is a fast and flexible framework for agglomerative
partitioning. partition uses an approach called direct, measure, reduce
to create new variables that maintain the user-specified minimum level
of information. Each reduced variable is also interpretable: the
original variables map to one and only one variable in the reduced data
set. partition is flexible, as well: how variables are selected to
reduce, how information loss is measured, and the way data is reduced
can all be customized.

## Installation

You can install the development version of partition2 GitHub with:

``` r
# install.packages("remotes)
remotes::install_github("malcolmbarrett/partition2")
```

## Example

``` r
library(partition2)
df <- simulate_block_data(c(3, 4, 5), lower_corr = .4, upper_corr = .6, n = 100)

#  don't accept reductions where information < .6
prt <- partition(df, threshold = .6)
prt
#> Partitioner:
#>    Director: Minimum Distance (Pearson) 
#>    Metric: Intraclass Correlation 
#>    Reducer: Scaled Mean
#> 
#> Number of Reduced Variables:
#> 3 reduced variables created from 7 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x3, block3_x4, block3_x5}
#> reduced_var_2 = {block2_x1, block2_x4}
#> reduced_var_3 = {block3_x1, block3_x2}
#> 
#> Minimum information:
#> 0.601

# return reduced data
partition_scores(prt)
#> # A tibble: 100 x 8
#>    block1_x1 block1_x2 block1_x3 block2_x2 block2_x3 reduced_var_1
#>        <dbl>     <dbl>     <dbl>     <dbl>     <dbl>         <dbl>
#>  1    0.826      0.458     0.462   -1.20      0.408        -0.250 
#>  2   -0.903     -0.214    -1.27    -0.773    -0.829         0.608 
#>  3    0.0922    -0.285     0.893   -0.782     0.302        -0.725 
#>  4    0.446     -0.462     1.26    -1.63     -0.798         0.0306
#>  5    0.967     -0.868    -1.34     0.493     1.21          0.177 
#>  6   -0.607     -0.229     0.397    2.41      0.816        -0.683 
#>  7    0.640     -0.127     1.39    -0.320    -1.51         -0.568 
#>  8   -1.24      -0.271    -1.45     0.307     0.0241        1.43  
#>  9    0.125     -0.308     0.535   -0.0230   -0.502         2.18  
#> 10   -2.67      -1.57     -2.18    -0.526    -0.990        -0.495 
#> # … with 90 more rows, and 2 more variables: reduced_var_2 <dbl>,
#> #   reduced_var_3 <dbl>

# access mapping keys
mapping_key(prt)
#> # A tibble: 8 x 4
#>   variable      mapping   information indices  
#>   <chr>         <list>          <dbl> <list>   
#> 1 block1_x1     <chr [1]>       1     <int [1]>
#> 2 block1_x2     <chr [1]>       1     <int [1]>
#> 3 block1_x3     <chr [1]>       1     <int [1]>
#> 4 block2_x2     <chr [1]>       1     <int [1]>
#> 5 block2_x3     <chr [1]>       1     <int [1]>
#> 6 reduced_var_1 <chr [3]>       0.611 <int [3]>
#> 7 reduced_var_2 <chr [2]>       0.612 <int [2]>
#> 8 reduced_var_3 <chr [2]>       0.601 <int [2]>

unnest_mappings(prt)
#> # A tibble: 12 x 4
#>    variable      information mapping   indices
#>    <chr>               <dbl> <chr>       <int>
#>  1 block1_x1           1     block1_x1       1
#>  2 block1_x2           1     block1_x2       2
#>  3 block1_x3           1     block1_x3       3
#>  4 block2_x2           1     block2_x2       5
#>  5 block2_x3           1     block2_x3       6
#>  6 reduced_var_1       0.611 block3_x3      10
#>  7 reduced_var_1       0.611 block3_x4      11
#>  8 reduced_var_1       0.611 block3_x5      12
#>  9 reduced_var_2       0.612 block2_x1       4
#> 10 reduced_var_2       0.612 block2_x4       7
#> 11 reduced_var_3       0.601 block3_x1       8
#> 12 reduced_var_3       0.601 block3_x2       9

# use a lower threshold of information loss
partition(df, threshold = .5, partitioner = part_kmeans())
#> Partitioner:
#>    Director: K-Means Clusters 
#>    Metric: Minimum Intraclass Correlation 
#>    Reducer: Scaled Mean
#> 
#> Number of Reduced Variables:
#> 3 reduced variables created from 9 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x1, block3_x2, block3_x3}
#> reduced_var_2 = {block2_x1, block2_x2, block2_x3, block2_x4}
#> reduced_var_3 = {block1_x1, block1_x3}
#> 
#> Minimum information:
#> 0.524

# use a custom partitioner
part_icc_rowmeans <- replace_partitioner(
  part_icc, 
  reduce = as_reducer(rowMeans)
)
partition(df, threshold = .6, partitioner = part_icc_rowmeans) 
#> Partitioner:
#>    Director: Minimum Distance (Pearson) 
#>    Metric: Intraclass Correlation 
#>    Reducer: <custom reducer>
#> 
#> Number of Reduced Variables:
#> 3 reduced variables created from 7 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x3, block3_x4, block3_x5}
#> reduced_var_2 = {block2_x1, block2_x4}
#> reduced_var_3 = {block3_x1, block3_x2}
#> 
#> Minimum information:
#> 0.601
```
