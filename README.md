
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
df <- simulate_block_data(c(3, 4, 5), .4, .6, 100)

#  don't accept reductions where information < .6
prt <- partition(df, threshold = .6)
prt
#> Partitioner:
#>    Director: Minimum Distance (Pearson) 
#>    Metric: Intraclass Correlation 
#>    Reducer: Scaled Mean
#> 
#> Number of Reduced Variables:
#> 1 reduced variables created from 5 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x1, block3_x2, block3_x3, block3_x4, block3_x5}
#> 
#> Minimum information:
#> 0.621

# return reduced data
partition_scores(prt)
#> # A tibble: 100 x 8
#>    block1_x1 block1_x2 block1_x3 block2_x1 block2_x2 block2_x3 block2_x4
#>        <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1    0.427   -0.00924     0.744     0.895    0.815     1.90      0.690 
#>  2   -1.05    -1.66       -0.848     0.223    1.55      1.27      1.77  
#>  3   -0.435   -0.175       0.448     0.321    0.762    -0.206    -0.429 
#>  4    0.0839   0.549       1.59     -0.115    1.79     -0.670     1.31  
#>  5    1.69     1.25        1.79     -1.09    -0.0842    1.32      0.492 
#>  6   -0.250   -1.37       -1.24     -0.734   -0.987    -0.0534   -0.747 
#>  7    0.297    0.545       0.733     1.59     1.70      0.895     0.242 
#>  8   -0.590   -1.42       -1.01      0.175   -0.869     1.11     -0.0859
#>  9   -0.105   -0.902       0.336    -0.418   -0.288    -0.865    -1.31  
#> 10    0.891   -0.713      -1.21     -0.116    0.333    -1.20     -0.630 
#> # … with 90 more rows, and 1 more variable: reduced_var_1 <dbl>

# access mapping keys
mapping_key(prt)
#> # A tibble: 8 x 4
#>   variable      mapping   information indices  
#>   <chr>         <list>          <dbl> <list>   
#> 1 block1_x1     <chr [1]>       1     <int [1]>
#> 2 block1_x2     <chr [1]>       1     <int [1]>
#> 3 block1_x3     <chr [1]>       1     <int [1]>
#> 4 block2_x1     <chr [1]>       1     <int [1]>
#> 5 block2_x2     <chr [1]>       1     <int [1]>
#> 6 block2_x3     <chr [1]>       1     <int [1]>
#> 7 block2_x4     <chr [1]>       1     <int [1]>
#> 8 reduced_var_1 <chr [5]>       0.621 <int [5]>

unnest_mappings(prt)
#> # A tibble: 12 x 4
#>    variable      information mapping   indices
#>    <chr>               <dbl> <chr>       <int>
#>  1 block1_x1           1     block1_x1       1
#>  2 block1_x2           1     block1_x2       2
#>  3 block1_x3           1     block1_x3       3
#>  4 block2_x1           1     block2_x1       4
#>  5 block2_x2           1     block2_x2       5
#>  6 block2_x3           1     block2_x3       6
#>  7 block2_x4           1     block2_x4       7
#>  8 reduced_var_1       0.621 block3_x1       8
#>  9 reduced_var_1       0.621 block3_x2       9
#> 10 reduced_var_1       0.621 block3_x3      10
#> 11 reduced_var_1       0.621 block3_x4      11
#> 12 reduced_var_1       0.621 block3_x5      12

# use a lower threshold of information loss
partition(df, threshold = .5, partitioner = part_kmeans())
#> Partitioner:
#>    Director: K-Means Clusters 
#>    Metric: Minimum Intraclass Correlation 
#>    Reducer: Scaled Mean
#> 
#> Number of Reduced Variables:
#> 1 reduced variables created from 2 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block2_x2, block2_x4}
#> 
#> Minimum information:
#> 0.577

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
#> 1 reduced variables created from 5 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x1, block3_x2, block3_x3, block3_x4, block3_x5}
#> 
#> Minimum information:
#> 0.621
```
