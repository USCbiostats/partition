
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
#> 1 reduced variables created from 4 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block2_x1, block2_x2, block2_x3, block2_x4}
#> 
#> Minimum information:
#> 0.615

# return reduced data
partition_scores(prt)
#> # A tibble: 100 x 9
#>    block1_x1 block1_x2 block1_x3 block3_x1 block3_x2 block3_x3 block3_x4
#>        <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>     <dbl>
#>  1     0.750     0.815    1.22    0.593        0.460   0.760      -0.227
#>  2    -0.804    -1.58    -0.678   0.554       -0.554  -1.14       -1.39 
#>  3     0.125    -0.745   -0.0912  1.83         0.602  -0.0697     -0.139
#>  4     1.21      1.54     0.455   0.957        0.890   1.18        0.493
#>  5    -0.847    -0.988   -0.326  -0.176       -0.717  -0.00146     0.516
#>  6    -0.543    -0.622    1.81   -2.60        -1.70   -1.65       -1.54 
#>  7    -0.427     0.404    0.177  -0.977       -0.581   0.0943      0.185
#>  8     0.160     1.65     0.204  -0.000595     0.626   0.0714      2.15 
#>  9    -1.73     -0.915    0.151   1.42        -0.144   0.0839     -0.138
#> 10    -0.391     0.659    1.26   -1.33        -0.464  -1.33       -0.982
#> # … with 90 more rows, and 2 more variables: block3_x5 <dbl>,
#> #   reduced_var_1 <dbl>

# access mapping keys
mapping_key(prt)
#> # A tibble: 9 x 4
#>   variable      mapping   information indices  
#>   <chr>         <list>          <dbl> <list>   
#> 1 block1_x1     <chr [1]>       1     <int [1]>
#> 2 block1_x2     <chr [1]>       1     <int [1]>
#> 3 block1_x3     <chr [1]>       1     <int [1]>
#> 4 block3_x1     <chr [1]>       1     <int [1]>
#> 5 block3_x2     <chr [1]>       1     <int [1]>
#> 6 block3_x3     <chr [1]>       1     <int [1]>
#> 7 block3_x4     <chr [1]>       1     <int [1]>
#> 8 block3_x5     <chr [1]>       1     <int [1]>
#> 9 reduced_var_1 <chr [4]>       0.615 <int [4]>

unnest_mappings(prt)
#> # A tibble: 12 x 4
#>    variable      information mapping   indices
#>    <chr>               <dbl> <chr>       <int>
#>  1 block1_x1           1     block1_x1       1
#>  2 block1_x2           1     block1_x2       2
#>  3 block1_x3           1     block1_x3       3
#>  4 block3_x1           1     block3_x1       8
#>  5 block3_x2           1     block3_x2       9
#>  6 block3_x3           1     block3_x3      10
#>  7 block3_x4           1     block3_x4      11
#>  8 block3_x5           1     block3_x5      12
#>  9 reduced_var_1       0.615 block2_x1       4
#> 10 reduced_var_1       0.615 block2_x2       5
#> 11 reduced_var_1       0.615 block2_x3       6
#> 12 reduced_var_1       0.615 block2_x4       7

# use a lower threshold of information loss
partition(df, threshold = .5, partitioner = part_kmeans())
#> Partitioner:
#>    Director: K-Means Clusters 
#>    Metric: Minimum Intraclass Correlation 
#>    Reducer: Scaled Mean
#> 
#> Number of Reduced Variables:
#> 4 reduced variables created from 10 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block3_x1, block3_x5}
#> reduced_var_2 = {block2_x1, block2_x2, block2_x3, block2_x4}
#> reduced_var_3 = {block3_x2, block3_x4}
#> reduced_var_4 = {block1_x2, block1_x3}
#> 
#> Minimum information:
#> 0.517

# use a custom partitioner
part_icc_rowmeans <- replace_partitioner(
  part_icc, 
  reduce = as_reducer(rowMeans)
)
partition(df, threshold = .6, partitioner = part_icc_rowmeans)
#> Partitioner:
#>    Director: Minimum Distance (Pearson) 
#>    Metric: Intraclass Correlation 
#>    Reducer: [90m<custom reducer>[39m
#> 
#> Number of Reduced Variables:
#> 1 reduced variables created from 4 observed variables
#> 
#> Mappings:
#> reduced_var_1 = {block2_x1, block2_x2, block2_x3, block2_x4}
#> 
#> Minimum information:
#> 0.615
```
