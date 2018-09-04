# synExtra: Extensions for `synapser`

Sage Bionetworks develops and maintains [synapser](https://github.com/Sage-Bionetworks/synapser), an R client for interfacing with [Synapse](https://www.synapse.org/). This package builds on top of `synapser` to streamline its usage. The package can be installed directly from GitHub by running the following from R:

    if( !require(devtools) ) install.packages( "devtools" )
    devtools::install_github( "ArtemSokolov/synExtra" )

The two main features of `synExtra` streamline file downloading and Synapse querying.

## Downloading files

File downloading from Synapse is done primarily via `synapser::synGet()`. Since the follow-up action is often to load the downloaded file, a canonical `synGet()` call retrieves the local filename of the downloaded file:

``` R
fn <- synapser::synGet( "syn15663039", downloadLocation = "/data/myproject" )$path
X <- readr::read_csv( fn )
```

If multiple files are to be downloaded, the above code would be wrapped inside a `for` loop or a `purrr::map()`-family call:

``` R
## 1. Downloading multiple files using a for loop
fns <- c()
for( id in c("syn1710445", "syn1695376", "syn1710429") )
    fns[id] <- synapser::synGet( id, downloadLocation = "/data/myproject" )$path

## 2. Downloading multiple files using purrr::map() family of functions
fns <- purrr::map_chr( c("syn1710445", "syn1695376", "syn1710429"),
                       ~synapser::synGet( .x, downloadLocation = "/data/myproject" )$path )
```

An alternative solution provided by `synExtra` creates a downloader function that is linked to a specific local path. The downloader returns location of the downloaded file(s), thereby streamlining the process:

``` R
syn <- synExtra::synDownloader( "/data/myproject" )

# Pass Synapse IDs directly to the downloader
fns <- syn( "syn1710445", "syn1695376", "syn1710429" )

# Or store them in a vector and pass that vector instead
ids <- c( "syn1710445", "syn1695376", "syn1710429" )
fns <- syn( ids )

# In either case, fns is now a character vector of local paths
# [1] "/data/myproject/PANCAN12.IlluminaGA_miRNASeq.miRNA.tumor_whitelist"    
# [2] "/data/myproject/PANCAN12.IlluminaHiSeq_miRNASeq.miRNA.normal_whitelist"
# [3] "/data/myproject/PANCAN12.MDA_RPPA_Core.RPPA.tumor_whitelist"           
```

An important feature of the downloader is that it recognizes whether its arguments are valid synapse IDs. Any argument not recognized to be one is assumed to be a local filename already.

``` R
syn( "localFile.csv" )      ## returns "localFile.csv"
```

This allows for development of tools that abstract away the distinction of local files and files residing on Synapse. For example, consider a simple `csvpeek.R` script which displays the first 10 lines of a .csv file:

``` R
synapser::synLogin()
syn <- synExtra::synDownloader( "./.syn" )
fn <- syn( commandArgs( trailingOnly=TRUE )[1] )
readr::read_csv( fn, col_types=readr::cols(), n_max = 10 )
```

Such a script accepts local filenames as well as synapse IDs; in the latter case, the file is automatically downloaded to the local `.syn` directory. The distinction is completely transparent to the user who would use `csvpeek.R` as follows:

    Rscript csvpeek.R localFile.csv
    Rscript csvpeek.R syn15663039

## Querying Synapse

The second main feature of `synExtra` streamlines Synapse querying. It is essentially a wrapper around `synapser::synQuery()` that allows users to specify query constraints directly in the function call, preventing the need for string arithmetic beforehand. The function `synExtra::synq()` accepts an arbitrary number of arguments. Any unnamed argument is treated as a field to "select", while any named argument is treated as a "where" constraint. For example,

``` R
synExtra::synq( "id", "name", "projectId", parentId = "syn15673834", type = "dataset" )
## select id,name,projectId from entity where "parentId"=="syn15673834" and "type"=="dataset"

## synq() understands querying by non-character values (e.g., "Field2" == 123)
synExtra::synq( "id", "Field1", projectId="syn12180284", Field2=123 )
## select id,Field1 from entity where "projectId"=="syn12180284" and "Field2"==123
```

An important feature of `synq()` is that it will always return a data frame. Non-existent fields will contain `NULL`.

``` R
synExtra::synq( "id", "name", "BadField", parentId = "syn1901044" )
# select id,name,BadField from entity where "parentId"=="syn1901044" 
# # A tibble: 2 x 3
#   name                                  id         BadField
#   <chr>                                 <chr>      <list>  
# 1 ESTIMATE_scores_PANCAN11.tsv          syn1896429 <NULL>  
# 2 ESTIMATE_scores_PANCAN11_RNASeqV2.tsv syn1901530 <NULL>
```

while queries that return no results will produce data frames with zero rows, where the columns still correspond to the requested fields.

``` R
synExtra::synq( "id", "name", parentId = "syn1901044", SomeField="BadQuery" )
# select id,name from entity where "parentId"=="syn1901044" and "SomeField"=="BadQuery" 
# # A tibble: 0 x 2
# # ... with 2 variables: id <chr>, name <chr>
```
