# synExtra: Extensions for `synapser`

Sage Bionetworks develops and maintains [synapser](https://github.com/Sage-Bionetworks/synapser), an R client for interfacing with [Synapse](https://www.synapse.org/). This package builds on top of `synapser` to streamline its usage in a functional programming (FP) paradigm. The package can be installed directly from GitHub by running the following from R:

    if( !require(devtools) ) install.packages( "devtools" )
    devtools::install_github( "ArtemSokolov/synExtra" )

The functionality can be loosely grouped into three categories: 1) downloading and uploading of files, 2) traversal of the Synapse directory structure, and 3) miscellaneous inspection of Synapse entities

## Downloading and uploading of files

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
    
### File uploading

Similarly, file uploading to Synapse is done primarily via `synapser::synStore()`. However, uploading a large number of files in a complicated directory structure can become tedious. `synExtra` provides a wrapper `synStoreMany()` that functions effectively like the Linux `mv` command. The function takes two arguments: a local path and the synapse ID of the Project/Folder where the files are to be uploaded to.

``` R
synExtra::synStoreMany( "/home/sokolov/devel/synExtra", "syn12180284" )

## The function understands local paths
synExtra::synStoreMany( ".", "syn12180284" )

## As well as relative paths
synExtra::synStoreMany( "../myOtherFolder", "syn12180284" )
```

