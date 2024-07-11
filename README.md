## Installation


To install, run the following:

```r
  remotes::install_github( "jae0/aegis")  # helper functions
  remotes::install_github( "jae0/aegis.temperature")
``` 

You probably will want to have an Rprofile set up properly such as:

```r
homedir = path.expand("~")
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

require( aegis )
require( aegis.temperature )

```

Data assimilation, thinning and organisation occurs in [inst/scripts/01_temperature_data.md](inst/scripts/01_temperature_data.md).

Data modelling to address spatiotemporal aliasing occurs in [inst/scripts/03_temperature_carstm.md](inst/scripts/03_temperature_carstm.md).
