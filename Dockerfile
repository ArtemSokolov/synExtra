FROM rocker/tidyverse:4.0.1

ENV CRYPTOGRAPHY_DONT_BUILD_RUST=true
RUN R -e 'install.packages( "synapser", repos=c("http://ran.synapse.org", "http://cran.fhcrc.org") )'
RUN R -e 'devtools::install_github( "ArtemSokolov/synExtra" )'
