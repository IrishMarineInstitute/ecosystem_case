FROM rocker/r-ver:3.6.3

RUN apt-get update && apt-get install -y \
    sudo \
    gdebi-core \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    xtail \
    wget

RUN apt-get update \
  && apt-get install -y --no-install-recommends \
    lbzip2 \
    libfftw3-dev \
    libgdal-dev \
    libgeos-dev \
    libgsl0-dev \
    libgl1-mesa-dev \
    libglu1-mesa-dev \
    libhdf4-alt-dev \
    libhdf5-dev \
    libjq-dev \
    liblwgeom-dev \
    libpq-dev \
    libproj-dev \
    libprotobuf-dev \
    libnetcdf-dev \
    libsqlite3-dev \
    libssl-dev \
    libudunits2-dev \
    netcdf-bin \
    postgis \
    protobuf-compiler \
    sqlite3 \
    tk-dev \
    unixodbc-dev

RUN wget --no-verbose https://download3.rstudio.org/ubuntu-14.04/x86_64/VERSION -O "version.txt" && \
    VERSION=$(cat version.txt)  && \
    wget --no-verbose "https://download3.rstudio.org/ubuntu-14.04/x86_64/shiny-server-$VERSION-amd64.deb" -O ss-latest.deb && \
    gdebi -n ss-latest.deb && \
    rm -f version.txt ss-latest.deb && \
    . /etc/environment && \
    R -e "install.packages(c('shiny', 'rmarkdown'), repos='$MRAN')" && \
    cp -R /usr/local/lib/R/site-library/shiny/examples/* /srv/shiny-server/ && \
    chown shiny:shiny /var/lib/shiny-server
RUN Rscript -e "install.packages(c('shinydashboardPlus','shinycssloaders','shinythemes','shinyWidgets','knitr','rgdal','pander','kableExtra','leaflet','leaflet.extras','leaflet.extras2','mapedit','sf','readr','ncdf4','ggplot2','rasterVis','papeR','htmltools','lwgeom','shinyjs'), repos='https://cran.rstudio.com/')"   
RUN Rscript -e "install.packages(c('RColorBrewer','RandomFields','RNetCDF','classInt','deldir','gstat','hdf5r','lidR','mapdata','maptools','mapview','ncdf4','proj4','raster','rgdal','rgeos','rlas','sf','sp','spacetime','spatstat','spdep','geoR','geosphere'), repos='https://cran.rstudio.com/')"   

##RUN install2.r --error \
##    RColorBrewer \
##    RandomFields \
##    RNetCDF \
##    classInt \
##    deldir \
##    gstat \
##    hdf5r \
##    lidR \
##    mapdata \
##    maptools \
##     mapview \
##     ncdf4 \
##     proj4 \
##     raster \
##     rgdal \
##     rgeos \
##     rlas \
##     sf \
##     sp \
##     spacetime \
##     spatstat \
##     spdep \
##     geoR \
##     geosphere \
    ## from bioconductor
##     && R -e "BiocManager::install('rhdf5', update=FALSE, ask=FALSE)"

COPY www /srv/shiny-server/ecosystem/www
COPY Data /srv/shiny-server/ecosystem/Data
COPY functions /srv/shiny-server/ecosystem/functions
COPY results /srv/shiny-server/ecosystem/results
COPY temp /srv/shiny-server/ecosystem/temp
COPY Sources.html /srv/shiny-server/ecosystem/
COPY Sources.Rmd /srv/shiny-server/ecosystem/
COPY README.md /srv/shiny-server/ecosystem/
COPY app_toolbox.R /srv/shiny-server/ecosystem/
COPY ecosystem_case.Rproj /srv/shiny-server/ecosystem/
COPY report.Rmd /srv/shiny-server/ecosystem/
COPY pf.Rmd /srv/shiny-server/ecosystem/
COPY pf.html /srv/shiny-server/ecosystem/

EXPOSE 3838
#COPY shiny-server.sh /usr/bin/shiny-server.sh
CMD ["/usr/bin/shiny-server.sh"]
