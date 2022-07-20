FROM openanalytics/r-ver:4.2.1

LABEL maintainer="John Clark <jclark@xylemgeo.com>"

# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.1 \
    libxml2-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
# RUN apt-get update && apt-get install -y \
#     libmpfr-dev \
#     && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# install dependencies of the euler app
RUN R -q -e "install.packages(c('devtools', 'tidyverse', 'bslib', 'bsplus', 'shinyjs', 'jsonlite', 'listviewer', 'shinyWidgets'))"
RUN R -q -e "devtools::install_github('timelyportfolio/reactR')"


# copy the app to the image
RUN mkdir /root/metaquest
RUN mkdir /root/metaquest/www
COPY www /root/metaquest/www
COPY app.R /root/metaquest
COPY mods.R /root/metaquest
COPY utils.R /root/metaquest
COPY quests.R /root/metaquest
COPY fields.R /root/metaquest
COPY metaquest_fields.json /root/metaquest

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

# works local but not in shinyproxy docker
CMD ["R", "-e", "shiny::runApp('/root/metaquest', host='0.0.0.0', port=3838)"]
# does not work locally or in shinyproxy docker
# CMD ["R", "-e", "shiny::runApp('/root/metaquest')"]
# CMD ["R", "-q", "-e", "shiny::runApp('/root/metaquest')"]