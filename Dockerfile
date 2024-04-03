FROM openanalytics/r-ver:4.3.2

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
    #libssl1.1 \
    libxml2-dev \
    libpoppler-cpp-dev \
    && rm -rf /var/lib/apt/lists/*


RUN curl -LO https://dl.google.com/linux/direct/google-chrome-stable_current_amd64.deb && \
    apt-get update -qq && \
    apt-get -y install \
    ./google-chrome-stable_current_amd64.deb && \ 
    rm google-chrome-stable_current_amd64.deb

# basic shiny functionality
RUN R -q -e "install.packages(c('shiny', 'rmarkdown'))"

# req r packages
RUN R -q -e "install.packages(c('devtools', 'tidyverse', 'bslib', 'bsplus', 'shinyjs', 'jsonlite', 'listviewer', 'shinyWidgets', 'reactR', 'pagedown', 'shinybusy', 'promises', 'future'))"
#RUN R -q -e "devtools::install_github('timelyportfolio/reactR')"


# copy the app to the image
RUN mkdir /root/metaquest
RUN mkdir /root/metaquest/www
COPY www /root/metaquest/www
COPY app.R /root/metaquest
COPY utils.R /root/metaquest
COPY fields.R /root/metaquest
COPY metaquest_fields.json /root/metaquest

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/metaquest', host='0.0.0.0', port=3838)"]