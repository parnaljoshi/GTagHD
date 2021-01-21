FROM openanalytics/r-base

MAINTAINER FriedbergLab "genesculptsuitehelp@gmail.com"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    #libssl1.0.0 \
    libxml2-dev

# install package dependencies for GTagHD
#RUN R -e "install.packages(c('shiny', 'shinyjs', 'stringr', 'plyr', 'XML', 'rentrez', 'rlist', 'devtools', 'httpuv', 'httr'), repos='https://cloud.r-project.org/')"
RUN R -e "install.packages(c('shiny', 'shinyjs', 'stringr', 'plyr', 'XML', 'rentrez', 'rlist', 'curl', 'httr', 'jsonlite', 'xml2'), repos='https://cloud.r-project.org/')"

# Copy GTagHD to image
RUN mkdir /root/gtaghd/
COPY / /root/gtaghd

COPY Rprofile.site /usr/lib/R/etc/

#Expose this port
EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/gtaghd/', port = 3838)"]
