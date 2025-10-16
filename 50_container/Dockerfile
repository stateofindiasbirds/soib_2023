# Dockerfile for container. Build with podman
# Reduced size by building over alpine
#
# Build with : podman build -t soib .
#
# podman save -o soib.tar localhost/soib:latest
#
# Image size:
# - 450 MB without data (R+deps+base linux)
# - 840 MB with data
#

#FROM alpine:latest actually resolves to ?
#Next step would be to tie down package versions
FROM alpine:latest

ARG R_VERSION
ENV R_VERSION ${R_VERSION:-4.5.0}
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV CRAN https://cran.r-project.org
ENV R_DAILY_URL https://stat.ethz.ch/R/daily

# Dependent Packages for all the R packages, plus the R packages themselves as
# one single layer
# NOTE: gdal-dev is added later again as sf has some shared lib dependency on this
# FIXME: Why is tidyverse coming from cloud-r project rather than cran project ?
RUN apk update && \
    apk add R cmake R-dev linux-headers g++ && \
    apk add udunits udunits-dev gdal-dev proj proj-dev && \
    apk add geos geos-dev && \
    apk add tzdata && \
    apk add libxml2-dev && \
    apk add fontconfig-dev && \
    apk add harfbuzz-dev && \
    apk add fribidi-dev && \
    apk add openssl-dev && \
    apk add curl-dev && \
    apk add libsecret-dev

# Install the awesome "pak" first!
ADD 50_container/install_pak.R /
RUN Rscript install_pak.R

ADD 50_container/install_packages_or_die.R /
#RUN Rscript install_packages_or_die.R lubdridate
# Somehow lubridate works better if first ?
RUN Rscript -e 'install.packages("lubridate", repos = "http://cloud.r-project.org")'
RUN Rscript install_packages_or_die.R tictoc
RUN Rscript install_packages_or_die.R dplyr
RUN Rscript install_packages_or_die.R peakRAM
RUN Rscript install_packages_or_die.R lme4
RUN Rscript install_packages_or_die.R arm
RUN Rscript install_packages_or_die.R VGAM
RUN Rscript install_packages_or_die.R merTools
RUN Rscript install_packages_or_die.R unmarked
RUN Rscript install_packages_or_die.R reshape2
RUN Rscript install_packages_or_die.R sf
RUN Rscript install_packages_or_die.R data.table
RUN Rscript install_packages_or_die.R glue
RUN Rscript install_packages_or_die.R tidyverse

RUN apk del *-dev linux-headers g++ cmake && \
    apk add gdal-dev && \
    rm -rf /var/cache/apk/*

# R build files
RUN rm -rf /tmp/*

# NOTE: keeping data inside the container is a policy thing,
# so now moved out

# Our R scripts
COPY p3s1a.R /app/p3s1a.R
COPY 00_scripts/run_species_trends_container.R /app/00_scripts/
COPY 00_scripts/00_functions.R /app/00_scripts/
COPY 50_container/VERSION /app/VERSION

# Set working directory
WORKDIR /app

ENV OMP_NUM_THREADS=1
# Command to run application
ENTRYPOINT ["Rscript", "p3s1a.R" ]
