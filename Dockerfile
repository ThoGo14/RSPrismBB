FROM rocker/tidyverse:4.3.3

WORKDIR /app

# Additional system libraries for common R package compilation/runtime needs.
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libfribidi-dev \
    libharfbuzz-dev \
    libicu-dev \
    libgit2-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    && rm -rf /var/lib/apt/lists/*

COPY . /app

RUN R -q -e "install.packages(c('remotes','golem'), repos='https://cloud.r-project.org')"
RUN R -q -e "remotes::install_deps('/app', dependencies = TRUE, upgrade = 'never')"
RUN R -q -e "remotes::install_local('/app', upgrade = 'never')"

EXPOSE 3838

CMD ["R", "-q", "-e", "options('golem.app.prod' = TRUE); app <- RSPrismBB::run_app(options = list(launch.browser = FALSE)); shiny::runApp(app, host = '0.0.0.0', port = 3838)"]
