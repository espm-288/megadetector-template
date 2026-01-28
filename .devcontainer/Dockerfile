FROM ghcr.io/rocker-org/ml:latest

# NB_USER corresponds to the default user in the rocker images (usually rstudio)
ARG NB_USER=ubuntu

# Install system dependencies required by OpenCV
# libgl1 is the vendor-neutral GL runtime (provides libGL.so.1) needed by opencv-python
# libglib2.0-0 is typically needed as well
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_USER}

RUN R -e 'remotes::install_github("boettiger-lab/megadetector"); library(megadetector); install_megadetector()'

