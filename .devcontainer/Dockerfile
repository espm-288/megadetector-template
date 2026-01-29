ARG BASE_IMAGE=ghcr.io/rocker-org/ml:latest
FROM ${BASE_IMAGE}

# Install system dependencies required by OpenCV
# libgl1 is the vendor-neutral GL runtime (provides libGL.so.1) needed by opencv-python
# libglib2.0-0 is typically needed as well
USER root
RUN apt-get update && apt-get install -y \
    libgl1 \
    libglib2.0-0 \
    && rm -rf /var/lib/apt/lists/*

USER ${NB_USER}

# Pre-install ALL megadetector dependencies in the base conda environment
# rocker/ml already has torch, torchvision, numpy, pandas, pillow, scikit-learn, opencv
# We add the remaining dependencies that megadetector needs
RUN mamba install -y -c conda-forge \
    humanfriendly \
    jsonpickle \
    python-dateutil \
    send2trash \
    pytorch \
    torchvision \
    libmamba \
    && mamba clean -afy

# Configure reticulate to use the base conda environment
# This prevents creating a new environment and avoids duplicate torch installations
ENV RETICULATE_PYTHON=/opt/conda/bin/python
ENV RETICULATE_PYTHON_ENV=/opt/conda
ENV LD_LIBRARY_PATH=/opt/conda/lib:$LD_LIBRARY_PATH


# Install remaining pip-only dependencies with --no-deps to use conda packages
# ultralytics-yolov5 and yolov9pip must come from pip, but we prevent them from
# reinstalling torch/torchvision that already exist in conda
RUN /opt/conda/bin/pip install --no-cache-dir \
    ultralytics-yolov5==0.1.1 \
    yolov9pip==0.0.4 \
    fastquadtree \
    clipboard \
    dill \
    ruff \
    pytest

# Now install megadetector itself with --no-deps since all dependencies are satisfied
RUN /opt/conda/bin/pip install --no-cache-dir --no-deps megadetector

# Pre-download megadetector model
USER root
RUN mkdir -p /opt/megadetector/models && \
    wget -q https://github.com/agentmorris/MegaDetector/releases/download/v5.0/md_v5a.0.0.pt -O /opt/megadetector/models/mdv5a.pt && \
    chmod 644 /opt/megadetector/models/mdv5a.pt

USER ${NB_USER}

# Finally install the R package from local source (to verify changes)
COPY . /src
RUN R -e 'remotes::install_local("/src", upgrade="never")'

# Smoketest: run megadetector from R
RUN R -e 'source(system.file("examples/demo.R", package="megadetector"))'
