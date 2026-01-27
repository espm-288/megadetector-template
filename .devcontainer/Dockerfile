FROM ghcr.io/rocker-org/ml:latest

# Install system dependencies required by OpenCV and other tools
# libgl1-mesa-dev is needed for opencv-python if not using the headless version
# python3-venv is helpful for some pip/venv operations
USER root
RUN apt-get update && apt-get install -y \
    libgl1-mesa-dev \
    libglib2.0-0 \
    python3-venv \
    && rm -rf /var/lib/apt/lists/*

USER rstudio
