FROM nvidia/cuda:12.4.1-devel-ubuntu22.04

ENV DEBIAN_FRONTEND=noninteractive

# First add NVIDIA package repository
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        curl && \
    curl -fsSL https://nvidia.github.io/libnvidia-container/gpgkey | gpg --dearmor -o /usr/share/keyrings/nvidia-container-toolkit-keyring.gpg && \
    curl -s -L https://nvidia.github.io/libnvidia-container/stable/deb/nvidia-container-toolkit.list | \
        sed 's#deb https://#deb [signed-by=/usr/share/keyrings/nvidia-container-toolkit-keyring.gpg] https://#g' | \
        tee /etc/apt/sources.list.d/nvidia-container-toolkit.list && \
    apt-get update

# Install RStudio Server dependencies, NVIDIA driver components, and CUDA toolkit
RUN apt-get update -y && \
    apt-get install -y --no-install-recommends \
        gdebi-core \
        psmisc \
        libharfbuzz-dev \
        libfribidi-dev \
        collectl \
        libclang-dev \
        python3 \
        python3-pip \
        python3-dev \
        sudo \
        r-base \
        r-base-dev \
        libssl-dev \
        libcurl4-openssl-dev \
        libxml2-dev \
        pandoc \
        pandoc-citeproc \
        lsb-release \
        wget \
        libfontconfig1-dev \
        libfreetype6-dev \
        libpng-dev \
        libtiff5-dev \
        libjpeg-dev \
        nvidia-utils-535 \
        libnvidia-ml-dev \
        libnvidia-compute-535 \
        cuda-toolkit-12-4 && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/*

# Set CUDA and NVIDIA paths
ENV CUDA_HOME=/usr/local/cuda
ENV PATH="${CUDA_HOME}/bin:${PATH}"
ENV LD_LIBRARY_PATH="/usr/lib/x86_64-linux-gnu:${CUDA_HOME}/lib64:/usr/local/nvidia/lib:/usr/local/nvidia/lib64"

# Verify nvcc and driver components
RUN nvcc --version && \
    echo "NVIDIA components:" && \
    ls -l /usr/lib/x86_64-linux-gnu/libnvidia-ml* && \
    ls -l /usr/bin/nvidia-smi && \
    nvidia-smi

# Install RStudio Server
RUN wget https://download2.rstudio.org/server/jammy/amd64/rstudio-server-2024.09.1-394-amd64.deb && \
    gdebi -n rstudio-server-2024.09.1-394-amd64.deb && \
    rm rstudio-server-2024.09.1-394-amd64.deb

# Create an RStudio user
RUN useradd -m rstudio && \
    echo "rstudio:rstudio" | chpasswd && \
    adduser rstudio sudo

# Install R packages
RUN R -e 'install.packages("remotes", repos="https://cran.rstudio.com")' && \
    R -e 'install.packages("BiocManager", repos="https://cran.rstudio.com")' && \
    R -e 'BiocManager::install(c("BiocStyle", "vjcitn/Rcollectl", "knitcitations"), ask = FALSE)' && \
    R -e 'install.packages(c("R6", "processx", "reticulate", "Matrix", "rsvd", "bench", "devtools"), repos="https://cran.rstudio.com")'

# Install Python GPU packages
RUN pip3 install --no-cache-dir \
    cupy-cuda12x \
    scipy \
    numpy \
    scikit-learn

# Expose RStudio port
EXPOSE 8787

# Start RStudio Server with GPU support
CMD ["/usr/lib/rstudio-server/bin/rserver", "--server-daemonize=0"]