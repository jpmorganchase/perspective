# *****************************************************************************
#
# Copyright (c) 2020, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#

FROM quay.io/pypa/manylinux2010_x86_64
RUN yum -y install rapidjson-devel sudo wget

# Build new cmake
RUN wget https://cmake.org/files/v3.15/cmake-3.15.4-Linux-x86_64.sh -q
RUN mkdir /opt/cmake
RUN printf "y\nn\n" | sh cmake-3.15.4-Linux-x86_64.sh --prefix=/opt/cmake > /dev/null
RUN rm -fr cmake*.sh /opt/cmake/doc
RUN rm -fr /opt/cmake/bin/cmake-gui
RUN rm -fr /opt/cmake/bin/ccmake
RUN rm -fr /opt/cmake/bin/cpack
RUN ln -s /opt/cmake/bin/cmake /usr/local/bin/cmake
RUN ln -s /opt/cmake/bin/ctest /usr/local/bin/ctest

# Remove auditwheel for reinstall
RUN rm -rf /opt/python/cp36-cp36m/bin/auditwheel
RUN rm -rf /opt/python/cp37-cp37m/bin/auditwheel
RUN rm -rf /opt/python/cp38-cp38/bin/auditwheel


# Copy assets
RUN cp -arf /opt/python/cp36-cp36m/* /usr/local/
RUN cp -arf /opt/python/cp37-cp37m/* /usr/local/
RUN cp -arf /opt/python/cp38-cp38/* /usr/local/

ENV PATH=/usr/local/bin:$PATH

# Install dependencies
RUN python3.6 -m pip install numpy scipy pybind11 cython codecov mock black>=20 flake8-black pytest pytest-cov traitlets ipywidgets faker psutil
RUN python3.7 -m pip install numpy scipy pybind11 cython codecov mock black>=20 flake8-black pytest pytest-cov traitlets ipywidgets faker psutil
RUN python3.8 -m pip install numpy scipy pybind11 cython codecov mock black>=20 flake8-black pytest pytest-cov traitlets ipywidgets faker psutil

# Install Auditwheel
RUN python3.6 -m pip install --ignore-installed auditwheel
RUN python3.7 -m pip install --ignore-installed auditwheel
RUN python3.8 -m pip install --ignore-installed auditwheel

# install boost
RUN wget https://dl.bintray.com/boostorg/release/1.71.0/source/boost_1_71_0.tar.gz >/dev/null 2>&1
RUN tar xfz boost_1_71_0.tar.gz
# https://github.com/boostorg/build/issues/468
RUN cd boost_1_71_0 && ./bootstrap.sh
RUN cd boost_1_71_0 && ./b2 -j8 --with-program_options --with-filesystem --with-system install 

RUN python2.7 -m pip install 'numpy>=1.13.1' 'pandas>=0.22.0' 'pyarrow>=0.16.0' git+https://chromium.googlesource.com/external/gyp
RUN python3.6 -m pip install 'numpy>=1.13.1' 'pandas>=0.22.0' 'pyarrow>=2.0.0'
RUN python3.7 -m pip install 'numpy>=1.13.1' 'pandas>=0.22.0' 'pyarrow>=2.0.0'
RUN python3.8 -m pip install 'numpy>=1.13.1' 'pandas>=0.22.0' 'pyarrow>=2.0.0'

# install node
RUN curl -sL https://rpm.nodesource.com/setup_10.x | sudo bash -
RUN yum install -y nodejs

RUN npm install --global yarn
RUN yarn --version

# install flatbuffers
RUN mkdir -p /usr/local \
    && cd /usr/local \
    && git clone https://github.com/google/flatbuffers.git \
    && cd flatbuffers \
    && cmake -G "Unix Makefiles" \
    && make \
    && cp -r /usr/local/flatbuffers/include/flatbuffers /usr/local/include \
    && ln -s /usr/local/flatbuffers/flatc /usr/local/bin/flatc \
    && chmod +x /usr/local/flatbuffers/flatc
