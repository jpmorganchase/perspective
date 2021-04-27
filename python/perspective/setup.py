# *****************************************************************************
#
# Copyright (c) 2019, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.
#
from __future__ import print_function

import os
import os.path
import platform
import re
import subprocess
import sys
from codecs import open
from distutils.version import LooseVersion

from jupyter_packaging import combine_commands  # install_npm,
from jupyter_packaging import create_cmdclass, ensure_targets, get_version
from setuptools import Extension, find_packages, setup
from setuptools.command.build_ext import build_ext
from setuptools.command.sdist import sdist

try:
    from shutil import which

    CPU_COUNT = os.cpu_count()
except ImportError:
    # Python2
    try:
        from backports.shutil_which import which
    except ImportError:
        # just rely on path
        def which(x):
            return x

    import multiprocessing

    CPU_COUNT = multiprocessing.cpu_count()

here = os.path.abspath(os.path.dirname(__file__))
name = "perspective"

with open(os.path.join(here, "README.md"), encoding="utf-8") as f:
    long_description = f.read().replace("\r\n", "\n")

requires = [
    "ipywidgets>=7.5.1",
    "future>=0.16.0",
    "numpy>=1.13.1",
    "pandas>=0.22.0",
    "python-dateutil>=2.8.0",
    "six>=1.11.0",
    "tornado>=4.5.3",
    "traitlets>=4.3.2",
]

if sys.version_info.major < 3:
    requires += ["backports.shutil-which"]

if (sys.version_info.major == 2 and sys.version_info.minor < 7) or (
    sys.version_info.major == 3 and sys.version_info.minor < 6
):
    raise Exception("Requires Python 2.7/3.6 or later")

requires_dev_py2 = [
    "Faker>=1.0.0",
    "flake8>=3.7.8",
    "jupyter_packaging",
    "mock",
    "pybind11>=2.4.0",
    "pyarrow>=0.16.0",
    "pytest>=4.3.0",
    "pytest-cov>=2.6.1",
    "pytest-check-links",
    "pytest-tornado",
    "pytz>=2018.9",
    "Sphinx>=1.8.4",
    "sphinx-markdown-builder>=0.5.2",
] + requires

requires_dev = [
    "flake8-black>=0.2.0",
    "black==20.8b1",
] + requires_dev_py2  # for development, remember to install black and flake8-black


version = get_version(os.path.join(here, name, "core", "_version.py"))

# Representative files that should exist after a successful build
jstargets = [
    # os.path.join(here, name, 'nbextension', 'index.js'),
    os.path.join(here, name, "labextension", "package.json"),
]


package_data_spec = {name: ["nbextension/**js*", "labextension/**"]}


data_files_spec = [
    (
        "share/jupyter/nbextensions/finos-perspective-jupyterlab",
        "{}/nbextension".format(name),
        "**",
    ),
    (
        "share/jupyter/labextensions/@finos/perspective-jupyterlab",
        "{}/labextension".format(name),
        "**",
    ),
    ("share/jupyter/labextensions/@finos/perspective-jupyterlab", ".", "install.json"),
    ("etc/jupyter/nbconfig/notebook.d", ".", "finos-perspective-jupyterlab.json"),
]


cmdclass = create_cmdclass(
    "jsdeps", package_data_spec=package_data_spec, data_files_spec=data_files_spec
)

cmdclass["jsdeps"] = combine_commands(
    # install_npm(here, build_cmd='build_python_labextension'),
    ensure_targets(jstargets),
)


class PSPExtension(Extension):
    def __init__(self, name, sourcedir="dist"):
        Extension.__init__(self, name, sources=[])
        self.sourcedir = os.path.abspath(sourcedir)


class PSPBuild(build_ext):
    def run(self):
        self.run_cmake()

    def run_cmake(self):
        self.cmake_cmd = which("cmake")
        try:
            out = subprocess.check_output([self.cmake_cmd, "--version"])
        except OSError:
            raise RuntimeError(
                "CMake must be installed to build the following extensions: "
                + ", ".join(e.name for e in self.extensions)
            )

        if platform.system() == "Windows":
            cmake_version = LooseVersion(
                re.search(r"version\s*([\d.]+)", out.decode()).group(1)
            )
            if cmake_version < "3.1.0":
                raise RuntimeError("CMake >= 3.1.0 is required on Windows")

        for ext in self.extensions:
            self.build_extension_cmake(ext)

    def build_extension_cmake(self, ext):
        extdir = os.path.abspath(os.path.dirname(self.get_ext_fullpath(ext.name)))
        cfg = "Debug" if self.debug else "Release"

        PYTHON_VERSION = "{}.{}".format(sys.version_info.major, sys.version_info.minor)

        cmake_args = [
            "-DCMAKE_LIBRARY_OUTPUT_DIRECTORY="
            + os.path.abspath(os.path.join(extdir, "perspective", "table")).replace(
                "\\", "/"
            ),
            "-DCMAKE_BUILD_TYPE=" + cfg,
            "-DPSP_CPP_BUILD=1",
            "-DPSP_WASM_BUILD=0",
            "-DPSP_PYTHON_BUILD=1",
            "-DPSP_PYTHON_VERSION={}".format(PYTHON_VERSION),
            "-DPython_ADDITIONAL_VERSIONS={}".format(PYTHON_VERSION),
            "-DPython_FIND_VERSION={}".format(PYTHON_VERSION),
            "-DPython_EXECUTABLE={}".format(sys.executable).replace("\\", "/"),
            "-DPython_ROOT_DIR={}".format(sys.prefix).replace("\\", "/"),
            "-DPython_ROOT={}".format(sys.prefix).replace("\\", "/"),
            "-DPSP_CMAKE_MODULE_PATH={folder}".format(
                folder=os.path.join(ext.sourcedir, "cmake")
            ).replace("\\", "/"),
            "-DPSP_CPP_SRC={folder}".format(folder=ext.sourcedir).replace("\\", "/"),
            "-DPSP_PYTHON_SRC={folder}".format(
                folder=os.path.join(ext.sourcedir, "..", "perspective").replace(
                    "\\", "/"
                )
            ),
        ]

        build_args = ["--config", cfg]

        if platform.system() == "Windows":
            import distutils.msvccompiler as dm

            msvc = {
                "12": "Visual Studio 12 2013",
                "14": "Visual Studio 14 2015",
                "14.1": "Visual Studio 15 2017",
            }.get(dm.get_build_version(), "Visual Studio 15 2017")

            cmake_args.extend(
                [
                    "-DCMAKE_LIBRARY_OUTPUT_DIRECTORY_{}={}".format(
                        cfg.upper(), extdir
                    ).replace("\\", "/"),
                    "-G",
                    os.environ.get("PSP_GENERATOR", msvc),
                ]
            )

            if sys.maxsize > 2 ** 32:
                # build 64 bit to match python
                cmake_args += ["-A", "x64"]

            build_args += [
                "--",
                "/m:{}".format(CPU_COUNT),
                "/p:Configuration={}".format(cfg),
            ]
        else:
            cmake_args += ["-DCMAKE_BUILD_TYPE=" + cfg]
            build_args += [
                "--",
                "-j2" if os.environ.get("DOCKER", "") else "-j{}".format(CPU_COUNT),
            ]

        env = os.environ.copy()
        env["PSP_ENABLE_PYTHON"] = "1"
        env["OSX_DEPLOYMENT_TARGET"] = "10.9"

        if not os.path.exists(self.build_temp):
            os.makedirs(self.build_temp)

        subprocess.check_call(
            [self.cmake_cmd, os.path.abspath(ext.sourcedir)] + cmake_args,
            cwd=self.build_temp,
            env=env,
            stderr=subprocess.STDOUT,
        )
        subprocess.check_call(
            [self.cmake_cmd, "--build", "."] + build_args,
            cwd=self.build_temp,
            env=env,
            stderr=subprocess.STDOUT,
        )
        print()  # Add an empty line for cleaner output


cmdclass["build_ext"] = PSPBuild


class PSPCheckSDist(sdist):
    def run(self):
        self.run_check()
        super(PSPCheckSDist, self).run()

    def run_check(self):
        for file in ("CMakeLists.txt", "cmake", "src"):
            path = os.path.abspath(os.path.join(here, "dist", file))
            if not os.path.exists(path):
                raise Exception(
                    "Path is missing! {}\nMust run `yarn build_python` before building sdist so cmake files are installed".format(
                        path
                    )
                )
        for file in ("labextension/package.json", "nbextension/index.js"):
            path = os.path.abspath(os.path.join(here, "perspective", file))
            if not os.path.exists(path):
                raise Exception(
                    "Path is missing! {}\nMust run `yarn build_js` before building sdist so extension js files are installed".format(
                        path
                    )
                )


cmdclass["sdist"] = PSPCheckSDist


setup(
    name="perspective-python",
    version=version,
    description="Python bindings and JupyterLab integration for Perspective",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/finos/perspective",
    author="Perspective Authors",
    author_email="open_source@jpmorgan.com",
    license="Apache 2.0",
    classifiers=[
        "Development Status :: 3 - Alpha",
        "Programming Language :: Python :: 2",
        "Programming Language :: Python :: 2.7",
        "Programming Language :: Python :: 3",
        "Programming Language :: Python :: 3.6",
        "Programming Language :: Python :: 3.7",
        "Programming Language :: Python :: 3.8",
    ],
    keywords="analytics tools plotting",
    packages=find_packages(exclude=["bench", "bench.*"]),
    include_package_data=True,
    zip_safe=False,
    install_requires=requires,
    extras_require={"dev": requires_dev, "devpy2": requires_dev_py2},
    ext_modules=[PSPExtension("perspective")],
    cmdclass=cmdclass,
)
