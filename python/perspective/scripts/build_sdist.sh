#!/bin/bash
#
# Copyright (c) 2021, the Perspective Authors.
#
# This file is part of the Perspective library, distributed under the terms of
# the Apache License 2.0.  The full license can be found in the LICENSE file.

SDIST_NAME=perspective-python
SDIST_VERSION=`git describe --abbrev=0 --tags  | cut -c2-`
SDIST_FULL_NAME=$SDIST_NAME-$SDIST_VERSION
SDIST_FULL_NAME_TAR=$SDIST_FULL_NAME.tar.gz
SDIST_PUBLISH_NAME=$SDIST_PUBLISH_NAME.tar.gz

# Remove build assets
rm -rf perspective/table/*.{so,dll}

# Run sdist
echo "-----------------------"
echo "Building sdist"

python3 setup.py sdist

# Extract the sdist
echo "-----------------------"
echo "Extracting built sdist"

mkdir tmp && tar xfzv ./dist/$SDIST_FULL_NAME_TAR -C ./tmp

# build the sdisted assets
echo "-----------------------"
echo "Building extracted sdist"

cd tmp/$SDIST_FULL_NAME && python3 setup.py build

# try to import the libs
echo "-----------------------"
echo "Testing sdist-built assets"

python3 -c "import perspective;print(perspective.is_libpsp())"

# rename asset to be uploaded
echo "-----------------------"
echo "Renaming assets for CI publish"

cd ../../
mv ./dist/$SDIST_FULL_NAME_TAR ./dist/$SDIST_PUBLISH_NAME 

echo "-----------------------"
echo "Done.."
