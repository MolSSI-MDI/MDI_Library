MolSSI Driver Interface (MDI) Library
=====================================

[![Build Status](https://travis-ci.org/MolSSI/MDI_Library.svg?branch=master)](https://travis-ci.org/MolSSI/MDI_Library)
[![Build Status](https://dev.azure.com/taylorabarnes/MDI_Library/_apis/build/status/MolSSI.MDI_Library?branchName=master)](https://dev.azure.com/taylorabarnes/MDI_Library/_build/latest?definitionId=1&branchName=master)
[![Language grade: Python](https://img.shields.io/lgtm/grade/python/g/MolSSI/MDI_Library.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/MolSSI/MDI_Library/context:python)
[![Language grade: C/C++](https://img.shields.io/lgtm/grade/cpp/g/MolSSI/MDI_Library.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/MolSSI/MDI_Library/context:cpp)
[![codecov](https://codecov.io/gh/MolSSI/MDI_Library/branch/master/graph/badge.svg)](https://codecov.io/gh/MolSSI/MDI_Library)

## Overview

The MolSSI Driver Interface (MDI) project provides a standardized API for fast, on-the-fly communication between computational chemistry codes.  This greatly simplifies the process of implementing methods that require the cooperation of multiple software packages and enables developers to write a single implementation that works across many different codes.  The API is sufficiently general to support a wide variety of techniques, including QM/MM, ab initio MD, machine learning, advanced sampling, and path integral MD, while also being straightforwardly extensible.  Communication between codes is handled by the MDI Library, which enables tight coupling between codes using either the MPI or TCP/IP methods.

## Documentation

Complete documentation can be found at https://molssi.github.io/MDI_Library

## License

The MDI Library is released under the BSD 3-clause license. See LICENSE for details.

## Acknowledgements

This work was supported by the Molecular Sciences Software Institute under U.S. National Science Foundation grant ACI-1547580.
