MolSSI Driver Interface (MDI) Library
=====================================

[![Build Status](https://dev.azure.com/taylorabarnes/MDI_Library/_apis/build/status/MolSSI.MDI_Library?branchName=master)](https://dev.azure.com/taylorabarnes/MDI_Library/_build/latest?definitionId=1&branchName=master)
[![codecov](https://codecov.io/gh/MolSSI-MDI/MDI_Library/branch/master/graph/badge.svg)](https://codecov.io/gh/MolSSI-MDI/MDI_Library)

## Overview

The MolSSI Driver Interface (MDI) project provides a standardized API for fast, on-the-fly communication between computational chemistry codes.  This greatly simplifies the process of implementing methods that require the cooperation of multiple software packages and enables developers to write a single implementation that works across many different codes.  The API is sufficiently general to support a wide variety of techniques, including QM/MM, ab initio MD, machine learning, advanced sampling, and path integral MD, while also being straightforwardly extensible.  Communication between codes is handled by the MDI Library, which enables tight coupling between codes using either the MPI or TCP/IP methods.

## Documentation

Complete documentation can be found at https://molssi-mdi.github.io/MDI_Library

## License

The MDI Library is released under the BSD 3-clause license. See LICENSE for details.

## Acknowledgements

This work was supported by the Molecular Sciences Software Institute under U.S. National Science Foundation grant ACI-1547580.

MDI builds upon the work of numerous development groups, without whom it could not exist.
The syntactical structure of the MDI Standard, including the fundamental command-response communication pattern, is modelled after that used by the <a href="http://ipi-code.org/">i-PI</a> project, as is the string-based representation of commands.
The Node System draws inspiration from the techniques used by several molecular mechanics packages, especially <a href="https://lammps.sandia.gov/">LAMMPS</a> and <a href="http://openmm.org/">OpenMM</a>, to enable modular code additions.
The unit conversions available through the MDI Library were provided by the <a href="https://github.com/MolSSI/QCElemental">QCElemental</a> project.
Certain details of the communication protocols implemented by the MDI Library, especially pertaining to MPI-based communication in the MPMD regime, were informed by the accomplishments of the <a href="https://cslib.sandia.gov/">CSlib</a> library.
The library-based communication protocol was developed in response to discussions with the <a href="https://gitlab.com/exaalt">EXAALT</a> team.
The interface, error handling, data types, and numerous other elements of the MDI Library are modelled after the <a href="https://www.mpi-forum.org/">MPI Standard</a>.
A distribution of the MDI Library for Python is provided by <a href="https://conda-forge.org/">Conda Forge</a>.
