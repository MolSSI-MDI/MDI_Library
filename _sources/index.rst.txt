.. molssi_doc_theme documentation master file, created by
   sphinx-quickstart on Thu Mar 15 13:55:56 2018.
   You can adapt this file completely to your liking, but it should at least
   contain the root `toctree` directive.


The MolSSI Driver Interface Project
=========================================================

.. image:: _static/mdi-dark.png
    :width: 600px
    :align: center
    :class: only-dark

.. image:: _static/mdi-light.png
    :width: 600px
    :align: center
    :class: only-light

The MolSSI Driver Interface (MDI) project provides a :ref:`standardized API<mdi_standard>` for fast, on-the-fly communication between computational chemistry codes.
Traditionally, enabling communication between various computational chemistry codes has required a cumbersome process of writing and reading files to disk. 
The MolSSI Driver Interface (MDI) project offers a streamlined API that facilitates direct, on-the-fly communication between different programs. 
With MDI, researchers can now seamlessly link multiple computational tools using a Python or C++ script, bypassing the need for intermediate files. 

.. grid:: 1 1 2 2

    .. grid-item-card:: Getting Started
      :margin: 0 3 0 0 
      
      Learn how to use MDI for ab initio MD and how to write your own driver.

      .. button-link:: ./getting_started/index.html
         :color: primary
         :expand:

         To the Getting Started Guide

    .. grid-item-card::  User Guide
      :margin: 0 3 0 0
      
      The User Guide provides detailed information on the design of and how to use MDI.

      .. button-link:: ./user_guide/index.html
         :color: primary
         :expand:

         To the User Guide

    .. grid-item-card:: API Reference
      :margin: 0 3 0 0
      
      The API Reference provides detailed information on the MDI Standard, including the C, C++, and Python APIs.

      .. button-link:: ./api/index.html
         :color: primary
         :expand:

         To the API Reference.

    .. grid-item-card::  Developer Guide
      :margin: 0 3 0 0
      
      How to make your code MDI-compliant.

      .. button-link:: ./developer_guide.html
         :color: primary
         :expand:

         To the Developer Guide

.. toctree::
   :maxdepth: 2
   :hidden:
   :titlesonly:

   getting_started/index
   user_guide/index
   api/index
   developer_guide/index


