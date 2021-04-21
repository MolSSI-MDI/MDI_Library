import os
import re
import sys
import subprocess

from setuptools import setup, Extension
from setuptools.command.build_ext import build_ext
from distutils.version import LooseVersion


# Get version number information
base_path = os.path.dirname(os.path.realpath(__file__))
file_path = os.path.join( base_path, 'MDI_Library', 'mdi_global.h' )
found_major_version = False
found_minor_version = False
found_patch_version = False
with open( file_path ) as mdi_file:
    line = mdi_file.readline()
    while line and ( not found_major_version or not found_minor_version or not found_patch_version ):
        print("line: " + str(line) )
        sline = line.split()
        if line.startswith( '#define MDI_MAJOR_VERSION_ ' ):
            major_version = sline[2]
            found_major_version = True
        elif line.startswith( '#define MDI_MINOR_VERSION_ ' ):
            minor_version = sline[2]
            found_minor_version = True
        elif line.startswith( '#define MDI_PATCH_VERSION_ ' ):
            patch_version = sline[2]
            found_patch_version = True
        line = mdi_file.readline()
if not found_major_version or not found_minor_version or not found_patch_version:
    raise RuntimeError("Unable to identify package version number")
mdi_version = major_version + '.' + minor_version + '.' + patch_version


class CMakeExtension(Extension):
    def __init__(self, name, source_dir=''):
        Extension.__init__(self, name, sources=[])
        self.source_dir = os.path.abspath(source_dir)


class CMakeBuild(build_ext):
    def run(self):

        # Verify that CMake >= 3.5.0 is available
        try:
            version = subprocess.check_output(['cmake', '--version'])
        except OSError:
            raise RuntimeError("CMake is required for the following extensions: " +
                               ", ".join(e.name for e in self.extensions))
        cmake_version = LooseVersion(re.search(r'version\s*([\d.]+)', version.decode()).group(1))
        if cmake_version < '3.5.0':
            raise RuntimeError("CMake >= 3.5.0 is required for the following extensions: " +
                               ", ".join(e.name for e in self.extensions))

        # Build all CMake extensions
        for ext in self.extensions:
            self.build_extension(ext)

    def build_extension(self, ext):
        # Get build directory
        ext_dir = os.path.abspath(os.path.dirname(self.get_ext_fullpath(ext.name)))
        if not ext_dir.endswith(os.path.sep):
            ext_dir += os.path.sep
        install_dir = os.path.join( ext_dir, "mdi" )

        # Set CMake arguments
        cmake_args = ['-DCMAKE_INSTALL_PREFIX=' + install_dir,
                      '-DCMAKE_INSTALL_INCLUDEDIR=' + ext_dir,
                      '-DCMAKE_INSTALL_LIBDIR=' + ext_dir,
                      '-DPYTHON_EXECUTABLE=' + sys.executable]

        # Do the CMake install
        if not os.path.exists(self.build_temp):
            os.makedirs(self.build_temp)
        subprocess.check_call( ['cmake', ext.source_dir] + cmake_args,
                               cwd=self.build_temp)
        subprocess.check_call( ['cmake', '--build', '.'],
                               cwd=self.build_temp)
        subprocess.check_call( ['cmake', '--install', '.', '--prefix', install_dir],
                               cwd=self.build_temp)


setup(
    name='pymdi',
    version=mdi_version,
    description='A library that enables code interoperability via the MolSSI Driver Interface.',
    long_description='',
    url='https://github.com/MolSSI-MDI/MDI_Library',
    author='Taylor Barnes',
    author_email='info@molssi.org',
    license='BSD 3-clause',
    packages=['mdi'],
    package_dir={'mdi': 'MDI_Library'},
    ext_modules=[CMakeExtension('mdi')],
    cmdclass=dict(build_ext=CMakeBuild),
    install_requires=[],
    classifiers=[
        'Programming Language :: Python :: 3',
        'Programming Language :: Python :: 3.4',
        'Programming Language :: Python :: 3.5',
        'Programming Language :: Python :: 3.7',
        ],
    )
