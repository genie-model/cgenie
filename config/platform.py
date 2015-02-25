# F90 compiler.
f90 = 'gfortran'

# Default F90 flags.
f90flags = ['-O2', '-O3', '-funroll-loops', '-msse', '-fno-automatic',
            '-x', 'f95-cpp-input', '-ffree-line-length-none',
            '-fdefault-real-8', '-fimplicit-none']

# F90 flag for module file output directory.
f90moddir = '-J'

# F90 flag for preprocessor definitions.
f90define = '-D'

# NetCDF base directory.
netcdf = '/usr/local/netcdf-cxx-4.1.3/'

# NetCDF libraries (either a single 'netcdf', or 'netcdf' and
# 'netcdff' if the F90 library is split).
netcdflibs = ['netcdf', 'netcdff']
