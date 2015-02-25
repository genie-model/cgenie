# Model source directory.
srcdir = 'src'

# Dimension sizes.
nlons = 36
nlats = 36
nlevs = 16
ntracers = 14

# Coordinate definitions.
coordvars = { 'GENIENX':          nlons,
              'GENIENY':          nlats,
              'GOLDSTEINNLONS':   nlons,
              'GOLDSTEINNLATS':   nlats,
              'GOLDSTEINNLEVS':   nlevs,
              'GOLDSTEINNTRACS' : ntracers }
