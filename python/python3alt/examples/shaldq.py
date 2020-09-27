# $Id$
#
#  Copyright (C) 2005   Gregory P. Smith (greg@krypto.org)
#  Licensed to PSF under a Contributor Agreement.
#
# This is Python's sha.py co-opted to include a mish-mash of Python2-3 cases. Many
#   of the mish-mash-items stem from this write-up:
#		http://sebastianraschka.com/Articles/2014_python_2_3_key_diff.html

import warnings

# Python 2ism
from __future__ import division

warnings.warn("the sha module is deprecated; use the hashlib module instead",
                DeprecationWarning, 2)

from hashlib import sha1 as sha
new = sha

blocksize = 1        # legacy value (wrong in any useful sense)
digest_size = 20
digestsize = 20

print("python 3 should be ok with this.") 

# Python 2ism
print err2, 'bla bla bla'
print "python 3 should NOT like this."

# Python 2ism (xrange is not in python 3)
print(xrange(10))

# Python 3ism
range.__contains__


# Python 2ism
raise IOError, "file error"


# Python 3ism
raise IOError("file error")


# Python 2ism
try:
    print("Not really causing an error here.. interested in the 'except' clause.")
except NameError, err:
    print err, '--> our error message'


# Python 3ism
try:
    print("Not really causing an error here.. interested in the 'except' clause.")
except NameError as err:
    print(err, '--> our error message')
    



my_generator = (letter for letter in 'abcdefg')
next(my_generator)
# Python 2ism (.next() does not exist in 3)
my_generator.next()



# Python 2ism (raw_input does not exist in 3)
my_input = raw_input('enter a number: ')
