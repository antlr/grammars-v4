
# First, check this one
# import_stmt: IMPORT dotted_as_names
# dotted_as_names: dotted_as_name (COMMA dotted_as_name)*;
# dotted_as_name: dotted_name (AS NAME)?;

# IMPORT dotted_name
import collections.abc

# IMPORT dotted_name AS NAME
import collections.abc as ss

# IMPORT dotted_name AS NAME COMMA dotted_name
import collections.abc as ss, itertools

# Then check this
# import_from: FROM   ((DOT | ELLIPSIS)* dotted_name | (DOT | ELLIPSIS)+)
#              IMPORT (STAR | OPEN_PAREN import_as_names CLOSE_PAREN | import_as_names);
# import_as_names: import_as_name (COMMA import_as_name)*;
# import_as_name: NAME (AS NAME)?;

# FROM dotted_name IMPORT import_as_name
from classdef import bar as b

# FROM DOT IMPORT import_as_name
from . import bar

# FROM DOT DOT DOT IMPORT import_as_name
from . . . import bar

# FROM DOT dotted_name IMPORT import_as_name
from .classdef import bar

# FROM dotted_name IMPORT STAR
from sys import *

# FROM dotted_name IMPORT import_as_name COMMA import_as_name
from collections import bar, baz

# FROM dotted_name IMPORT OPEN_PAREN import_as_name CLOSE_PAREN
from collections import (bar)
