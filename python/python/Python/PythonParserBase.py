import sys
from enum import Enum
from typing import TextIO

from antlr4 import Parser, TokenStream


class PythonVersion(Enum):
    Autodetect = 0
    Python2 = 2
    Python3 = 3


# To use the PythonParser in Python, make sure that the generated parser class and this class are in the
# same directory. Furthermore, the parser actions have to be adapted, since they're written for Java.
# For instance, `{CheckVersion(3)}` gets `{self._check_version(3)}`.
class PythonParserBase(Parser):
    def __init__(self, input_stream: TokenStream, output: TextIO = sys.stdout):
        super().__init__(input_stream, output)
        self.__version = PythonVersion.Autodetect

    @property
    def version(self) -> PythonVersion:
        return self.__version

    @version.setter
    def version(self, version: PythonVersion | int):
        if isinstance(version, PythonVersion):
            self.__version = version
        else:
            self.__version = PythonVersion(version)

    def _check_version(self, version: int) -> bool:
        return self.__version == PythonVersion.Autodetect or version == self.__version.value

    def set_version(self, required_version: int) -> None:
        self.__version = PythonVersion(required_version)
