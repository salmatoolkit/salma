from distutils.core import setup

from Cython.Build import cythonize

setup(
    name="Hellop world app",
    ext_modules= cythonize("hello.pyx")
)