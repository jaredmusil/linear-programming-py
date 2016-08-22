#!/usr/bin/env python

from cx_Freze import setup, Executable

includes = ['']
excludes = []
packages = []
include_files = [r'README.md',
                 r'bin/icon.ico']

setup(
    name='linear-programming-tookit',
    description='A simple app that can solve linear inequalities using the simplex method',
    author='',
    author_email='jaredmusil@gmail.com',
    options={
        'build_exe': {
            'excludes': excludes,
            'packages': packages,
            'include_files': include_files
        }
    },
    executables = [Executable({r'bin/app.py'})]
)