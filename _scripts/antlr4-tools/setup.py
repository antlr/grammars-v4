from setuptools import setup

# To RELEASE:
#
# $ pip install --upgrade build setuptools wheel twine  # update tools
# $ rm -rf dist build *.egg-info
# $ python -m build
# $ twine upload dist/*

v = '0.9.9'

setup(
    name='antlr4-tools',
    version=v,
    py_modules=['antlr4_tool_runner'],
    install_requires=[
        "install-jdk"
    ],
    url='http://www.antlr.org',
    license='MIT',
    author='Terence Parr',
    author_email='parrt@antlr.org',
    entry_points={'console_scripts': [
        'antlr4=antlr4_tool_runner:tool',
        'antlr4-parse=antlr4_tool_runner:interp'
    ]
    },
    description='Tools to run ANTLR4 tool and grammar interpreter/profiler',
    classifiers=['License :: OSI Approved :: MIT License',
                 'Intended Audience :: Developers']
)
