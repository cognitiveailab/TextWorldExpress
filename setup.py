import os.path, sys

from setuptools import setup

with open(os.path.join("textworld_express", "version.py")) as f:
    VERSION = f.readlines()[0].split("=")[-1].strip("' \n")

BASEPATH = os.path.dirname(os.path.abspath(__file__))
JAR_FILE = 'textworld-express-{version}.jar'.format(version=VERSION)
JAR_PATH = os.path.join(BASEPATH, 'textworld_express', JAR_FILE)
OBJECTS_LUT_FILE = "object_type_ids.tsv"

if not os.path.isfile(JAR_PATH):
    print('ERROR: Unable to find required library:', JAR_PATH)
    sys.exit(1)

setup(name='textworld-express',
    version=VERSION,
    description='TextWorldExpress: a highly optimized reimplementation of three text game benchmarks focusing on instruction following, commonsense reasoning, and object identification.',
    author='Peter Jansen',
    packages=['textworld_express'],
    include_package_data=True,
    package_dir={'textworld_express': 'textworld_express'},
    package_data={'textworld_express': [JAR_FILE, OBJECTS_LUT_FILE]},
    url="https://github.com/cognitiveailab/TextWorldExpress",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    install_requires=open('requirements.txt').readlines(),
    extras_require={
        'webserver': open('requirements.txt').readlines() + ['pywebio'],
    },
)
