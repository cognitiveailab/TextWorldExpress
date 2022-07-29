import os.path, sys
from os.path import join as pjoin

from setuptools import setup

# Check if all required files are there.
with open(pjoin("textworld_express", "version.py")) as f:
    VERSION = f.readlines()[0].split("=")[-1].strip("' \n")

BASEPATH = os.path.dirname(os.path.abspath(__file__))
JAR_FILE = 'textworld-express-{version}.jar'.format(version=VERSION)
JAR_PATH = pjoin(BASEPATH, 'textworld_express', JAR_FILE)
OBJECTS_TWC_FILE = "twc_objects.folds.json"
OBJECTS_TWC_PATH = pjoin(BASEPATH, 'textworld_express', OBJECTS_TWC_FILE)
COOKING_WORLD_FILE = "cooking_world.json"
COOKING_WORLD_PATH = pjoin(BASEPATH, 'textworld_express', COOKING_WORLD_FILE)

missing_file = False
for required_file in [JAR_PATH, OBJECTS_TWC_PATH, COOKING_WORLD_PATH]:
    if not os.path.isfile(required_file):
        print('ERROR: Unable to find required file:', required_file)
        missing_file = True

if missing_file:
    sys.exit(1)  # Do not move forward with the installation.

setup(name='textworld_express',
    version=VERSION,
    description='TextWorldExpress: a highly optimized reimplementation of three text game benchmarks focusing on instruction following, commonsense reasoning, and object identification.',
    author='Peter Jansen',
    packages=['textworld_express'],
    include_package_data=True,
    package_dir={'textworld_express': 'textworld_express'},
    package_data={'textworld_express': [JAR_FILE, OBJECTS_TWC_FILE, COOKING_WORLD_FILE]},
    url="https://github.com/cognitiveailab/TextWorldExpress",
    long_description=open("README.md").read(),
    long_description_content_type="text/markdown",
    install_requires=open('requirements.txt').readlines(),
    extras_require={
        'webserver': open('requirements.txt').readlines() + ['pywebio'],
    },
)
