import sys
import argparse
import re
import os
import glob

from matches import *
from container import *
from parser import *

# Get the default path to the templates
templates_dir = os.path.dirname(__file__)
reffile = glob.glob("../*/list.f90", recursive=True)[0]
templates_dir = os.path.dirname(reffile)


# Define the command line options
parser = argparse.ArgumentParser()
parser.add_argument('--verbose', required=False, action='store_true', help='Trace detailed information of the process steps')
parser.add_argument('--templates-dir', required=False, default=templates_dir, help='Location of the container templates (default to path of fxx.py)')
parser.add_argument('source', help='Source file with meta code for template instantiation')


# Process command line
args = parser.parse_args()
verbose = args.verbose
filepath = args.source
templates_dir = args.templates_dir
if verbose: print('Command line:', args)


# Check input file name
filename, file_extension = os.path.splitext(filepath)
dirname = os.path.dirname(filepath)
if file_extension not in ['.t90', '.t95', '.t03', '.t08', '.t15', '.t18']:
    print('Invalid file extension "' + file_extension + '"')
    sys.exit()
if not os.path.isfile(filepath):
    print('Input file "' + filepath + '" not found')
    sys.exit()


# Initialise output file cofiguration
output_file_directory = dirname
output_file_extension = ".f" + file_extension[2:]


# Parse the input source file
parser = Parser(filepath)
containers = parser.parse_and_expand(output_file_extension, verbose)
    

# Loop on the containers to expand from templates
for container in containers:
    container.expand(templates_dir, output_file_directory, output_file_extension, verbose)
    