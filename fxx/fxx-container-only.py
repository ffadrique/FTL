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
parser.add_argument('--templates-dir', required=True, default=tesmplates_dir, help='Location of the container templates (default to path of fxx.py)')
parser.add_argument('--name', required=True, default='list', choices=Container.names, help='Container name (default: list)')
parser.add_argument('--module', required=True, help='Module name')
parser.add_argument('--type', required=True, help='Type name')
parser.add_argument('--constructor', required=False, help='Constructor name (optional if type has the form "t_typename", then constructor = "typenme")')
parser.add_argument('--output-dir', required=False, default='.', help='Output directory (default: . or the directory of the input file')
parser.add_argument('--pure', required=False, default=True, action='store_true', help='Configure the container to have pure accessors (default: True)' )
parser.add_argument('--polymorphic', required=False, default=False, action='store_true', help='Configure the container to store polymorphic objects (default: False)' )

# Process command line
args = parser.parse_args()
verbose = args.verbose
templates_dir = args.templates_dir
name = args.name
module = args.module
type = args.type
output_dir = args.output_dir
if verbose: print('Command line:', args)
if args.constructor == None:
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type
else:
    constructor = args.constructor
polymorphic = args.polymorphic
pure = args.pure

# Initialise the container
container = Container( name, module, type, constructor, polymorphic )
container.pure = pure

# Expand the template
container.expand(templates_dir, output_dir, Container.templates_extension, verbose)


