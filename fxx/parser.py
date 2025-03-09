import re
import os

from datetime import datetime

from container import *
from matches import *

class Parser(object):
    """Parse a fortran source file with FTL metatags"""

    # Constructor
    def __init__(self, source_path):
        self.source = source_path

    # Parse and expand the source file
    def parse_and_expand(self, output_file_extension, verbose):

        # Read the source input file
        with open(self.source) as file:
            lines = file.read().splitlines()

        # Get the module name
        for line in lines:                                                                                                                  
            m = re.match('[ ]*module[ ]+([^ ]+)', line, re.IGNORECASE)
            if m:
                module = m.group(1)
                if verbose: print('Module: ', module)
                break

        # Loop on the lines in the input source file
        containers = []
        for line in lines:

            # Try to match a inheritance construct line
            m = re.match(inherited_pattern, line, re.IGNORECASE)
            if m:  
                item = inherited_expand(m, line, lines, verbose)
            else:

                # Try to match a reference to the base of a type dervied by inheritance line
                m = re.match(reference_pattern, line, re.IGNORECASE)
                if m:  
                    item = reference_expand(m, line, lines, verbose)
                else:

                    # Try to match a constructor line
                    m = re.match(constructor_pattern, line, re.IGNORECASE)
                    if m:  
                        item = constructor_expand(m, line, lines, verbose)
                    else:

                        # Try to match a type selection line
                        m = re.match(selection_pattern, line, re.IGNORECASE)
                        if m:  
                            item = selection_expand(m, line, lines, verbose)
                        else:

                            # Try to match a declaration line
                            m = re.match(declaration_pattern, line, re.IGNORECASE)
                            if m:  
                                item = declaration_expand(m, line, lines, verbose)

            # Store the container if there has been a match (only once)
            if m and not item.is_in_array(containers): containers.append(item)

        if verbose: print('Containers:', containers)

        # Loop on the lines to replace the use statements
        for line in lines:
            m = re.match(use_pattern, line, re.IGNORECASE)
            if m:
                use_expand(m, line, lines, containers, verbose)

        # Add the time stamp
        # lines.append('')
        # lines.append('! ' + datetime.now().strftime("%Y-%m-%dT%H:%M:%S") )

        # Write the expanded output source file
        filename, ext = os.path.splitext(self.source)
        output_file_path = filename + output_file_extension
        if verbose: print('Output source file: ', output_file_path)
        with open(output_file_path, "w") as file:
            file.write("\n".join(lines))

        # Return the containers for this file
        return containers