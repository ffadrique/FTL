import re

from container import Container

# Initialise prefixes for pure and polymorphic container configuraiton
pure_prefix = '~'
pure_prefix_pattern = '[' + pure_prefix + ']'
polymorphic_prefix = '*'
polymorphic_prefix_pattern = '[' + polymorphic_prefix + ']'


# Define base patterns
alnum = "[a-z0-9][a-z0-9_]+"
base_pattern = '[<](' + alnum + ')[ ]*[,][ ]*(' + polymorphic_prefix_pattern + ')?(' + alnum + ')[ ]*[>]'
containers_list_pattern = '|'.join(Container.names)


# Define the pattern for use statements
use_pattern = '([ ]*)use[ ]+[<][ ]*(' + pure_prefix_pattern + ')?(' + containers_list_pattern + ')[ ]*[>](.*)'

# Process a use statement
def use_expand(m, line, lines, containers, verbose):

    # Process the line
    tab = m.group(1)
    prefix = m.group(2)
    cname = m.group(3)
    only = m.group(4)
    if verbose: print('Use: ', cname)

    # Loop on the containers for the current container name
    uses = []
    for c in filter(lambda x: x.name == cname, containers):
        try:
            uses.append(tab + 'use ' + c.module + '_' + c.name + '_ftl' + only)
        except TypeError:
            uses.append(tab + 'use ' + c.module + '_' + c.name + '_ftl')
    if verbose: print('Uses: ', uses)

    # Expand the uses
    index = lines.index(line)
    lines[index:index + 1] = uses

    # Check if the containers must implement pure interfaces
    if prefix == None:
        for c in filter(lambda x: x.name == cname, containers):
            c.pure = False


# Define the pattern for the detection of a container declaration
# This detects statements of the form: '   container<module,type>, attributes :: x'
#                                      '   container_iterator<module,type>, attributes :: x'
declaration_pattern = '([ ]*)(' + containers_list_pattern + ')(_iterator)?[ ]*'+ base_pattern + '(.*)'

# Process a container declaration
def declaration_expand(m, line, lines, verbose):

    # Process the record
    tab = m.group(1)
    container = m.group(2)
    iterator = m.group(3)
    module = m.group(4)
    polymorphic = m.group(5)
    type = m.group(6)
    vars = m.group(7)
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type

    if verbose: 
        print('Line: ', line)
        print('Iterator: ', iterator)
        print('Module: ', module)
        print('Type: ', type)
        print('Constructor: ', constructor)
        print('Variables: ', vars)

    # Check if the object has the intent attribute (function/subroutine argument)
    if re.match( '.*intent[ ]*[(]', vars, re.IGNORECASE ):
        derived = 'class'
    else:
        derived = 'type'

    # Format the expanded line
    try:
        newline = tab + derived +'(' + type + '_' + container + '_ftl' + iterator + ')' + vars
    except TypeError:
        newline = tab + derived + '(' + type + '_' + container + '_ftl)' + vars
    if verbose: 
        print('Expanded line: ' + newline)

    # Replace the line in the source code
    index = lines.index(line)
    lines[index] = newline

    # Build the container information
    container = Container(container, module, type, constructor, polymorphic)
    return container


# Pattern for the template type selection statement
# This detects statements of the form: '   type is( <module,type> )'
#                                      '   class is( <module,type> )'
selection_pattern = "([ ]*(class|type)[ ]+is[ ]*[(][ ]*)(" + containers_list_pattern + ")(_iterator)?[ ]*" + base_pattern + "([ ]*[)].*)"

# Process a selection declaration
def selection_expand(m, line, lines, verbose):

    # Process the record
    lead = m.group(1)
    container = m.group(3)
    iterator = m.group(4)
    module = m.group(5)
    polymorphic = m.group(6)
    type = m.group(7)
    trail = m.group(8)
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type
    if verbose: 
        print('Line: ', line)
        print('Lead: ', lead)
        print('Iterator: ', iterator)
        print('Module: ', module)
        print('Type: ', type)
        print('Constructor: ', constructor)
        print('Trail: ', trail)

    # Format the expanded line
    try:
        newline = lead + type + '_' + container + '_ftl' + iterator + trail
    except TypeError:
        newline = lead + type + '_' + container + '_ftl' + trail
    if verbose: 
        print('Expanded line: ' + newline)

    # Replace the line in the source code
    index = lines.index(line)
    lines[index] = newline

    # Build the container information
    container = Container(container, module, type, constructor, polymorphic)
    return container



# Pattern for the template reference statement; to refer to the base type in a type derived by inheritance
# This detects statements of the form: '   x%container<module,type>'
reference_pattern = "(.*[ ]*[%][ ]*)(" + containers_list_pattern + ")(_iterator)?[ ]*" + base_pattern + "(.*)"

# Process a selection statement
def reference_expand(m, line, lines, verbose):

    # Process the record
    lead = m.group(1)
    container = m.group(2)
    iterator = m.group(3)
    module = m.group(4)
    polymorphic = m.group(5)
    type = m.group(6)
    trail = m.group(7)
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type
    if verbose: 
        print('Line: ', line)
        print('Lead: ', lead)
        print('Iterator: ', iterator)
        print('Module: ', module)
        print('Type: ', type)
        print('Constructor: ', constructor)
        print('Trail: ', trail)

    # Format the expanded line
    try:
        newline = lead + type + '_' + container + '_ftl' + iterator + trail
    except TypeError:
        newline = lead + type + '_' + container + '_ftl' + trail
    if verbose: 
        print('Expanded line: ' + newline)

    # Replace the line in the source code
    index = lines.index(line)
    lines[index] = newline

    # Build the container information
    container = Container(container, module, type, constructor, polymorphic)
    return container



# Pattern for the template constructor statement
# This detects statements of the form '   x = container<module,type>(y)'
constructor_pattern = "(.*[ ]*)(" + containers_list_pattern + ")(_iterator)?[ ]*" + base_pattern + "([ ]*[(].*[)][ ]*)"

# Process a constructor statement
def constructor_expand(m, line, lines, verbose):

    # Process the record
    lead = m.group(1)
    container = m.group(2)
    iterator = m.group(3)
    module = m.group(4)
    polymorphic = m.group(5)
    type = m.group(6)
    trail = m.group(7)
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type
    if verbose: 
        print('Line: ', line)
        print('Lead: ', lead)
        print('Iterator: ', iterator)
        print('Module: ', module)
        print('Type: ', type)
        print('Constructor: ', constructor)
        print('Trail: ', trail)

    # Format the expanded line
    try:
        newline = lead + constructor + '_' + container + '_ftl' + iterator + trail
    except TypeError:
        newline = lead + constructor + '_' + container + '_ftl' + trail
    if verbose: 
        print('Expanded line: ' + newline)

    # Replace the line in the source code
    index = lines.index(line)
    lines[index] = newline

    # Build the container information
    container = Container(container, module, type, constructor, polymorphic)
    return container



# Pattern for the template declaration as inherited type
# This detects statements of the form '   type, extends(container<module,type>), attributes :: x'
inherited_pattern = "([ ]*type[ ]*(,[ ]*abstract)?[ ]*[,][ ]*extends[(])(" + containers_list_pattern + ")(_iterator)?[ ]*" + base_pattern + "([ ]*[)].*)"

# Process a inheritance statement
def inherited_expand(m, line, lines, verbose):

    # Process the record
    lead = m.group(1)
    container = m.group(3)
    iterator = m.group(4)
    module = m.group(5)
    polymorphic = m.group(6)
    type = m.group(7)
    trail = m.group(8)
    if type[0:2] == 't_': 
        constructor = type[2:]
    else: 
        constructor = type
    if verbose: 
        print('Line: ', line)
        print('Lead: ', lead)
        print('Iterator: ', iterator)
        print('Module: ', module)
        print('Type: ', type)
        print('Constructor: ', constructor)
        print('Trail: ', trail)

    # Format the expanded line
    try:
        newline = lead + type + '_' + container + '_ftl' + iterator + trail
    except TypeError:
        newline = lead + type + '_' + container + '_ftl' + trail
    if verbose: 
        print('Expanded line: ' + newline)

    # Replace the line in the source code
    index = lines.index(line)
    lines[index] = newline

    # Build the container information
    container = Container(container, module, type, constructor, polymorphic)
    return container

