import os

from datetime import datetime

class Container(object):
    """Container metadata"""

    # Initialise the list of containers and pattern
    names = ['list', 'slist', 'vector', 'queue', 'stack', 'tree']

    # Template default extension
    templates_extension = '.f90'

    # Default consturctor
    def __init__(self):
        self.name = ''
        self.module = ''
        self.type = ''
        self.constructor = ''
        self.polymorphic = False
        self.pure = True

    # Constructor
    def __init__(self, name, module, type, constructor, polymorphic):
        self.name = name
        self.module = module
        self.type = type
        self.constructor = constructor
        self.polymorphic = polymorphic
        self.pure = True

    # Expand container template
    def expand(self, templates_directory, output_directory, output_file_extension, verbose):

        # Container items
        cname = self.name
        module = self.module
        type = self.type
        constructor = self.constructor

        # Load the container template
        template_path = os.path.join(templates_directory, cname + self.templates_extension)
        if verbose: print('Template: ', template_path)
        with open(template_path) as file:
            template = file.read()
        
        # Check if the container shall contain polymorphic objects
        if not self.polymorphic:
            template = template.replace("class(xxtypebase__)", "type(xxtypebase__)")

        # Replace the template placeholders
        template = template.replace("xxuse__", module)
        template = template.replace("xxmodulebase__", module)
        template = template.replace("xxtypebase__", type)
        template = template.replace("xxconstructor__", constructor)
    
        # Select pure access to accessors
        if not self.pure:
            template = template.replace("pure subroutine element_assign_allocatable", "subroutine element_assign_allocatable")
            template = template.replace("pure subroutine element_assign_pointer", "subroutine element_assign_pointer")
            template = template.replace("pure function " + cname + "_front", "function " + cname + "_front")
            template = template.replace("pure function " + cname + "_back", "function " + cname + "_back")
            template = template.replace("pure function " + cname + "_top", "function " + cname + "_top")
            template = template.replace("pure function " + cname + "_bottom", "function " + cname + "_bottom")
            template = template.replace("pure function " + cname + "_iterator_get_element", "function " + cname + "_iterator_get_element")
            template = template.replace("pure subroutine " + cname + "_iterator_set_element", "subroutine " + cname + "_iterator_set_element")

        # Add the time stamp
        template += '\n! ' + datetime.now().strftime("%Y-%m-%dT%H:%M:%S") + '\n'

        # Expanded container file name
        container_path = os.path.join(output_directory, self.module + '_' + self.name + '_ftl' + output_file_extension)
        if verbose: print('Container: ', container_path)
    
        # Write the expanded container
        with open(container_path, "w") as file:
            file.write(template)

    # Check if the object is in an array
    def is_in_array(self, array):

        # Initialise
        res = False

        # Loop on the array elements
        for item in array:
            
            # Check if the element in array is the same as self
            res = item.name == self.name and item.module == self.module and item.type == self.type 
            if res: break

        # Return match status
        return res
