import os
from pymatgen.io.vasp.inputs import Incar
from magmango.in_out.in_out import read_incar, write_incar

class IncarSettings:
    def __init__(self, input_settings=None):
        #add exception handeling
        self._settings = input_settings
        self._file_path = None

    def from_file(self, file_path):
        #add exception handeling
        self._file_path = file_path
        self._settings  = read_incar(file_path)

    def from_dict(self, input_settings):
        #add exception handeling
        self._settings = input_settings

    def get_settings(self, key):
        ## add excpetion handeling to ensure that settings exist
        return self._settings[key]

    def update_settings(self, key, value):
        if self._settings:
            self._settings[key] = value
        else:
            pass
            ## return error message

    def remove_settings(self, key):
        if self._settings:
            self._settings.pop(key)
        else:
            pass
        ## return error message

    def write_file(self, path):
        ## add exception handeling to see if file exists
        if self._settings:
            write_incar(path, self._settings)
        else:
            pass
            ## add exception handeling
