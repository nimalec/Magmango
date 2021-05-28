"""The incar module handles the INCAR parameters belonging to a VASP calculation.

The incar module allows for the initialization, modification, and managing of input/output of settings for VASP INCAR file parameters.
The incar module allows for settings to be extracted from a provided INCAR file or to be inputed manually.
The setting types are partitioned into: start, electronic, parallel, magnetic, ionic, hubbard, hybrid, misc.

 Typical usage example:

  from magmango.calculation.incar import IncarSettings
  incar_path = "/home/directory/INCAR"
  incar_obj = Incar(from_file = True)
  incar_obj.incar_from_file(incar_path)
  settings = incar_obj.get_settings(setting_type = "all")
"""

import os
from pymatgen.io.vasp.inputs import Incar
from magmango.in_out.in_out import read_incar, write_incar

class IncarSettings:
    def __init__(self, input_settings=None):
        self._settings = input_settings
        self._file_path = None

    def incar_from_file(self, file_path):
        """This function...."""
        self._file_path = file_path
        self._settings  = read_incar(file_path)

    def get_settings(self, key):
        return self._settings[key]

    def update_settings(self, key, value):
        self._settings[key] = value

    def write_file(self,path):
        write_incar(path, self._settings)
