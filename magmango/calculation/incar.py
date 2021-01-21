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
    """ Summary of class.

        Longer class info.

        Attributes:
            _settings:
        """

    def __init__(self, from_file = False, start_settings = None, electronic_settings = None, parallel_settings = None, magnetic_settings = None, ionic_settings = None, hubbard_settings=None, hybrid_settings=None, misc_settings=None):
        """ IncarSettings constructor method.

        Retrieves rows pertaining to the given keys from the Table instance
        represented by table_handle. String keys will be UTF-8 encoded.

        Args:
            from_file:
                Description of ....
            start_settings:
                Description of ...
            electronic_settings:
                Descritpion of ...
            parallel_settings:
                Description of ...
            magnetic_settings:
                Description of ...
            ionic_settings:
                Description of ...
            hybrid_settings:
                Description of ...
            misc_settings:
                Description of ...
        """

        if not isinstance(from_file, bool):
            raise TypeError('from_file must be True or False!')
        else:
            pass
        if not isinstance(start_settings, dict):
            raise TypeError('start_settings must be a dictionary!')
        if not isinstance(parallel_settings, dict):
            raise TypeError('parallel_settings must be a dictionary!')
        if not isinstance(magnetic_settings, dict):
            raise TypeError('magnetic_settings must be a dictionary!')
        if not isinstance(ionic_settings, dict):
            raise TypeError('ionic_settings must be a dictionary!')
        if not isinstance(hybrid_settings, dict):
            raise TypeError('hybrid_settings must be a dictionary!')
        if not isinstance(misc_settings, dict):
            raise TypeError('misc_settings must be a dictionary!')

        self._settings = {}
        if not from_file:
            self._settings["start"] = start_settings or {"nwrite": 2, "istart": 1, "iniwav": 1, "icharg": None, "nelect": None, "loptics": ".FALSE.","isym": -1 , "lel": None, "lvhar": None, "rwigs": None, "lvtof": None, "nbands": None, "lwave": None}
            self._settings["electronic"] = electronic_settings or  {"prec":"Accurate" , "algo": "Normal", "encut": 800,
            "nelm": None, "nelmin": None, "gga": "PS" ,"ediff": 10E-05, "ismear": 1, "sigma": 0.2, "lasph": ".TRUE.", "lreal": "Auto", "addgrid": ".TRUE.", "maxmix": 100, "bmix": 1.5}
            self._settings["parallel"] = parallel_settings or {"ncore": "2" , "lplane": ".TRUE.",  "kpar": "2"}
            self._settings["magnetic"] = magnetic_settings
            self._settings["ionic"] = ionic_settings
            self._settings["hubbard"] = hubbard_settings
            self._settings["hybrid"] = hybrid_settings
            self._settings["misc"] = misc_settings
        else:
            pass

    def incar_from_file(self, incar_path):
        """This function...."""
        incar_dict = read_incar(incar_path) ## handles exceptions internally
        self._settings["start"] = incar_dict["start"]
        self._settings["electronic"] = incar_dict["electronic"]
        self._settings["magnetic"] = incar_dict["magnetic"]
        self._settings["ionic"] = incar_dict["ionic"]
        self._settings["hubbard"] = incar_dict["hubbard"]
        self._settings["hybrid"] = incar_dict["hybrid"]
        self._settings["misc"] = incar_dict["misc"]

    def get_settings(self, setting_type):
        """ Retrieves settings.

        Description...

        Args:

        setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
        key (str): key within setting type
        value : value corresponding to updated key
        """
        setting_types = set(["all", "start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ])
        if not isinstance(setting_type, str):
            raise TypeError('setting_type must have type string!')
        elif not setting_type in setting_types:
            raise ValueError('setting_type must be in ["all", "start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]!')
        else:
            pass

        if setting_type == "all":
            return self._settings
        else:
            return self._settings[setting_type]

    def update_settings(self, setting_type, key, value):
        """ Update a parameter in settings.

        Description...

        Args:

        setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
        key (str): key within setting type
        value : value corresponding to updated key
        """
        setting_types = set(["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ])
        if setting_type in setting_types:
            self._settings[setting_type][key] = value
        else:
            print("Error: Invalid setting_type!!")

    def write_file(self, file_path):
        """ Update a parameter in settings.

        Description...

        Args:

        setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
        key (str): key within setting type
        value : value corresponding to updated key
        """

        if not os.path.isdir(file_path):
            raise OSError('file_path does not exist!')
        else:
            pass

        path = os.path.join(os.path.dirname(file_path),"INCAR")
        write_incar(path, self._settings)
        print("INCAR file written in"+" "+ file_path)
