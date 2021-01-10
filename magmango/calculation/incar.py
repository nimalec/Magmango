import os
from pymatgen.io.vasp.inputs import Incar
from magmango.in_out.in_out import read_incar, write_incar

class IncarSettings:
        def __init__(self, from_file = False, start_settings=None, electronic_settings=None, parallel_settings=None, magnetic_settings=None, ionic_settings=None, hubbard_settings=None, hybrid_settings=None, misc_settings=None):
               self._settings = {}
               if from_file is False:
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
            incar_dict = read_incar(incar_path)
            self._settings["start"] = incar_dict["start"]
            self._settings["electronic"] = incar_dict["electronic"]
            self._settings["magnetic"] = incar_dict["magnetic"]
            self._settings["ionic"] = incar_dict["ionic"]
            self._settings["hubbard"] = incar_dict["hubbard"]
            self._settings["hybrid"] = incar_dict["hybrid"]
            self._settings["misc"] = incar_dict["misc"]

        def get_settings(self):
            return self._settings

        def get_start_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["start"]

        def get_electronic_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["electronic"]

        def get_magnetic_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["magnetic"]

        def get_ionic_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["ionic"]

        def get_hubbard_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["hubbard"]

        def get_hybrid_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["hybrid"]

        def get_misc_settings(self):
            """Getter function for Vasp Input paramters"""
            return self._settings["misc"]

        def update_settings(self, setting_type, key, value):

            """
            Update a parameter in settings.

            **Args:

            setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
            key (str): key within setting type
            value : value corresponding to updated key
            """
            setting_types = set(["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ])
            if setting_type in setting_types:
                self._settings[setting_type][key] = value
            else:
                print("Error: Invalid setting_type!!")

        def write_file(self, fl_path):
            if os.path.isdir(fl_path) == False:
                print("Directory path doesen't exist!!")
                break
            else:
                path = os.path.join(os.path.dirname(fl_path),"INCAR")
                write_incar(path, self._settings)
                print("INCAR file written in"+" "+ fl_path)
