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

class DefaultSCFParameters(IncarSettings):
         def __init__(self, encut):
             """
             Sets default input parameters for scf ground state energy calculation

             **Args:
               encut (float): planewave energy cutoff for calculation
               name (str): name for scf setting [default="scf_settings"]

             """
             IncarSettings.__init__(self, name=name)
             self.update_settings(setting_type="electronic", "encut", encut)

class DefaultSCFUParameters(IncarSettings):
         def __init__(self, encut, ldaul, Uparam, Jparam):
             """
             Sets default input parameters for scf ground state energy calculation with +U correction

             encut (float): planewave energy cutoff for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for scf+U setting [default="DFTU_settings"]

             """

             dftu_settings = {"ldau": ".TRUE." , "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             IncarSettings.__init__(self, hubbard_settings=dftu_settings)
             self.update_settings(setting_type="electronic", "encut", encut)


class DefaultMagCLParameters(IncarSettings):
         def __init__(self, encut, magmom, ldaul, Uparam, Jparam):
             """
             Sets default input parameters for scf spin collinear calculation

             encut (flt): planewave energy cutoff for calculation
             magmom (list): list of magnetic moments for each species
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTCL_settings"]

             """

             cl_settings =  {"ispin": 2, "magmom": magmom, "saxis": None, "lsorbit": None, "lnoncollinear": None}
             dftu_settings = {"ldau": ".TRUE.", "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             IncarSettings.__init__(self, magnetic_settings=cl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("encut", encut)

class DefaultMagNCLParameters(InputParameters):
         def __init__(self, encut, spinaxis, ldaul, Uparam, Jparam):
             """
            Sets default input parameters for scf spin non-collinear calculation

             encut (flt): planewave energy cutoff for calculation
             spinaxis (ndarray): spinaxis  for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTNCL_settings"]
             """
             ncl_settings =  {"ispin": 2, "magmom": None, "saxis": spinaxis, "lsorbit": ".TRUE.", "lnoncollinear": ".TRUE."}
             dftu_settings = {"ldau": ".TRUE.", "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             InputParameters.__init__(self, magnetic_settings=ncl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("encut", encut)
# class PoscarSettings:
#     def __init__(self, ):
#         self.__structure =
#         self.__fl_path =
#         se
#
#     def from_structure(self, structure):
#         struct = structure
#
#
#     def from_file(self, fl_path):
#         struct = tructure
#
#
#
# class Potcar:
#     def __init__(self, ):
# #        d
