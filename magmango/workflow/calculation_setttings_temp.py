import os
import pymatgen
from pymatgen.io.vasp.inputs import Incar

class Incar:
        def __init__(self, incar_path = None, start_settings=None, electronic_settings=None, magnetic_settings=None,
        ionic_settings=None, hubbard_settings=None, hybrid_settings=None, misc_settings=None):

            if incar_path != None:
               self.__incar__path = incar_path
               self.__settings = self.settings_from_file(self.__incar__path)
               self.__start_settings = self.__settings["start"]
               self.__electronic_settings  = self.__settings["electronic"]
               self.__magnetic_settings = self.__settings["magnetic"]
               self.__ionic_settings  = self.__settings["ionic"]
               self.__hubbard_settings  = self.__settings["hubbard"]
               self.__hybrid_settings =  self.__settings["hybrid"]


            elif start_settings == None and electronic_settings == None and magnetic_settings==None and ionic_settings==None  and hubbard_settings==None and misc_settings==None:
                print("INCAR or settings doesen't exist file doesen't exist")
                break
            else:
                self.__start_settings = start_settings or {"NWRITE": 2, "ISTART": 1, "INIWAV": 1, "ICHARG": None, "NELECT": None, "LORBIT": 11,
                              "NEDOS": 1000, "LOPTICS": ".FALSE.","ISYM": -1 , "LELF": None, "LVHAR": None, "RWIGS": None, "LVTOF": None, "NBANDS": None, "LWAVE": None}
                self.__electronic_settings =  electronic_settings or  {"PREC":"Accurate" , "ALGO": "Normal", "ENCUT": 800,
                "NELM": None, "NELMIN": None, "GGA": "PS" ,"EDIFF": 10E-05, "ISMEAR": 1, "SIGMA": 0.2, "LASPH": ".TRUE.", "LREAL": "Auto", "ADDGRID": ".TRUE.", "MAXMIX": 100, "BMIX": 1.5}
                self.__magnetic_settings = magnetic_settings
                self.__ionic_settings = ionic_settings
                self.__hubbard_settings = hubbard_settings
                self.__hybrid_settings = hybrid_settings
                self.__misc_settings = misc_settings


           def settings_from_file(incar_path):
               incar = Incar.from_file(incar_path)
               dict = incar.as_dict()
               dict.pop("@class")
               dict.pop("@module")
               start_settings = set([" ", ""])
               electronic_settings = set([" ", ""])
               magnetic_settings = set([" ", ""])
               ionic_settings = set([" ", ""])
               hubbard_settings = set([" ", ""])
               hybrid_settings = set([" ", ""])

               settings = {}
               start_dict  = {}
               electronic_dict  = {}
               magnetic_dict  = {}
               ionic_dict  = {}
               hubbard_dict  = {}
               hybrid_dict  = {}
               misc_dict = {}

               for key, value in dict.items():
                   key = key.lower()
                   if key in start__settings:
                       start_dict[key] = value
                   elif key in electronic__settings:
                       electronic_dict[key] = value
                   elif key in magnetic__settings:
                       magnetic_dict[key] = value
                   elif key in ionic__settings :
                       ionic_dict[key] = value
                   elif key in hubbard_settings:
                       hubbard_dict[key] = value
                   elif key in hybrid_settings :
                       hybrid_dict[key] = value
                   else:
                       misc_dict[key] = value

                settings["start"] = start_dict
                settings["electronic"] = electronic_dict
                settings["magnetic"] = magnetic_dict
                settings["ionic"] = ionic_dict
                settings["hubbard"] = hubbard_dict
                settings["hybrid"] = hybrid_dict
                settings["misc"] = misc_dict
                return settings








       def get_input_settings(self):
            """Getter function for Vasp Input paramters"""
            return self.__settings


       def update_start_settings(self, key, value):

            """
            Update a parameter in start settings.

            **Args:

            key (str): key in start settings
            value : value corresponding to updated key
            """

            if key in self._start_settings:
                self._start_settings[key] = value
            else:
                print("key does not exist!! keys include: {charge_option, prec, encut, nstep, epsilon, pseudo, n_elect.structure, smear, sigma, isym}")

        def update_parallel_settings(self, key, value):

            """
            Update a parameter in parallel settings.

            **Args:

            key (str): key in parallel settings
            value : value corresponding to updated key
            """

            if key in self._parallel_settings:
                self._parallel_settings[key] = value
            else:
                print("key does not exist!! keys include: {flnm , job_name , machine, partition, nodes  ,ppn, max_time , ncore,  kpar}")

        def update_electronic_settings(self, key, value):
            """
            Update a parameter in electronic settings.

            **Args:

            key (str): key in electronic settings
            value : value corresponding to updated key
            """

            if key in self._electronic_settings:
                self._electronic_settings[key] = value
            else:
                print("key does not exist!! keys include: {prec_level, algo, encut , nelm,nelmin, ediff, sigma, lasph, lreal, addgrid, bmaxmix, bmix}")

        def update_ionic_settings(self, key, value):
            """
            Update a parameter in ionic settings.

            **Args:

            key (str): key in ionic settings
            value : value corresponding to updated key
            """
            if self._ionic_settings:
              if key in self._ionic_settings:
                self._ionic_settings[key] = value
              else:
                print("key does not exist!! keys include: {ediff ,nsw, ibrion ,isif, isym, nblock,  kblock}")
            else:
              print("magnetic settings not present!")

        def update_magnetic_settings(self, key, value):

            """
            Update a parameter in magnetic settings.

            **Args:

            key (str): key in magnetic settings
            value : value corresponding to updated key
            """

            if self._magnetic_settings:
              if key in self._magnetic_settings:
                self._magnetic_settings[key] = value
              else:
                print("key does not exist!! keys include: {ispin, magmom, nupdown, saxis, lsorbit,noncollinear}")
            else:
              print("magnetic settings not present!")


        def update_hubbard_settings(self, key, value):

            """
            Update a parameter in Hubbard settings.

            **Args:

            key (str): key in Hubbard settings
            value : value corresponding to updated key
            """

            if self._hubbard_settings:
              if key in self._hubbard_settings:
                  self._hubbard_settings[key] = value
              else:
                  print("key does not exist!! keys include: {ldau, ldatype, ldaul, dlauu, ldauj, lmaxmix}")
            else:
               print("hybrid settings not present!")

        def write_incar(self, work_dir, calc_nm = "system"):
            """
            Writes incar file in directory
            """
            from in_out import make_incar_h
            path = workdir + "/INCAR"
            make_incar_h(path, self.__settings, name=calc_nm)
            self.__out_dir = dir

class Poscar:
    def __init__(self, ):
        self.__structure =
        self.__fl_path =
        se

    def from_structure(self, structure):
        struct = structure


    def from_file(self, fl_path):
        struct = tructure



class Potcar:
    def __init__(self, ):
#        d
