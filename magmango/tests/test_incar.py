import unittest
import os

import numpy as np
from magmango.calculation.incar import IncarSettings

class IncarSettingsTest(unittest.TestCase):
   def setUp(self):
       self.incar_file_path = "data/incar_pto"
       self.settings = {}
       self.settings["start"] = {"nwrite": "2", "istart": "1", "iniwav": "1", "nelect": "1185"}
       self.settings["electronic"] =  {"prec":"Accurate" , "algo": "Normal", "encut": "750", "nelm": "200", "gga": "Ps" ,"ediff": "1e-06", "lasph": "True", "lreal": "Auto"}
       self.settings["parallel"] = {"ncore": "2" ,  "kpar": "2"}
       self.settings["magnetic"] = {"ispin": "2"}
       self.settings["ionic"] = {"ediffg": "-0.02", "ibrion": "1"}
       self.settings["hubbard"] = {"ldau": "True", "ldatype": "2", "ldau": "True",  "lmaxmix": "4"}
       self.settings["hybrid"] = {}
       self.settings["misc"] = {}

   def test_default_settings(self):
      start = {"nwrite": 2, "istart": 1, "iniwav": 1, "icharg": None, "nelect": None, "loptics": ".FALSE.","isym": -1 , "lel": None, "lvhar": None, "rwigs": None, "lvtof": None, "nbands": None, "lwave": None}
      electronic =  {"prec": "Accurate" , "algo": "Normal", "encut": 800,
      "nelm": None, "nelmin": None, "gga": "PS" ,"ediff": 10E-05, "ismear": 1, "sigma": 0.2, "lasph": ".TRUE.", "lreal": "Auto", "addgrid": ".TRUE.", "maxmix": 100, "bmix": 1.5}
      parallel = {"ncore": "2" , "lplane": ".TRUE.",  "kpar": "2"}

      incar_sett = IncarSettings()
      for key in start:
          self.assertEqual(incar_sett._settings["start"][key], start[key])
      for key in electronic:
          self.assertEqual(incar_sett._settings["electronic"][key], electronic[key])
      for key in parallel:
          self.assertEqual(incar_sett._settings["parallel"][key], parallel[key])

   def test_from_file(self):
      incar_infile_sett = IncarSettings()
      incar_infile_sett.incar_from_file(self.incar_file_path)
      incar_sett = incar_infile_sett._settings

      for key_1 in self.settings:
         for key_2 in self.settings[key_1]:
            self.assertEqual(self.settings[key_1][key_2], incar_sett[key_1][key_2])

 ## def test_write_file()

 ## def test_update_method()
