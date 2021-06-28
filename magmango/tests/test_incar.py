import unittest
import os

import numpy as np
from magmango.calculation.incar import IncarSettings

class IncarSettingsTest(unittest.TestCase):
   def setUp(self):
       self.incar_file_path = "data/incar_pto"
       self.settings = {'NWRITE': 2, 'ISTART': 1, 'INIWAV': 1, 'NBANDS': 768, 'NELECT': 1185, 'NCORE': 8, 'KPAR': 4, 'PREC': 'Accurate', 'ALGO': 'Normal', 'ENCUT': 750, 'NELM': 200, 'EDIFF': 1e-06, 'GGA': 'Ps', 'LASPH': True, 'LREAL': 'Auto', 'EDIFFG': -0.02, 'IBRION': 1, 'SIGMA': 0.01, 'ISPIN': 2, 'LDAU': True, 'LDATYPE': 2, 'LDAUL': [-1, -1, -1, 2], 'LDAUU': [0, 0, 0, 4], 'LDAUJ': [0, 0, 0, 0], 'LMAXMIX': 4}

   #def test_default_settings(self):
      # start = {"nwrite": 2, "istart": 1, "iniwav": 1, "icharg": None, "nelect": None, "loptics": ".FALSE.","isym": -1 , "lel": None, "lvhar": None, "rwigs": None, "lvtof": None, "nbands": None, "lwave": None}
      # electronic =  {"prec": "Accurate" , "algo": "Normal", "encut": 800,
      # "nelm": None, "nelmin": None, "gga": "PS" ,"ediff": 10E-05, "ismear": 1, "sigma": 0.2, "lasph": ".TRUE.", "lreal": "Auto", "addgrid": ".TRUE.", "maxmix": 100, "bmix": 1.5}
      # parallel = {"ncore": "2" , "lplane": ".TRUE.",  "kpar": "2"}

   def test_from_file(self):
       incar_infile_sett = IncarSettings()
       incar_infile_sett.from_file(self.incar_file_path)
       incar_sett = incar_infile_sett._settings
       for key in self.settings:
           self.assertEqual(self.settings[key], incar_sett[key])

   def test_get_method(self):
       ##add get method for magmom (and maybe also U paramaters)
       incar = IncarSettings(self.settings)
       out = incar.get_settings("KPAR")
       self.assertEqual(out, 4)

   def test_update_method(self):
       ## add update method for magmom
       incar = IncarSettings(self.settings)
       incar.update_settings("KPAR", 8)
       self.assertEqual(incar._settings["KPAR"], 8)

   def test_write_file(self):
       out_path = "data/incar_pto_out"
       incar_1 = IncarSettings(self.settings)
       incar_1.write_file(out_path)
       incar_2 =  IncarSettings()
       incar_2.from_file(out_path)
       for key in self.settings:
           self.assertEqual(incar_1._settings[key], incar_2._settings[key])
