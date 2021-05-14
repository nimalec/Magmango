import unittest
import os

import numpy as np
from magmango.calculation.incar import IncarSettings

class MaeTest(unittest.TestCase):
   def setUp(self):
       ## Include PTO in-files: kpts, poscar, potcar, incar
       self.incar_file_path = "data/incar_pto"
       ##setup calculations

   def test_mae_pts(self):
       ### test cases for generating mae points (use 10 for now)

      incar_sett = IncarSettings()
      for key in start:
          self.assertEqual(incar_sett._settings["start"][key], start[key])
      for key in electrosnic:
          self.assertEqual(incar_sett._settings["electronic"][key], electronic[key])
      for key in parallel:
          self.assertEqual(incar_sett._settings["parallel"][key], parallel[key])

   def test_mae_run(self):
       ###  test run settings for mae calculation
      incar_infile_sett = IncarSettings()

   def test_mae_symm_reduce(self):
       ###  test run settings for mae calculation
      incar_infile_sett = MaeSettings()

   def test_mae_extraction(self):
       ###  test run settings for mae calculation
      incar_infile_sett = MaeSettings()

   def test_total_energy(self):
       ###  test run settings for mae calculation
      incar_infile_sett = MaeSettings()

   def test_mag_moment(self):
       ###  test run settings for mae calculation
      incar_infile_sett = MaeSettings()
