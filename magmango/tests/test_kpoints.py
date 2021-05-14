import unittest
import os

import numpy as np
from magmango.calculation.kpoints import KpointsSettings

class KpointsSettingsTest(unittest.TestCase):
   def setUp(self):
       self.kpoints_file_path = "data/kpoints_pto"
       self.settings  = {'comment': 'kpoints_pto', 'nkpoints': 0, 'generation_style': 'Gamma', 'kpoints': ((1, 1, 1),), 'usershift': (0, 0, 0), 'kpts_weights': None, 'coord_type': None, 'labels': None, 'tet_number': 0, 'tet_weight': 0, 'tet_connections': None, '@module': 'pymatgen.io.vasp.inputs', '@class': 'Kpoints'}

   def test_from_input(self):
      kpts_sett = KpointsSettings(self.settings)
      for key in kpts_sett._settings:
          self.assertEqual(kpts_sett._settings[key], self.settings[key])

   def test_from_file(self):
      kpoints_infile = KpointsSettings()
      kpoints_infile.kpoints_from_file(self.kpoints_file_path)
      kpts_sett = kpoints_infile._settings
      for key in kpts_sett:
          if key != "comment":
             self.assertEqual(kpts_sett[key], self.settings[key])
          else:
             pass

   def test_update_settings(self):
      kpoints_infile_sett = KpointsSettings(self.settings)
      kpoints_infile_sett.update_settings("kpoints", ((2, 2, 2),))
      self.assertEqual(kpoints_infile_sett._settings["kpoints"],((2, 2, 2),) )

   def test_write_file(self):
      out_path = "data/kpoints_pto_out"
      kpoints_1 = KpointsSettings(self.settings)
      kpoints_1.write_file(out_path)
      kpoints_2 = KpointsSettings()
      kpoints_2.kpoints_from_file(out_path)
      for key in kpoints_1._settings:
         if key != "comment":
            self.assertEqual(kpoints_1._settings[key], kpoints_2._settings[key])
         else:
            pass
