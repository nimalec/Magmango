import unittest
import os

import numpy as np
from pymatgen import Structure
from magmango.calculation.potcar import PotcarSettings
#
# class PotcarSettingsTest(unittest.TestCase):
#    def setUp(self):
#        self.potcar_file_path = "data/potcar_pto"
#        #self.structure = Structure.from_file(self.poscar_file_path)
#
#    def test_from_input(self):
#       #poscar_sett = PoscarSettings(self.structure, self.poscar_file_path)
#       #self.assertEqual(poscar_sett._structure, self.structure)
#
#    # def test_from_file(self):
#    #    poscar_infile_sett = PoscarSettings()
#    #    poscar_infile_sett.poscar_from_file(self.poscar_file_path)
#    #    struct = poscar_infile_sett._structure
#    #    self.assertEqual(struct, self.structure)
#
#    def test_update_settings(self):
#       poscar_infile_sett = PoscarSettings()
#       poscar_infile_sett.poscar_from_file(self.poscar_file_path)
#       poscar_sett = poscar_infile_sett._structure
