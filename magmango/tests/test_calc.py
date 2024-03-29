import os
import unittest
from magmango.calculation.calculation import Calculation
from magmango.calculation.incar import IncarSettings
from magmango.calculation.poscar import PoscarSettings
from magmango.calculation.potcar import PotcarSettings
from magmango.calculation.runscript import RunscriptSettings
from magmango.calculation.kpoints import KpointsSettings

class CalculationSettingsTest(unittest.TestCase):
   def setUp(self):
       self.calc_directory = "data/calculations_pto"
       self.incar = IncarSettings()
       self.incar.from_file("data/incar_pto")
       self.poscar = PoscarSettings()
       self.poscar.from_file("data/poscar_pto.vasp")
       self.potcar = PotcarSettings()
       self.potcar.from_file("data/potcar_pto")
       runscript_file_path = "data/run_cl.sh"

       run_settings = {"job_name": "scf_cl", "qos": "regular", "nodes": 1, "constraint": "knl", "time": "24:00:00"}
       modules = ["vasp/20181030-knl"]
       exports = ["OMP_NUM_THREADS=4"]
       execute = "vasp_std"
       self.runscript =RunscriptSettings(runscript_file_path,run_settings,modules,exports,execute)
       self.kpoints = KpointsSettings()
       self.kpoints.from_file("data/kpoints_pto")

   def test_makecalc(self):
       calc = Calculation(self.incar,self.kpoints,self.poscar,self.potcar,self.runscript)
       calc.make_calculation("data/scf_calc_workdir")
