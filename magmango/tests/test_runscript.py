import unittest
import os

import numpy as np
from magmango.calculation.runscript import RunscriptSettings

class RunscriptSettingsTest(unittest.TestCase):
   def setUp(self):
      self.runscript_file_path = "data/run_cl.sh"
      self.run_settings = {"job_name": "scf_cl", "qos": "regular", "nodes": 1, "constraint": "knl", "time": "24:00:00"}
      self.modules = ["vasp/20181030-knl"]
      self.exports = ["OMP_NUM_THREADS=4"]
      self.execute = "vasp_ncl"
      #self.execute = {"vasp_executable": "vasp_std", "run_line": "time srun -n16 -c16 --cpu_bind=cores $EXE"}

   #def test_from_file(self):
   #    incar_infile_sett = IncarSettings()
   #    incar_infile_sett.incar_from_file(self.incar_file_path)
   #    incar_sett = incar_infile_sett._settings

   def test_write_file(self):
      run_infile_sett = RunscriptSettings(run_settings=run_settings,modules=modules,exports=exports, execute=execute)
      run_infile_sett.write_file("data/run_cl_test.sh")
