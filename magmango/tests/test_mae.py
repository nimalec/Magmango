import unittest
import os

import numpy as np
from magmango.mae.mae_calc import MagneticAnisotropyFlow
from magmango.calculation.calculation import Calculation
from magmango.calculation.incar import IncarSettings
from magmango.calculation.poscar import PoscarSettings
from magmango.calculation.potcar import PotcarSettings
#from magmango.calculation.runscript import RunscriptSettings
from magmango.calculation.kpoints import KpointsSettings

class MaeTest(unittest.TestCase):
   def setUp(self):
       ##runscript paramaters
     #  run_settings = {"job_name": "scf", "qos": "regular", "nodes": 1, "constraint": "knl", "time": "12:00:00"}
     #  modules = ["vasp/20181030-knl"]
     #  exports = ["OMP_NUM_THREADS=4"]
     #  links = ["ln -l ../scf_cl/WAVECAR .", "ln -l ../scf_cl/CHARGCAR ."]
     #  runscript_cl = RunscriptSettings(run_settings=run_settings,modules=modules,exports=exports, execute="vasp_std")
     #  self.runscript_ncl = RunscriptSettings(run_settings=run_settings,modules=modules,exports=exports, execute="vasp_ncl",links=links)

       ##cl_calculation settings
       incar = IncarSettings()
       incar.from_file("data/incar_pto")
       poscar = PoscarSettings()
       poscar.from_file("data/poscar_pto.vasp")
       potcar = PotcarSettings()
       potcar.from_file("data/potcar_pto")
       kpoints = KpointsSettings()
       kpoints.from_file("data/kpoints_pto")
       self.calc_cl = Calculation(incar=incar,kpoints=kpoints,poscar=poscar,potcar=potcar)

   def test_make_mae(self):
       work_dir = "data/mae_pto"
       npoints = 10
       mae=MagneticAnisotropyFlow(npoints=npoints,work_dir=work_dir,cl_calculation=self.calc_cl,symm_reduce=True)
      # mae.make_calculations()
