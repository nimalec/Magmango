import unittest
import os

import numpy as np
from magmango.calculation.incar import IncarSettings

class MaeTest(unittest.TestCase):
   def setUp(self):
       ##runscript paramaters
       run_settings = {"job_name": "scf", "qos": "regular", "nodes": 1, "constraint": "knl", "time": "12:00:00"}
       modules = ["vasp/20181030-knl"]
       exports = ["OMP_NUM_THREADS=4"]
       links = ["ln -l ../scf_cl/WAVECAR .", "ln -l ../scf_cl/CHARGCAR ."]
       runscript_cl = RunscriptSettings(run_settings=run_settings,modules=modules,exports=exports, execute="vasp_std")
       self.runscript_ncl = RunscriptSettings(run_settings=run_settings,modules=modules,exports=exports, execute="vasp_ncl",links=links)

       ##cl_calculation settings
       incar = IncarSettings()
       incar.incar_from_file("data/incar_pto")
       poscar = PoscarSettings()
       poscar.poscar_from_file("data/poscar_pto.vasp")
       potcar_path = potcar._outfile_path
       kpoints = KpointsSettings()
       kpoints.kpoints_from_file("data/kpoints_pto")
       self.calc_cl = Calculation(self.calc_directory,incar,kpoints,poscar,potcar_path,runscript_cl)

   def test_make_mae(self):
       work_dir = "data/mae_pto"
       npoints = 10
       nbands = 700
       mae=MagneticAnisotropyFlow(work_dir=work_dir,npoints=npoints,cl_calculation=self.calc_cl,nbands=nbands,runscript_ncl=self.runscript_ncl)
       mae.make_calculations()  
