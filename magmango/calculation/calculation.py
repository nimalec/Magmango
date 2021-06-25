"""The calculation module handles the setup and execution of a VASP calculation.

The calculation module initializes and supports the modification of the settings for a VASP calculation. This module
also manages the job submissions and input/output of a single VASP calculation.

 Typical usage example:

  import os
  from magmango.calculation.calculation import Calculation
  dir = os.path.join(os.getcwd(),"vasp_calc_directory")
  incar_obj = Incar()
  kpoints_obj = KPoints()
  poscar_obj = Poscar()
  potcar_obj = Potcar()
  calc = Calculation(work_dir = dir, incar =  incar_obj, kpoints = kpoints_obj, poscar_obj = poscar_obj, potcar = potcar_obj)
  calc.make_calculation()
  calc.run_calculation()
"""
import os
from magmango.calculation.incar import IncarSettings
from magmango.calculation.poscar import PoscarSettings
from magmango.calculation.potcar import PotcarSettings
from magmango.calculation.runscript import RunscriptSettings
from magmango.calculation.kpoints import KpointsSettings
from shutil import copyfile

class Calculation:

    def __init__(self, work_dir=None, incar=None, kpoints=None, poscar=None, potcar_path=None, runscript=None):
        #
        # if not isinstance(work_dir, str):
        #     raise TypeError("dir_name or work_dir must be type str!")
        #
        # if not os.path.isdir(work_dir):
        #     raise OSError("The work directory path" + " " + work_dir + "does not exist! Please choose another directory.")
        # else:
        #     self._work_dir = work_dir
        self._work_dir = work_dir
        self._incar = incar
        self._kpoints = kpoints
        self._poscar = poscar
        self._potcar_path = potcar_path
        self._runscript = runscript

        # if not isinstance(incar, magmango.calculation.incar.IncarSettings):
        #     raise TypeError("Input incar must be of type Incar!")
        # else:
        #     self._incar = incar
        #
        # if not isinstance(kpoints, magmango.calculation.kpoints.KPointsSettings):
        #     raise TypeError("Input kpoints must be of type KPoints!")
        # else:
        #     self._kpoints = kpoints
        #
        # if not isinstance(poscar, magmango.calculation.poscar.PoscarSettings):
        #     raise TypeError("Input poscar must be of type Poscar"!")
        # else:
        #     self._poscar = poscar
        #
        # if not isinstance(potcar, magmango.calculation.potcar.PotcarSettings):
        #     raise TypeError("Input potcar must be of type Potcar!")
        # else:
        #     self._potcar =  potcar
        #
        # if not isinstance(runscript, magmango.calculation.runscript.RunscriptSettings):
        #     raise TypeError("Input runscript must have type RunscriptSettings!")
        # else:
        #     self._runscript = runscript

    def make_calculation(self, work_dir=None):
        if work_dir:
            self._work_dir = work_dir
        else:
            pass
    #    print("Work Directory now in: " + self._work_dir)
        os.mkdir(self._work_dir)
        potcar  = PotcarSettings()
        self._incar.write_file(self._work_dir+"/INCAR")
        self._kpoints.write_file(self._work_dir+"/KPOINTS")
        self._poscar.write_file(self._work_dir+"/POSCAR")
        copyfile(self._potcar_path, self._work_dir+"/POTCAR")
        self._runscript.write_file(self._work_dir+"/run.sh")

    def run_calculation(self):
        # import threading, queue
        # q = queue.Queue()
        # def worker():
        #     while True
        cwd_pth = os.getcwd()
        os.chdir(self._work_dir)
        os.system("sbatch"+" "+"run.sh")
        os.chdir(cwd_pth)
        #self._run_status = "submitted"

    # def calculation_from_dir(self):
    #     incar =
    #     incar.
    #     poscar =
    #     kpoints =
    #     runscript =




    #def update_run_status(self):
    # def update_tot_energy(self):
    # def update_mag_moment(self):
    # def get_incar(self):
    # def get_kpoints(self):
    # def get_poscar(self):
    # def get_potcar(self):
    # def get_runscript(self):
