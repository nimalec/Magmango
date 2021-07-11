import os
from magmango.calculation.incar import IncarSettings
from magmango.calculation.poscar import PoscarSettings
from magmango.calculation.potcar import PotcarSettings
from magmango.calculation.runscript import RunscriptSettings
from magmango.calculation.kpoints import KpointsSettings
from shutil import copyfile

class Calculation:
    def __init__(self, incar=None, kpoints=None, poscar=None, potcar=None, runscript=None):

        self._incar = incar
        self._kpoints = kpoints
        self._poscar = poscar
        self._potcar = potcar
        self._runscript = runscript
        self._work_dir = None

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

    def make_calculation(self, work_dir):
        ## Insert exception haneling for input
        self._work_dir = work_dir
        os.mkdir(self._work_dir)
        self._incar.write_file(os.path.join(self._work_dir, "INCAR"))
        self._kpoints.write_file(os.path.join(self._work_dir, "KPOINTS"))
        self._poscar.write_file(os.path.join(self._work_dir, "POSCAR"))
        self._potcar.write_file(os.path.join(self._work_dir, "POTCAR"))
        #self._runscript.write_file(os.path.join(self._work_dir, "run.sh"))

    def run_calculation(self):
        cwd_pth = os.getcwd()
        os.chdir(self._work_dir)
        os.system("sbatch"+" "+"run.sh")
        os.chdir(cwd_pth)
        #self._run_status = "submitted"
    #def update_run_status(self):
    # def update_tot_energy(self):
    # def update_mag_moment(self):
    # def get_incar(self):
    # def get_kpoints(self):
    # def get_poscar(self):
    # def get_potcar(self):
    # def get_runscript(self):
