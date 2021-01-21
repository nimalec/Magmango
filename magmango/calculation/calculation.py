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

from magmango.in_out import make_incar, make_kpoints, make_poscar, make_potcar

class Calculation:
        """ Summary of class.

        Longer class info.

        Attributes:
            _path:
            _incar:
            _kpoints:
            _poscar:
            _potcar:
            _runscript:
            _run_status:
            _run_time:
            _tot_energy:
            _mag_moment:
        """

    def __init__(self, dir_name, work_dir, incar, kpoints, poscar, potcar, runscript):
        """ Calculation constructor method.

        Retrieves rows pertaining to the given keys from the Table instance
        represented by table_handle. String keys will be UTF-8 encoded.

        Args:
            work_dir:
                Description of ....
            incar:
                Description of ...
            kpoints:
                Descritpion of ...
            poscar:
                Description of ...
            potcar:
                Description of ...
            runscript:
                Description of ...
        """
        if not isinstance(dir_name, str) and not isinstance(work_dir, str):
            raise TypeError("dir_name or work_dir must be type str!")

        path = os.path.join(work_dir, dir_name)
        if not os.path.isdir(work_dir):
            raise OSError("The work directory path" + " " work_dir "does not exist! Please choose another directory.")
        else:
            self._path = path

        if not isinstance(incar, magmango.calculation.incar.IncarSettings):
            raise TypeError("Input incar must be of type Incar!")
        else:
            self._incar = incar

        if not isinstance(kpoints, magmango.calculation.kpoints.KPointsSettings):
            raise TypeError("Input kpoints must be of type KPoints!")
        else:
            self._kpoints = kpoints

        if not isinstance(poscar, magmango.calculation.poscar.PoscarSettings):
            raise TypeError("Input poscar must be of type Poscar"!")
        else:
            self._poscar = poscar

        if not isinstance(potcar, magmango.calculation.potcar.PotcarSettings):
            raise TypeError("Input potcar must be of type Potcar!")
        else:
            self._potcar =  potcar

        if not isinstance(runscript, magmango.calculation.potcar.RunscriptSettings):
            raise TypeError("Input potcar must be of type Potcar!")
        else:
            self._potcar = potcar

        self._run_status = "not submitted"
        self._run_time = 0.0
        self._tot_energy = None
        self._mag_moment = None

    def make_calculation(self):
        """ Calculation make method. Generates specified files and directories of Calculation.

        Retrieves rows pertaining to the given keys from the Table instance
        represented by table_handle. String keys will be UTF-8 encoded.
        """

        os.mkdir(self._path)
        print("Work Directory now in: " + self._path)
        self._incar.write_file(self._path)
        self._kpoints.write_file(self._path)
        self._poscar.write_file(self._path)
        self._potcar.write_file(self._path)
        self._runscript.write_file(self._path)

    def run_calculation(self):
        # import threading, queue
        # q = queue.Queue()
        # def worker():
        #     while True:
        os.system("sbatch"+" "+self._runscript._file_name)
        self._run_status = "submitted"

    #def update_run_status(self):
    # def update_tot_energy(self):
    # def update_mag_moment(self):
    # def get_incar(self):
    # def get_kpoints(self):
    # def get_poscar(self):
    # def get_potcar(self):
    # def get_runscript(self):
