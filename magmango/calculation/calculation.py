"""Calculation module handles the setup and execution of a VASP calculation.

The Calculation object .

  Typical usage example:

  import os
  dir = os.path.join(os.getcwd(),"vasp_calc_directory")
  incar_obj = Incar()
  kpoints_obj = KPoints()
  poscar_obj = Poscar()
  potcar_obj = Potcar()
  calc = Calculation(work_dir = dir, incar =  incar_obj, kpoints = kpoints_obj, poscar_obj = poscar_obj, potcar = potcar_obj)
  calc.make_calculation()
  calc.run_calculation()
  calc.update_run_status()
  calc.update_tot_energy()
"""

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

        if not isinstance(incar, magmango.calculation.incar.Incar):
            raise TypeError("Input incar must be of type Incar!")
        else:
            self._incar =  incar

        if not isinstance(kpoints, magmango.calculation.kpoints.KPoints):
            raise TypeError("Input kpoints must be of type KPoints!")
        else:
            self._kpoints =  KPoints

        if not isinstance(poscar, magmango.calculation.incar.Poscar):
            raise TypeError("Input poscar must be of type Poscar"!")
        else:
            self._poscar =  poscar

        if not isinstance(potcar, magmango.calculation.incar.Potcar):
            raise TypeError("Input potcar must be of type Potcar!")
        else:
            self._potcar =  potcar

        self._run_status = "not submitted"
        self._run_time = 0.0
        self._tot_energy = None
        self._mag_moment = None

    def make_calculation(self):
        os.mkdir(self._path)
        print("Work Directory now in: " + self._workdir)
        make_incar_h(self._workdir, self._input_settings)

        if struct_path:
            copyfile(struct_path, self._workdir+"/"+"POSCAR")
            self._struct_path = struct_path
        else:
            #make_poscar_h(self._workdir, self._structure, [4], ["Mn"])
            pass

        if run_script_path:
            copyfile(run_script_path, self._workdir+"/"+"run_scf.sh")
        else:
            make_runscript_h(self._workdir, self._input_settings)

        if k_points_path:
            copyfile(k_points_path, self._workdir+"/"+"KPOINTS")
            self._kpoint_path = k_points_path
        else:
            make_kpoints_h(self._workdir, self._kmesh)

        if potcar_path:
            copyfile(potcar_path, self._workdir+"/"+"POTCAR")
            self._potcar_path = potcar_path
        else:
            make_potcar_h(self._workdir, self._pseudo_par)
    #
    # def run_calculation(self):
    #
    # def update_run_status(self):
    #
    # def update_tot_energy(self):
    #
    # def update_mag_moment(self):
    #
    # def get_incar(self):
    #
    # def get_kpoints(self):
    #
    # def get_poscar(self):
    #
    # def get_potcar(self):
    #
    # def get_runscript(self):



    # def make_calculation(self, struct_path=None, run_script_path=None, k_points_path=None, potcar_path=None):
    #  """
    #  Sets up VASP input files and directory
    #  **Args:
    #  struct_path (str): path (including POSCAR file) of availible POSCAR in external directory
    #  run_script_path (str): path (including runscript file) of availible runscript in external directory
    #  k_points_path (str): path (including kpoints file) of availible kpoints file in external directory
    #  """
    #
    #      os.mkdir(self._workdir)
    #      print("Work Directory now in: " + self._workdir)
    #      make_incar_h(self._workdir, self._input_settings)
    #
    #      if struct_path:
    #         copyfile(struct_path, self._workdir+"/"+"POSCAR")
    #         self._struct_path = struct_path
    #      else:
    #          #make_poscar_h(self._workdir, self._structure, [4], ["Mn"])
    #          pass
    #
    #      if run_script_path:
    #          copyfile(run_script_path, self._workdir+"/"+"run_scf.sh")
    #      else:
    #          make_runscript_h(self._workdir, self._input_settings)
    #
    #      if k_points_path:
    #          copyfile(k_points_path, self._workdir+"/"+"KPOINTS")
    #          self._kpoint_path = k_points_path
    #      else:
    #          make_kpoints_h(self._workdir, self._kmesh)
    #
    #      if potcar_path:
    #          copyfile(potcar_path, self._workdir+"/"+"POTCAR")
    #          self._potcar_path = potcar_path
    #      else:
    #          make_potcar_h(self._workdir, self._pseudo_par)
    #
    # def run_calculation(self):
    #     # os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
    #     # self._run_status = "submitted"
    #
    # def update_run_status(self):
    #      #os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
    #      #self._run_status = "submitted"
    #
    # def update_total_energy(self):
    #      #os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
    #      #self._run_status = "submitted"
    #
    # def update_mag_moment(self):
    #     # os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
    #     # self._run_status = "submitted"
