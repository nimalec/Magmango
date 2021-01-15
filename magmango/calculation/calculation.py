class Calculation:
    def __init__(self, dir_name="scf_calc", path=os.getcwd(), incar, kpoints, poscar, potcar, runscript):
        """
        This is a docstring


        """
        self._path = os.path.join(path, dir_name)
        self._incar = incar
        self._kpoints = kpoint
        self._poscar = poscar
        self._potcar = potcar
        self._runfile = runscript
        self._run_status = "not submitted"
        self._run_time = 0.0
        self._tot_energy = None
        self._mag_moment = None

    def make_calculation(self, struct_path=None, run_script_path=None, k_points_path=None, potcar_path=None):
     """
     Sets up VASP input files and directory
     **Args:
     struct_path (str): path (including POSCAR file) of availible POSCAR in external directory
     run_script_path (str): path (including runscript file) of availible runscript in external directory
     k_points_path (str): path (including kpoints file) of availible kpoints file in external directory
     """

         os.mkdir(self._workdir)
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

         if os.path.exists("__pycache__") is True:
            os.system("rm -r __pycache__")

    def run_calculation(self):
        # os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
        # self._run_status = "submitted"

    def update_run_status(self):
         #os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
         #self._run_status = "submitted"

    def update_total_energy(self):
         #os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
         #self._run_status = "submitted"

    def update_mag_moment(self):
        # os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
        # self._run_status = "submitted"
