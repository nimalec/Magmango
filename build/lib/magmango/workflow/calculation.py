class SCFCalculation():
     def __init__(self, workdir, pseudo_par, kgrid, structure=None, name="scf_calc", encut=600, input_parameters=None):
         """
         Sets standard input parameters for a VASP calculation.

         **Args:

         workdir (str): workdirectory for scf calculation
         pseudo_par (dict): pseudopotential parameters {"directory":  , "flavor":[]}
         kgrid (list): kgrid for calculation
         structure (Structure): structure for scf calculation
         name (str): calculation name [default="scf_calc"]
         encut (float): planewave energy cutoff
         input_paramters (InputParameters): input paramters for scf calculation [defualt=DefaultSCFParameters(encut)]

         """

         self._name  = name
         self._workdir = workdir
         self._structure = structure
         self._kmesh = kgrid
         self._pseudo_par = pseudo_par
         self._input_settings = input_parameters or DefaultSCFParameters(encut=encut)
         self._struct_path = None
         self._potcar_path = None
         self._kpoint_path = None
         self._struct_path = None
         self._run_status = "unstarted"
         self._jobid = None
         self._cputime = None
         self._tot_energy = None
         self._fermi = None
# add Gamma line between 0 and k points

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
         os.system("sbatch"+" "+self._input_settings._parallel_settings["flnm"])
         self._run_status = "submitted"

     def get_run_time(self):
        fl_nm = self._workdir + 'OUTCAR'
        isfile = os.path.isfile(fl_nm)
        if isfile == False:
            print("OUTCAR file not present! try to re-run the calculation.")
            pass
        else:
           with open(fl_nm, 'r') as f:
             for line in f.readlines():
               if 'Total CPU time used (sec):' in line:
                   time_str = line
        return float(time_str[49:57])

     # def update_run_status(self):
     #     current_status_ = self._run_status
     #     path_ = self._workdir
     #
     #     def check_slurm_file_h(path):  ## checks if slurm file is present
     #         slurm_files = []
     #         slurm_file = None
     #         slurm_files = [i for i in os.listdir(path) if os.path.isfile(os.path.join(path,i)) and "slurm" in i]
     #         str_len = len(slurm_files)
     #
     #         if str_len == 0:
     #             pass
     #         else:
     #             slurm_file = slurm_files[str_len-1]
     #         return slurm_file
     #
     #     def check_outcar_file_h(path): ## checks if job is done, conditioned on there being outcar
     #         assert os.path.exists("OUTCAR"), "OUTCAR file not present in directory"
     #         isdone = False
     #         with open(fl_nm, 'r') as f:
     #             for line in f.readlines():
     #                 if "General timing" in line:
     #                     isdone = True
     #                     break
     #        return isdone
     #
     #     def is_submitted_h(path, current_status): gets job status
     #         if current_status == "submitted" or current_status == "running" or current_status == "finished":
     #             status = True
     #         else:
     #             status = False
     #        return status
     #
     #     def is_running_h(path, current_status):
     #
     #         if current_status == "running":
     #             job_status = True
     #         elif current_status == "finished" or current_status == "not_submitted":
     #             job_status = False
     #         else:
     #             if check_slurm_file_h(path) == None:
     #                 status = "submitted"
     #             else:
     #                 status = "running"
     #
     #     def fetch_run_id_h(path):
     #         flnm = check_slurm_file_h(path)
     #         assert flnm != None, "job not yet submitted, ID does not exist!"
     #         return str(flnm[6:14])
     #
     #  slurm_file_status = check_slurm_file_h(path_)
     #  outcar_status = check_outcar_file_h(path_)
     #  if self._run_status == "not_submited":
     #     pass
     #  elif self._run_status == "submited" and slurm_file_status == None:
     #       pass
     #  elif self._run_status == "submited" and slurm_file_status != None:
     #       self._run_status == "running"
     #       self._jobid = fetch_run_id_h(path_)
     #  elif self._run_status == "running" and check_outcar_file_h(path) == True:
     #       self._cputime = self.get_run_time()

def get_total_energy(self):
   energ_list = []
   fl_nm = self._workdir + 'OUTCAR'
   isfile = os.path.isfile(fl_nm)
   if isfile == False:
     print("OUTCAR file not present! try to re-run the calculation.")
      pass
   else:
     with open(fl_nm, 'r') as f:
       for line in f.readlines():
         if 'TOTEN' in line:
           energ_list.append(line)
   tot_energ = energ_list[len(energ_list)-1]
   return float(tot_energ[30:40])

#def get_fermi(self):
#    fl_nm = self._workdir + 'OUTCAR'
#    isfile = os.path.isfile(fl_nm)
#    if isfile == False:
#        print("OUTCAR file not present! try to re-run the calculation.")
#        pass
#    else:
#       with open(fl_nm, 'r') as f:
#         for line in f.readlines():
#           if 'E-fermi :' in line:
#               fermi_str = line
#   return float(fermi_str[12:18])
