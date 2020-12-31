class InputParameters:
        def __init__(self, name=None, start_settings=None,
        parallel_settings=None, electronic_settings=None, magnetic_settings=None,
        ionic_settings=None, hubbard_settings=None, hybrid_settings=None, misc_settings=None):

            """
            Sets standard input parameters for a VASP calculation.

            **Args:

            name (str): name of input paramter settings [default = "input_param"]
            start_settings (dict): start settings for calculation [default = {"nwrite": 2, "istart": 1, "iniwav": 1,
             "icharg": None, "nelect": None, "lorbit": 11,
              "nedos": 1000, "loptics": False, "lelf": None, "lvhar": None, "rwigs": None, "lvtof": None}]
            parallel_settings (dict): parallization settings for calculation [default = "flnm": "scf_run.sh", "job_name": "scf_std", "machine": "nano" ,
             "partition": "etna", "nodes": 4,"ppn": 24,
              "max_time": 24.0, "ncore": 8, "kpar": 2, "exec": "vasp_std"}]
            electronic_settings (dict): electronic structure settings for calculation [default = {"algo": "Normal", "encut": 800,
            "nelm": 200, "nelmin": 4, "ediff": 10E-5, "ismear": 1,
            "sigma": 0.2,"lasph": True, "lreal": "Auto", "addgrid": True, "maxmix": 100, "bmix": 1.5}]
            ionic_settings (dict): ionic settings for calculation, used for relaxations and structure optimization [default=None]
            magnetic_settings (dict): magnetic structure settings for calculation [default=None]
            hybrid_settings (dict): hybrid/hse settings for accurate band calculation [default=None]
            hubbard_settings (dict): hubbard calculation settings for localized d/f orbital predicitons [default=None]
            misc_settings_settings (dict): miscalaneous settings for calculation, can be any VASP setting [default=None]

            """

            self._name = name or "input_param"
            self._start_settings = start_settings or {"NWRITE": 2, "ISTART": 1, "INIWAV": 1,
             "ICHARG": None, "NELECT": None, "LORBIT": 11,
              "NEDOS": 1000, "LOPTICS": ".FALSE.","ISYM": -1 , "LELF": None, "LVHAR": None, "RWIGS": None, "LVTOF": None, "NBANDS": None, "LWAVE": None}
            self._parallel_settings = parallel_settings or {"flnm": "run_scf.sh", "job_name": "scf_std", "machine": "nano" ,
             "partition": "etna", "nodes": 4,"ppn": 24,
              "max_time": "24:00:00", "NCORE": 8, "KPAR": 2, "exec": "vasp_std"}
            self._electronic_settings = electronic_settings or  {"PREC":"Accurate" , "ALGO": "Normal", "ENCUT": 800,
            "NELM": None, "NELMIN": None, "GGA": "PS" ,"EDIFF": 10E-05, "ISMEAR": 1,
            "SIGMA": 0.2, "LASPH": ".TRUE.", "LREAL": "Auto", "ADDGRID": ".TRUE.", "MAXMIX": 100, "BMIX": 1.5}
            self._ionic_settings = ionic_settings
            self._magnetic_settings = magnetic_settings
            self._hybrid_settings = hybrid_settings
            self._hubbard_settings = hubbard_settings
            self._misc_settings = misc_settings

        def get_input_settings(self):

            """Getter function for Vasp Input paramters"""

            input_settings = {"name": name,
             "start": self._start_settings, "parallel": self._parallel_settings ,
            "electronic": self._electronic_settings, "magnetic": self._magnetic_settings,
            "hybrid": self._hybrid_settings, "hubbard": self._hubbard_settings, "misc_setting": self._misc_settings}
            return input_settings

        def update_start_settings(self, key, value):

            """
            Update a parameter in start settings.

            **Args:

            key (str): key in start settings
            value : value corresponding to updated key
            """

            if key in self._start_settings:
                self._start_settings[key] = value
            else:
                print("key does not exist!! keys include: {charge_option, prec, encut, nstep, epsilon, pseudo, n_elect.structure, smear, sigma, isym}")

        def update_parallel_settings(self, key, value):

            """
            Update a parameter in parallel settings.

            **Args:

            key (str): key in parallel settings
            value : value corresponding to updated key
            """

            if key in self._parallel_settings:
                self._parallel_settings[key] = value
            else:
                print("key does not exist!! keys include: {flnm , job_name , machine, partition, nodes  ,ppn, max_time , ncore,  kpar}")

        def update_electronic_settings(self, key, value):
            """
            Update a parameter in electronic settings.

            **Args:

            key (str): key in electronic settings
            value : value corresponding to updated key
            """

            if key in self._electronic_settings:
                self._electronic_settings[key] = value
            else:
                print("key does not exist!! keys include: {prec_level, algo, encut , nelm,nelmin, ediff, sigma, lasph, lreal, addgrid, bmaxmix, bmix}")

        def update_ionic_settings(self, key, value):
            """
            Update a parameter in ionic settings.

            **Args:

            key (str): key in ionic settings
            value : value corresponding to updated key
            """
            if self._ionic_settings:
              if key in self._ionic_settings:
                self._ionic_settings[key] = value
              else:
                print("key does not exist!! keys include: {ediff ,nsw, ibrion ,isif, isym, nblock,  kblock}")
            else:
              print("magnetic settings not present!")

        def update_magnetic_settings(self, key, value):

            """
            Update a parameter in magnetic settings.

            **Args:

            key (str): key in magnetic settings
            value : value corresponding to updated key
            """

            if self._magnetic_settings:
              if key in self._magnetic_settings:
                self._magnetic_settings[key] = value
              else:
                print("key does not exist!! keys include: {ispin, magmom, nupdown, saxis, lsorbit,noncollinear}")
            else:
              print("magnetic settings not present!")


        def update_hubbard_settings(self, key, value):

            """
            Update a parameter in Hubbard settings.

            **Args:

            key (str): key in Hubbard settings
            value : value corresponding to updated key
            """

            if self._hubbard_settings:
              if key in self._hubbard_settings:
                  self._hubbard_settings[key] = value
              else:
                  print("key does not exist!! keys include: {ldau, ldatype, ldaul, dlauu, ldauj, lmaxmix}")
            else:
               print("hybrid settings not present!")


class DefaultOptimizationParameters(InputParameters):
        def __init__(self, encut, name="relax_settings"):
            """
            Sets default input parameters for optimization

            **Args:

            encut (float): planewave energy cutoff for calculation
            name (str): name for relaxation setting [default="relax_settings"]

            """

            ionic = {"EDIFF": 1E-17, "NSW": 20, "IBRION": 2,"ISIF": 2, "ISYM": -1, "NBLOCK": 1,  "KBLOCK": 20}
            InputParameters.__init__(self, ionic_settings=ionic, name=name)
            self.update_electronic_sttings("ENCUT", encut)



class DefaultSCFParameters(InputParameters):
         def __init__(self, encut, name="scf_settings"):
             """
             Sets default input parameters for scf ground state energy calculation

             **Args:
               encut (float): planewave energy cutoff for calculation
               name (str): name for scf setting [default="scf_settings"]

             """
             InputParameters.__init__(self, name=name)
             self.update_electronic_settings("ENCUT", encut)

class DefaultSCFUParameters(InputParameters):
         def __init__(self, encut, ldaul, Uparam, Jparam, name="DFTU_settings"):
             """
             Sets default input parameters for scf ground state energy calculation with +U correction

             encut (float): planewave energy cutoff for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for scf+U setting [default="DFTU_settings"]

             """

             dftu_settings = {"LDAU": ".TRUE." , "LDAUU": Uparam, "LDATYPE": 2, "LADAUL": ldaul, "LDAUJ": Jparam , "LMAXMIX": 4}
             InputParameters.__init__(self, name=name, hubbard_settings=dftu_settings)
             self.update_electronic_settings("ENCUT", encut)


class DefaultMagCLParameters(InputParameters):
         def __init__(self, encut, magmom, ldaul, Uparam, Jparam, name="DFTCL_settings"):
             """
             Sets default input parameters for scf spin collinear calculation

             encut (flt): planewave energy cutoff for calculation
             magmom (list): list of magnetic moments for each species
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTCL_settings"]

             """

             cl_settings =  {"ISPIN": 2, "MAGMOM": magmom, "SAXIS": None, "LSORBIT": None, "LNONCOLLINEAR": None}
             dftu_settings = {"LDAU": ".TRUE.", "LDAUU": Uparam, "LDATYPE": 2, "LDAUL": ldaul, "LDAUJ": Jparam , "LMAXMIMX": 4}
             InputParameters.__init__(self, name=name, magnetic_settings=cl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("encut", encut)

class DefaultMagNCLParameters(InputParameters):
         def __init__(self, encut, spinaxis, ldaul, Uparam, Jparam, name='DFTCL_settings'):
             """
            Sets default input parameters for scf spin non-collinear calculation

             encut (flt): planewave energy cutoff for calculation
             spinaxis (ndarray): spinaxis  for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTNCL_settings"]
             """
             ncl_settings =  {"ISPIN": 2, "MAGMOM": None, "SAXIS": spinaxis, "LSORBIT": ".TRUE.", "LNONCOLLINEAR": ".TRUE."}
             dftu_settings = {"LDAU": ".TRUE.", "LDAUU": Uparam, "LDATYPE": 2, "LDAUL": ldaul, "LDAUJ": Jparam , "LMAXMIX": 4}
             InputParameters.__init__(self, name=name, magnetic_settings=ncl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("ENCUT", encut)
