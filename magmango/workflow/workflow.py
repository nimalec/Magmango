class MagenticAnisotropySphereFlow:

    def __init__(self, workdir, npoints, kgrid, nbands, nodes, ppn, ref_orient, ldaul, magmom, Uparam, Jparam, encut, potcar_path, struct_path, name ="mae_calc", time_cl="12:00:00", time_ncl="01:40:00", ismear=-5, sigma=0.2, nelect=None, cl_dir=None):

        """
        Computes the MCAE sphere for a defined structure.
        """

        self._workdir = workdir
        self._npoints = npoints
        self._name = name
        self._structure_path = struct_path
        self._potcar_path =  potcar_path
        self._reference_orientation = ref_orient
        self._collinear_calc = None
        self._cl_dir = cl_dir
        self._collinear_calc = None
        if self._cl_dir:
           pass
        else:
           self._cl_dir = self._workdir+"/"+"scf_cl"
        self._non_collinear_calcs = []

        def generate_spin_axes_h(npoints):
            golden_angle = (3 - np.sqrt(5)) * np.pi
            theta = golden_angle * np.arange(npoints)
            Sz = np.linspace(1/npoints-1, 1-1/npoints, npoints)
            radius = np.sqrt(1 - Sz * Sz)
            Sy = radius * np.sin(theta)
            Sx = radius * np.cos(theta)
            saxes_temp = np.round(np.array([Sx,Sy,Sz]).T, 4)
            saxes = saxes_temp.tolist()
            return saxes

        self._saxes = generate_spin_axes_h(self._npoints)
        self._saxes.append(self._reference_orientation)

        def set_calculations_h(cl_calc, cl_dir, saxes, workdir, nodes, ppn, time_cl, time_ncl, ismear, sigma, kgrid, encut, magmom, ldaul, Uparam, Jparam, nbands, nelect):
            if cl_calc:
                collinear_calc = None
            else:
               cl_settings = DefaultMagCLParameters(encut=encut, magmom=magmom, ldaul=ldaul, Uparam=Uparam, Jparam=Jparam)
               cl_settings.update_parallel_settings("flnm ", "run_cl.sh")
               cl_settings.update_parallel_settings("job_name", "cl_run")
               cl_settings.update_parallel_settings("nodes", nodes)
               cl_settings.update_parallel_settings("ppn", ppn)
               cl_settings.update_parallel_settings("max_time", time_cl)
               cl_settings.update_electronic_settings("ISMEAR", ismear)
               cl_settings.update_electronic_settings("SIGMA", sigma)
               cl_settings.update_electronic_settings("EDIFF", 1.0E-6)
               if nelect:
                    cl_settings.update_start_settings("NELECT", nelect)
               else:
                 pass

               collinear_calc = SCFCalculation(cl_dir, pseudo_par=None, kgrid=kgrid, name="scf_cl", input_parameters=cl_settings)
            itr = 0
            non_collinear_calcs = []
            for spin_axis in saxes:
                ncl_settings = DefaultMagNCLParameters(encut=encut, spinaxis=spin_axis, ldaul=ldaul, Uparam=Uparam, Jparam=Jparam)
                ncl_settings.update_start_settings("NBANDS", nbands)
                ncl_settings.update_start_settings("LWAVE", ".FALSE.")
                ncl_settings.update_parallel_settings("flnm ", "run_ncl.sh")
                ncl_settings.update_parallel_settings("job_name", "ncl_run_"+str(itr))
                ncl_settings.update_parallel_settings("nodes", nodes)
                ncl_settings.update_parallel_settings("ppn", ppn)
                ncl_settings.update_parallel_settings("max_time", time_ncl)
                ncl_settings.update_parallel_settings("KPAR", None)
                ncl_settings.update_parallel_settings("exec", "vasp_ncl")
                ncl_settings.update_electronic_settings("ISMEAR", ismear)
                ncl_settings.update_electronic_settings("SIGMA", sigma)
                ncl_settings.update_electronic_settings("EDIFF", 1.0E-4)
                if nelect:
                     ncl_settings.update_start_settings("NELECT", nelect)
                else:
                  pass

                ncl_dir = workdir+"/"+"scf_ncl"+"/"+"scf_ncl_"+str(itr)
                ncl_calc = SCFCalculation(ncl_dir, pseudo_par=None, kgrid=kgrid, name="scf_ncl_"+str(itr), input_parameters=ncl_settings)
                non_collinear_calcs.append(ncl_calc)
                itr += 1
            return [collinear_calc, non_collinear_calcs]


        [self._collinear_calc, self._non_collinear_calcs] = set_calculations_h(self._collinear_calc, self._cl_dir, self._saxes, self._workdir, nodes, ppn, time_cl, time_ncl, ismear, sigma, kgrid, encut, magmom, ldaul, Uparam, Jparam, nbands, nelect)

    def make_calculations(self):
        os.mkdir(self._workdir)
        self._collinear_calc.make_calculation(struct_path=self._structure_path, potcar_path=self._potcar_path)
        os.mkdir(self._workdir+"/"+"scf_ncl")
        for calc in self._non_collinear_calcs: calc.make_calculation(struct_path=self._structure_path, potcar_path=self._potcar_path)

    # def run_cl_calculation(self):
    #     self._collinear_calc.run_calculation()

    # def run_ncl_calculations(self):
    #     if self._collinear_calc._run_status != "finished":
    #         self._collinear_calc.update_run_status()
    #         self.run_ncl_calculations()
    #     else:
    #        cl_wvcr = self._collinear_calc._workdir+"/"+"WAVECAR"
    #        cl_chcr = self._collinear_calc._workdir+"/"+"CHGCAR"
    #        for calc in self._non_collinear_calcs:
    #            copyfile(cl_wvcr, calc._workdir)
    #            copyfile(cl_chcr, calc._workdir)
    #            calc.run_calculation()

    # def retrieve_mae_data(self):
    #     mae_data = []
    #     for calc in self._non_collinear_calcs:
    #         if calc._run_status != "finished":
    #            self._collinear_calc.update_run_status()
    #            self.retrieve_mae_data()
    #         else:
    #             spin = calc._input_settings._magnetic_settings["SAXIS"]
    #             energ = calc.get_total_energy()
    #             os.system("rm -r "+calc._workdir+"/"+"WAVECAR")
    #             os.system("rm -r "+calc._workdir+"/"+"CHGCAR")
    #             mae_data.append([spin[0], spin[1], spin[2], energ])
    #     write_maefile(mae_data)



	# class BandCalculation(Calculation):
	#         def __init__(self, charge_option, wfn,  istart, work_dir, structure, k_dim, pseudo_lists, k_path = None, nbnds = None, lorbit = True , smear = False , sigma = 0.01, isym = 0):
	#             """OUTPUT List:
	#                Istart:
	#                   - 0 = begin from scratch
	#                   - 1 = continue job w energy cutoff
	#                   - 2 = continue, restart w constant basis
	#                ICHARG:
	#                    -   0 (computes from initial wfn)
	#                    - = 1 extrapolate from old positions, reads from CHCAR
	#
	#               LORBIT =  10 ==> not decomposed DOS
	#               LORBIT = 11 ==> decomposed DOS
	#             """
	#             super(SCFCalculation, self).__init__(self, work_dir, structure, k_dim, pseudo_lists)
	#
	#             self._bands_options_ = [k_path, nbnds, charge_option]
	#
	#             self._scf_setting_ = [charge_option, prec, encut, nstep, epsilon, pseudo, n_elect.structure, smear, sigma, isym]
	#             self._ionic_  = []
	#             self._dos_ = [lorbit, dos_pts]
	#             self._bands_options_ = [k_path, nbnds, charge_option]
	#             self._magentic_options_ = [spin_polar, magmom, spin_orbit]
	#             self._hubbard_ = [dft_u, dudarev, ldaul, u_param, j_param, lda_mix]
	#             self._hybrid_ = [hf_calc, hf_fft, hf_max, hf_scren, hf_xch]
	#             self._rho_decomp_ = [par_chg, en_rng, ref_ef, over_k, over_bnd]
	#
	#         def make_calculation(self):
	#                 """Sets input parameters for ground state calculation and makes files for calculation"""
	#             temp_dir = os.getcwd()
	#             os.mkdir(self._workdir)
	#             print("Work Directory now in:" + self._workdir)
	#             os.chdir(self._workdir)
	#             make_incar(self._pseudos, self._input)
	#             make_poscar(self. )
	#             make_potcar(self._pseudos)
	#             make_runscript(self._parallelization)
	#             os.chdir(temp_dir)
	#


	#class PhononCalculation(Calculation):
	#    def __init__(self, input):
	#        """""
	#        Sets input for relaxation calculation
	#        """
	#        self._algorithm_ =
	#        self._bands_ =
	#        self._rho_   =
	#        self._spin_orbit_ =
	#        self._charge_compensate_ =
	#        self._spin_polar_  =
	#        super(RelaxationCalculation, self).__init__(self, work_dir, structure, k_dim, pseudo_list )



	#class EFieldCalculation(Calculation):


	# class SerialComputeFlow():
	# """
	# Sets up a serial set of compuations which iterate over a desired degree of freedom
	# (i.e. strain, dopant type/position, substrate)
	# """
	#     def __init__(self, workdir, dir_prefix="scf_calc",name="series_calculation" ):
	#     """
	#     Sets up a serial set of compuations which iterate over a desired degree of freedom
	#     (i.e. strain, dopant type/position, substrate)
	#     """
	#         self._workdir = workdir
	#         self._dir_prefix = dir_prefix
	#         self._dir_names = []
	#         self._ncalc = 0
	#         self._param_list = []
	#         self._calc_list = []

	    # def setup_calc_series(self):
	    #     os.chdir(self._workdir)
	    #     param_list = self._param_list
	    #     for i in range(self._ncalc):
	    #         dir_name = self._dir_prefix + param_list[i]
	    #         self._dir_names.append(dirname)
	    #         os.mkdir(dirname)

	    # def make_series(self):
	    #     if len(self._calc_list) == 0:
	    #         pass
	    #     else:
	    #         for calc in self._calc_list:
	    #             calc.make_calculation()
	    # def add_calc(self, param):
	    #
	    # def remove_calc(self):
	    #
	    # def run_status(self):
	    #
	    #
	    #

	# class ConvergeTest(SerialComputeFlow):
	#     """
	#     Sets up a serial set of compuations which iterate over a desired degree of freedom
	#     (i.e. strain, dopant type/position, substrate)
	#     """

	    # """OUTPUT List:
	    #    - CHG: chrge density, lattice vecctors, coords.
	    #    - DOSCAR: DOS
	    #    - EIGENVAL: bands
	    #    - IBZKPT: BZ
	    #    - LOCPOT: local potential
	    #    - OSZICAR: information at each nstep
	    #    - OUTCAR: outputfile, main
	    #    - PARCHG: partial charage density
	    #    - PROCAR: site projected wfn cahracter
	    #    - WAVECAR: wavefunctions and coefficients, eigenvalues
	    # """
	    #
	    #     self._algorithm_ =
	    #     self._bands_ =
	    #     self._rho_   =
	    #     self._spin_orbit_ =
	    #     self._charge_compensate_ =
	    #     self._spin_polar_  =
	    #     self._dos_ =
	    #     super(SerialComputeFlow, self).__init__(inialized inputs, )

	#class TimingTest(SerialComputeFlow):#
	#    """
	    # Sets up a serial set of compuations which iterate over a desired degree of freedom
	    # (i.e. strain, dopant type/position, substrate)
	    # """
	    #     self._algorithm_ =
	    #     self._bands_ =
	    #     self._rho_   =
	    #     self._spin_orbit_ =
	    #     self._charge_compensate_ =
	    #     self._spin_polar_  =
	    #     self._dos_ =
	    #     super(SerialComputeFlow, self).__init__(inialized inputs, )
