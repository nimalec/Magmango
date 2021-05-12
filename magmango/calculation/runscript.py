class RunscriptSettings:
   def __init__(self, run_settings, module_imports, run_exec, links):
		"""set attributes
		"""
      self._run_settings = run_settings
      self._modules = module_imports
      self._run_exec = run_exec
      self._links  = links
		#self._run_settings  =  run_settings or {"job_name": "scf_calc" , "partition": "etna" , "account": "nimalec", "qos": , "tpn": , "time":  }
		#self._modules = module_imports or ["module unload intel/2016.4.072", "module load intel/2018.5.274.par", "module load vasp_intelmpi/5.4.4.16052018"]
		#self._run_exec = run_exec or {"exe": "'vasp_std'" , "run": "time mpirun $EXE"}
		#self._links = links

	def runscript_from_file(self, file_path): 
		settings = read_runscript(settings, file_path)
		self._run_settings = settings["run_settings"]
		self._modules = settings["modules"]
		self._run_exec = settings["run_exec"]
		self._links = settings["links"]

	def get_settings(self):
		"""Function  """
		settings = {"run_settings": self._run_settings, "modules": self._modules, "run_exec": self._run_exec, "links": self._links}
		return settings

	def update_settings(self, setting_type, key, value):
		"""Function  """
		if setting_type is "run_settings":
			self._run_settings[key] = value
		elif setting_type is "modules":
			self._modules[key] = value
		elif setting_type is "run_exec":
			self._run_exec[key] = value
		elif setting_type is "links":
			self._link[key]= value

	def write_file(self, file_path):
		"""Function  """
		settings = {"run_settings": self._run_settings, "modules": self._modules, "run_settings": self._run_settings, "links": self._links}
		write_runscript(file_path, settings)
