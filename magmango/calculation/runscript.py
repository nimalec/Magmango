class RunscriptSettings:
   def __init__(self, run_settings=None, modules=None, exports=None, execute=None):
	#	"""set attributes
#		"""
      self._run_settings = run_settings
      self._modules = modules
      self._exports = exports
      self._execute  = execute
      self._links  = links
      self._settings = {"run_settings": self._run_settings, "modules": self._modules, "exports": self._exports, "execute": self._execute, "links": self._links}
		#self._run_settings  =  run_settings or {"job_name": "scf_calc" , "partition": "etna" , "account": "nimalec", "qos": , "tpn": , "time":  }
		#self._modules = module_imports or ["module unload intel/2016.4.072", "module load intel/2018.5.274.par", "module load vasp_intelmpi/5.4.4.16052018"]
		#self._run_exec = run_exec or {"exe": "'vasp_std'" , "run": "time mpirun $EXE"}
		#self._links = links

   def runscript_from_file(self, file_path):
      self._settings = read_runscript(file_path)

   def get_settings(self, setting_type):
	#	"""Function  """
      return self._runscript_settings[setting_type]

   def update_settings(self, setting_type, value):
	#	"""Function  """
      self._settings[setting_type] = value

   def write_file(self, file_path):
	#	"""Function  """
      write_runscript(file_path, self._run_settings)
