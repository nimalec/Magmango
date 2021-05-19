#from magmango.in_out.in_out import write_runscript, read_runscript
from magmango.in_out.in_out import write_runscript 

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
   #
   # def runscript_from_file(self, file_path):
   #    self._settings = read_runscript(file_path)

   def get_settings(self, setting_type):
	#	"""Function  """
      return self._runscript_settings[setting_type]

   def update_settings(self, setting_type, value):
	#	"""Function  """
      self._settings[setting_type] = value

   def write_file(self, file_path):
	#	"""Function  """
      write_runscript(file_path, self._run_settings)
