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
   #    self._settings = read_runscript(file_path
   def get_settings(self, key):
	#	"""Function  """
      return  self._settings[key]

   def update_settings(self, key, value):
	#	"""Function  """
      self._settings[key] = value

   def write_file(self, work_dir):
	#	"""Function  """
      file_path = work_dir +"/run.sh"
      write_runscript(file_path, self._settings)
