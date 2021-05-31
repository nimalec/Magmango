#from magmango.in_out.in_out import write_runscript, read_runscript
from magmango.in_out.in_out import write_runscript

class RunscriptSettings:
   def __init__(self, file_path=None, run_settings=None, modules=None, exports=None, execute=None, links=None):
	#	"""set attributes
#		"""
      self._file_path = file_path
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

   def write_file(self, file_path):
	#	"""Function  """
      write_runscript(file_path, self._settings)

   def runscript_from_file(self, file_path):
	#	"""Function  """
       pass 
     # write_runscript(file_path, self._settings)
