import os
from magmango.in_out.in_out import read_kpoints, write_kpoints
class KpointsSettings:

   def __init__(self, input_settings = None):

        self._settings = input_settings
        self._file_path = None

   def from_file(self, file_path):
      ## insert exception handeling
      self._file_path = file_path
      self._settings =  read_kpoints(file_path)

   def from_dict(self, input_settings):
        #add exception handeling
       self._settings = input_settings

   def get_settings(self, key):
        ## add excpetion handeling to ensure that settings exist
       return self._settings[key]

   def update_settings(self, key, value):
       self._settings[key] = value

   def remove_settings(self, key):
       if self._settings:
           self._settings.pop(key)
       else:
           pass
        ## return error message

   def write_file(self, file_path):
      # if not os.path.isdir(file_path):
      #    raise OSError('Specified file_path does not exist!')
      # else:
      #    pass
      write_kpoints(file_path, self._settings)
      self._file_path = file_path
