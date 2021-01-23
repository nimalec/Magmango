"""The incar module handles the INCAR parameters belonging to a VASP calculation.

The incar module allows for the initialization, modification, and managing of input/output of settings for VASP INCAR file parameters.
The incar module allows for settings to be extracted from a provided INCAR file or to be inputed manually.
The setting types are partitioned into: start, electronic, parallel, magnetic, ionic, hubbard, hybrid, misc.

 Typical usage example:

  from magmango.calculation.incar import IncarSettings
  incar_path = "/home/directory/INCAR"
  incar_obj = Incar(from_file = True)
  incar_obj.incar_from_file(incar_path)
  settings = incar_obj.get_settings(setting_type = "all")
"""
import os
import shutil
class PotcarSettings:
	def __init__(self, from_file = False, file_paths = None):

        self._outfile_path = None
		if not from_file and file_paths is None:
			raise ValueError('Must specify either a POSCAR file path or POSCAR file_paths')
		elif file_paths:
			for path in file_paths:
				if not os.path.isfile(path):
					raise OSError(path + ' ' + 'does not exist!')
				else:
					continue
		else:
			pass

		self._file_paths = file_paths

	def from_file(self, file_path):
		"""Function... """
		if not os.path.isfile(file_path):
		    raise OSError(file_path+ ' ' + 'does not exist!')
		else:
	        pass
		self._outfile_path = file_path

	def write_file(self, file_path):
		"""Function... """

		if not os.path.isdir(file_path):
		    raise OSError(file_path+ ' ' + 'does not exist!')
		else:
	        pass

		if self._outfile_path:
			shutil.copyfile(self._outfile_path, file_path)
		else:
			potcar_path = os.path.join(file_path, "POTCAR")
			with open(potcar_path, 'w') as out_file:
			    for file in self._file_paths:
					with open(file) as in_file:
						for line in in_file:
							out_file.write(line)
