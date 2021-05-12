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
from magmango.in_out.in_out import read_kpoints, write_kpoints
class KpointsSettings:
   """ Summary of class.

        Longer class info.

        Attributes:
            _settings:
   """

   def __init__(self, settings = None):
        """ IncarSettings constructor method.

        Retrieves rows pertaining to the given keys from the Table instance
        represented by table_handle. String keys will be UTF-8 encoded.
        """
        self._settings = settings
        self._file_path = None
        # if not isinstance(from_file, bool):
        #     raise TypeError('from_file must be True or False!')
        # else:
        #     pass
        #
        # if not isinstance(comment, str):
        #     raise TypeError('comment must have type String!')
        # else:
        #     pass
        #
        # if not isinstance(npoints, int):
        #     raise TypeError('npoints must be an integer!')
        # else:
        #     pass
        #
        # if not isinstance(k_pts, list):
        #    raise TypeError('k_pts must be a list!')
        # elif len(k_pts) != 3:
		#    raise ValueError('k_pts must have 3 elements!')
		# else:
		#    pass
        #
		# if not from_file:
		# 	self._settings = {"comment": comment, "npoints": npoints, "kpoints": k_pts, "qpoints": q_shift}
		# else:
		# 	pass

   def kpoints_from_file(self, file_path):
      """This function...."""
      kpoints_dict = read_kpoints(file_path)
      self._settings =  kpoints_dict
      self._file_path = file_path

   def update_settings(self, key, value):
      """This function...."""
      # if not setting_type in setting_types:
      #    raise ValueError('Setting must be ["comment", "npoints", "kpoints", "qpoints"]')
      # else:
      #    pass
      self._settings[key] = value

   def write_file(self, file_path):
      """ Update a parameter in settings.

        Description...

        Args:

        setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
        key (str): key within setting type
        value : value corresponding to updated key
        """

      # if not os.path.isdir(file_path):
      #    raise OSError('Specified file_path does not exist!')
      # else:
      #    pass
      write_kpoints(file_path, self._settings)
      self._file_path = file_path
