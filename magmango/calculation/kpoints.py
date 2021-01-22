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

class KPointsSettings:
    """ Summary of class.

        Longer class info.

        Attributes:
            _settings:
        """

	def __init__(self, from_file = False, comment = "Gamma", npoints = 0, k_pts = [2,2,2], q_shift = [0,0,0]):
        """ IncarSettings constructor method.

        Retrieves rows pertaining to the given keys from the Table instance
        represented by table_handle. String keys will be UTF-8 encoded.

        Args:
            from_file:
                Description of ....
            comment:
                Description of ...
            npoints:
                Descritpion of ...
            k_pts:
                Description of ...
            q_shift:
                Description of ...
        """

        if not isinstance(from_file, bool):
            raise TypeError('from_file must be True or False!')
        else:
            pass

        if not isinstance(comment, str):
            raise TypeError('comment must have type String!')
        else:
            pass

        if not isinstance(npoints, int):
            raise TypeError('npoints must be an integer!')
        else:
            pass

        if not isinstance(k_pts, list):
            raise TypeError('k_pts must be a list!')
        elif len(k_pts) != 3:
			raise ValueError('k_pts must have 3 elements!')
		else:
			pass

		if not from_file:
			self._settings = {"comment": comment, "npoints": npoints, "kpoints": k_pts, "qpoints": q_shift}
		else:
			pass

	def kpoints_from_file(self, file_path):
		"""This function...."""
		kpoints_dict = read_kpoints(file_path)
	    self._settings["comment"] = kpoints_dict["comment"]
		self._settings["npoints"] = kpoints_dict["npoints"]
		self._settings["kpoints"] = kpoints_dict["kpoints"]
		self._settings["qpoints"] = kpoints_dict["qpoints""]

	def get_settings(self):
		"""This function...."""
		return self._settings

	def update_settings(self, key, value):
		"""This function...."""
		setting_types = set(["comment", "npoints", "kpoints", "qpoints"])
        if not setting_type in setting_types:
            raise ValueError('Setting must be ["comment", "npoints", "kpoints", "qpoints"]')
        else:
            pass
        self._settings[key] = value

    def write_file(self, file_path):
        """ Update a parameter in settings.

        Description...

        Args:

        setting_type (str): setting type among ["start", "electronic", "magnetic", "ionic", "hubbard", "hybrid", "misc" ]
        key (str): key within setting type
        value : value corresponding to updated key
        """

        if not os.path.isdir(file_path):
            raise OSError('Specified file_path does not exist!')
        else:
            pass

        path = os.path.join(os.path.dirname(file_path),"KPOINTS")
        write_kpoints(path, self._settings)
        print("KPOINTS file written in"+" "+ file_path)
