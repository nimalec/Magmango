class PoscarSettings:
	def __init__(self, from_file = False, structure = None):
		"""Func """

        if not isinstance(from_file, bool):
            raise TypeError('from_file must be True or False!')
        else:
            pass

		if structure:
		    if not isinstance(structure, pymatgen.core.structure.Structure):
	            raise TypeError('from_file must have type pymatgen.core.structure.Structure !')
			else:
				pass
		else:
			pass

	    self._structure = structure

    def poscar_from_file(self, file_path):
		"""Func """
		if not os.path.isfile(file_path):
		    raise OSError(file_path + ' ' + 'does not exist!')
		else:
			pass
		self._structure = pymatgen.Structure.from_file(file_path)

	def get_structure(self):
		"""Func """
		return self._structure

	def update_structure(self, structure):
		"""Func """
	    if not isinstance(structure, pymatgen.core.structure.Structure):
	        raise TypeError('from_file must be True or False!')
	    else:
	        pass
		self._structure = structure

	def write_file(self, file_path):
		"""Func """
		if not os.path.isdir(file_path):
		    raise OSError(path + ' ' + 'does not exist!')
		else:
			pass
		if self._structure is None:
			raise ValueError('_structure attribute cannot be None!')
		else:
			pass
		poscar_path = os.path.join(file_path,"POSCAR.vasp")
		self._structure.to(filename = poscar_path)
		shutil.move(poscar_path, os.path.join(file_path,"POSCAR"))
