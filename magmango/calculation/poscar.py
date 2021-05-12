from pymatgen import Structure
from pymatgen.io.vasp.inputs import Poscar
class PoscarSettings:
   def __init__(self, structure = None, file_path = None):


        # if not isinstance(from_file, bool):
        #     raise TypeError('from_file must be True or False!')
        # else:
        #     pass
		#
		# if structure:
		#     if not isinstance(structure, pymatgen.core.structure.Structure):
	    #         raise TypeError('from_file must have type pymatgen.core.structure.Structure !')
		#     else:
		# 		pass
		# else:
		# 	pass

      self._structure = structure
      self._file_path = file_path

      if structure and file_path:
         self._file_path = self._file_path + ".vasp"
         poscar_obj = Poscar(self._structure)
         poscar_obj.write_file(self._file_path)
      else:
         pass

   def poscar_from_file(self, file_path):
      # if not os.path.isfile(file_path):
		#     raise OSError(file_path + ' ' + 'does not exist!')
		# else:
		# 	pass
      file_path = file_path + ".vasp"
      self._structure = Structure.from_file(file_path)
      self._file_path = file_path

   def get_structure(self):
      return self._structure

   def update_structure(self, structure):
      # """Func """
	   # if not isinstance(structure, pymatgen.core.structure.Structure):
	   #     raise TypeError('from_file must be True or False!')
	   # else:
	   #     pass
      self._structure = structure

   def write_file(self, file_path):
       #"""Func """
	   # if not os.path.isdir(file_path):
		#     raise OSError(path + ' ' + 'does not exist!')
		# else:
		# 	pass
		# if self._structure is None:
		# 	raise ValueError('_structure attribute cannot be None!')
		# else:
		# 	pass
		# poscar_path = os.path.join(file_path,"POSCAR.vasp")
		# self._structure.to(filename = poscar_path)
		# shutil.move(poscar_path, os.path.join(file_path,"POSCAR"))
      if self._structure:
         self._file_path  = file_path
         poscar_obj = Poscar(self._structure)
         poscar_obj.write_file(self._structure)
      else:
         pass
