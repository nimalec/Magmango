import os
from pymatgen import Structure
from pymatgen.io.vasp.inputs import Poscar
from shutil import copyfile
from magmango.in_out.in_out import read_poscar, write_poscar

class PoscarSettings:
   def __init__(self, structure = None, file_path = None):

      self._structure = structure
      self._file_path = file_path

      # if structure and file_path:
      #    path = self._file_path + ".vasp"
      #    poscar_obj = Poscar(self._structure)
      #    poscar_obj.write_file(path)
      # else:
      #    pass

   def poscar_from_file(self, file_path):
      # if not os.path.isfile(file_path):
		#     raise OSError(file_path + ' ' + 'does not exist!')
		# else:
		# 	pass
      copyfile(file_path,file_path + ".vasp")
      self._structure = read_poscar(file_path)
      self._file_path = file_path
      #os.rmdir(file_path + ".vasp")

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
         write_poscar(self._file_path, self._structure)
      else:
         pass
