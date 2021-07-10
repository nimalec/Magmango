import os
from pymatgen import Structure
from pymatgen.io.vasp.inputs import Poscar
from shutil import copyfile
from magmango.in_out.in_out import read_poscar, write_poscar

class PoscarSettings:
   def __init__(self, structure = None):

      self._structure = structure
      self._file_path = None

   def from_file(self, file_path):
      #copyfile(file_path,file_path + ".vasp")
      self._structure = read_poscar(file_path)
      self._file_path = file_path
      #os.rmdir(file_path + ".vasp")

   def get_structure(self):
      return self._structure

   def update_structure(self, structure):
      self._structure = structure

   def write_file(self, file_path):
      if self._structure:
         self._file_path = file_path
         write_poscar(self._file_path, self._structure)
      else:
         pass
