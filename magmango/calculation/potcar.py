import os
from shutil import copyfile
from pymatgen.io.vasp.inputs import Potcar

class PotcarSettings:
    def __init__(self, species = None, functionals = None):
        if species and functionals:
            self._potcar = Potcar(species, functionals)
            self._species = species
            self._functionals = functionals
        else:
            self._potcar = None
            self._species = None
            self._functionals = None

        self._file_path = None

    def from_file(self, file_path):
        #add exception handeling
        self._file_path = file_path
        self._potcar = Potcar().from_file(file_path)
        self._species = self._potcar.symbols
        self._functionals = self._potcar.FUNCTIONAL_CHOICES

    def write_file(self, path):
        if self._potcar:
            self._potcar.write_file(path)
        else:
            pass

    # def potcar_from_file(self, file_path):
	# 	# """Function... """
	# 	# if not os.path.isfile(file_path):
	# 	#     raise OSError(file_path+ ' ' + 'does not exist!')
	# 	# else:
	#     #     pass
    #
    # #    copyfile(in_path, out_path)
    #     self._outfile_path = file_path

    # def write_file(self, file_path):
	# 	# """Function... """
    #     #
	# 	# if not os.path.isdir(file_path):
	# 	#     raise OSError(file_path+ ' ' + 'does not exist!')
	# 	# else:
	#     #     pass
    #     if self._file_paths:
    #         os_command = "cat "
    #         for file in self._file_paths:
    #             os_command += file + " "
    #             os_command = os_command + " >> " + file_path
    #         os.system(os_command)
    #     else:
    #         pass
		# if self._outfile_path:
		# 	shutil.copyfile(self._outfile_path, file_path)
		# else:
		# 	potcar_path = os.path.join(file_path, "POTCAR")
		# 	with open(potcar_path, 'w') as out_file:
		# 	    for file in self._file_paths:
		# 			with open(file) as in_file:
		# 				for line in in_file:
		# 					out_file.write(line)
