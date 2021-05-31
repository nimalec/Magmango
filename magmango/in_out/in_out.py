import numpy as np
import os
import shutil
from pymatgen.io.vasp.inputs import Incar, Kpoints, Poscar
from pymatgen import Structure, Lattice

def write_runscript(file_path, settings):
    """
    Generates runscript provided run settings for VASP calculation.

    **Args:

    workdir (str):
    run_settings (RunscriptSettings):
    """

    f=open(file_path, "w")
    f.write("#!/bin/bash" + "\n\n")
    for key in settings["run_settings"]:
        f.write("#SBATCH --"+key+"="+str(settings["run_settings"][key])+"\n")
    f.write("\n \n")

    if settings["modules"]:
        for item in settings["modules"]:
            f.write("module load "+str(item)+"\n")
        f.write("\n \n")
    else:
       pass

    f.write("\n \n")
    f.write("EXE= '"+settings["execute"]+"'")
    f.write("\n \n")

    if settings["links"]:
        for item in settings["links"]:
            f.write(item+"\n")
        f.write("\n \n")
    else:
       pass

    if settings["exports"]:
        for item in settings["exports"]:
            f.write("export "+item+"\n")
        f.write("\n \n")
    else:
       pass

    f.write("time mpirun $EXE \n\n")
    f.write("exit 0\n\n")
    f.close()
    print("Runscript file printed in: " + file_path)

def write_incar(file_path, settings):
    """
    Generates VASP INCAR file for VASP calculation.

    **Args:

    workdir (str):
    input_settings (InputParameters):
    """

    #file_path = os.path.join(work_dir,"INCAR")
    incar = Incar().from_dict(settings)
    incar.write_file(file_path)
    #print("INCAR file printed in: " + file_path)

def write_potcar(work_dir, pseudo_dir, species):
    """
    Generates VASP POTCAR file for VASP calculation.

    **Args:

    workdir (str):
    pseudo_par (dict):
    """
    paths = []
    for pot in species:
        pseudo_path = os.path.join(work_dir +"/"+ pot, "POTCAR")
        paths.append(pseudo_path)
    files = " ".join(paths)
    os.system("cat"+files+">> POTCAR")
    shutil.move("POTCAR", work_dir+"/POTCAR")
    ##print("POTCAR file printed in:" work_dir+"/POTCAR")

def write_poscar(work_dir, struct):
    file_path = os.path.join(work_dir)
    poscar = Poscar(struct)
    poscar.write_file(file_path)
    ##print("POSCAR file printed in: " work_dir+"/POSCAR")

def write_kpoints(file_path, kpts_dict):
    kpt_obj = Kpoints().from_dict(kpts_dict)
    kpt_obj.write_file(file_path)
    ##print("KPOINTS file printed in: " work_dir+"/KPOINTS")

#def read_runscript(runscript_path):

def read_incar(incar_path):
    incar = Incar.from_file(incar_path)
    return incar.as_dict()

def read_poscar(poscar_path):
    lattice = Lattice.cubic(4.2)
    struct_temp = Structure(lattice, ["Cs", "Cl"],[[0, 0, 0], [0.5, 0.5, 0.5]])
    poscar = Poscar(struct_temp).from_file(poscar_path,check_for_POTCAR=False)
    struct = poscar.structure
    return struct

def read_kpoints(kpt_path):
    kpts = Kpoints.from_file(kpt_path)
    kpt_dict = kpts.as_dict()
    return kpt_dict
