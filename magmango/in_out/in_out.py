import numpy as np
import os
import shutil
from pymatgen.io.vasp.inputs import Incar


def write_runscript(work_dir, input_settings):
    """
    Generates runscript provided run settings for VASP calculation.

    **Args:

    workdir (str):
    input_settings (InputParameters):
    """
    run_settings = input_settings._parallel_settings
    fl_nm = run_settings["flnm"]
    if os.path.exists(work_dir+fl_nm) is True:
        os.remove(work_dir+fl_nm)

    f=open(fl_nm, "w")
    f.write("#!/bin/bash" + "\n\n")
    f.write("#SBATCH --job-name="+run_settings["job_name"]+"\n")
    f.write("#SBATCH --partition="+run_settings["partition"]+"\n")
    f.write("#SBATCH --account="+run_settings["machine"]+"\n")
    f.write("#SBATCH --qos=normal \n")
    f.write("#SBATCH --nodes="+str(run_settings["nodes"])+"\n")
    f.write("#SBATCH --ntasks-per-node=" + str(run_settings["ppn"])+"\n")
    f.write("#SBATCH --time="+run_settings["max_time"]+"\n\n")
    f.write("module unload intel/2016.4.072\n")
    f.write("module load intel/2018.5.274.par\n")
    f.write("module load vasp_intelmpi/5.4.4.16052018\n\n")
    f.write("EXE="+"'"+run_settings["exec"]+"'"+"\n\n")
    f.write("time mpirun $EXE\n\n")
    f.write("exit 0\n\n")
    f.close()
    shutil.move(fl_nm, work_dir)
    if os.path.exists("__pycache__") is True:
       os.system("rm -r __pycache__")

def write_incar(work_dir, input_settings,name="system"):
    """
    Generates VASP INCAR file for VASP calculation.

    **Args:

    workdir (str):
    input_settings (InputParameters):
    """

    fl_nm = "INCAR"
    pth = os.path.join(os.path.dirname(work_dir),"INCAR")
    if os.path.exists(pth) is True:
        os.remove(pth)
    else:
        pass
    f=open(pth, "w")

    f.write("SYSTEM=   "+name+"\n")
    f.write("start parameters"+"\n")

    for key, value in input_settings["start"].items():
        if value:
            f.write(key.upper()+"=   "+str(value)+"\n")
        else:
            pass

    f.write("\n")
    f.write("parallel"+"\n")
    if input_settings["parallel"]["ncore"] == None:
       pass
    else:
       f.write("NCORE"+"=   "+str(input_settings["parallel"]["ncore"])+"\n")
    if input_settings["parallel"]["kpar"] == None:
       pass
    else:
       f.write("KPAR"+"=   "+str(input_settings["parallel"]["kpar"])+"\n\n")
    f.write("\n")
    f.write("electronic"+"\n")
    for key, value in input_settings["electronic"].items():
        if value:
            f.write(key.upper()+"=   "+str(value)+"\n")
        else:
            pass
    f.write("\n")

    if input_settings["ionic"]:
        f.write("ionic"+"\n")
        for key, value in input_settings["ionic"].items():
            if value:
                f.write(key.upper()+"=   "+str(value)+"\n")
            else:
                pass
        f.write("\n")

    if input_settings["magnetic"]:
        f.write("magnetic"+"\n")
        for key, value in input_settings["magnetic"].items():
            if value:
                if key == "saxis":
                    saxis = value
                    saxis_line = str(saxis[0])+" "+str(saxis[1])+" "+str(saxis[2])
                    f.write(key.upper()+"=   "+saxis_line+"\n")
                elif key.lower() == "magmom":
                   line = " "
                   magmom = value
                   for i in magmom:
                     line += str(i) + " "
                   #magmom_line = str(magmom[0])+" "+str(magmom[1])+" "+str(magmom[2])
                   f.write(key.upper()+"=   "+line+"\n")
                else:
                   f.write(key.upper()+"=   "+str(value)+"\n")
            else:
                pass

    # if input_settings._hybrid_settings:
    #     f.write("hybrid"+"\n")
    #     for key in input_settings._hybrid_settings:
    #         if input_settings._hybrid_settings[key]:
    #             f.write(key+"="+str(input_settings._hybrid_settings[key])+"\n")
    #         else:
    #             pass
    #     f.write("\n")

    f.write("\n")
    if input_settings["hubbard"]:
        f.write("hubbard"+"\n")
        for key, value in input_settings["hubbard"].items():
            if value:
                if key.lower() == "ldaul" or key.lower() == "ldauj" or key.lower()=="ldauu":
                    line = ""
                    for i in value:
                        line += str(i)+ " "
                    f.write(key.upper()+"=   "+line+"\n")
                else:
                    f.write(key.upper()+"=   "+str(value)+"\n")
            else:
                pass
        f.write("\n")

    if input_settings["misc"]:
        f.write("misc"+"\n")
        for key, value in input_settings["misc"].items():
            if value:
                f.write(key.upper()+"=   "+str(value)+"\n")
            else:
                pass
        f.write("\n")

    f.close()
    if os.path.exists("__pycache__") is True:
      os.system("rm -r __pycache__")

def write_potcar(work_dir, pseudo_par):
    """
    Generates VASP POTCAR file for VASP calculation.

    **Args:

    workdir (str):
    pseudo_par (dict):
    """
    pseudo_dir = pseudo_par["directory"]
    pseudos = pseudo_par["flavor"]

    paths = []
    for pot in pseudos:
        pseudo_path = pseudo_dir + pot
        paths.append(pseudo_path)
    files = " ".join(paths)
    os.system("cat"+files+">> POTCAR")
    shutil.move("POTCAR", work_dir)

def write_poscar(work_dir, structure, number, species, name=None):
    """
    Generates VASP POSCAR file for VASP calculation.

    **Args:

    structure (Structure):
    workdir (str):
    """
    sites = structure._sites
    lattice = structure._lattice
    name = name or structure._name
    number = [str(num) for num in number]
    species_line = " ".join(species)
    number_line = " ".join(number)

    fl_nm = "POSCAR"
    if os.path.exists(work_dir+"/"+fl_nm) is True:
        os.remove(work_dir+"/"+fl_nm)

    f=open(fl_nm, "w+")
    f.write(name+"\n")
    f.write(str(1)+"\n")
    f.write(str(lattice[0][0])+"  "+str(lattice[0][1])+"  "+str(lattice[0][2])+"\n")
    f.write(str(lattice[1][0])+"  "+str(lattice[1][1])+"  "+str(lattice[1][2])+"\n")
    f.write(str(lattice[2][0])+"  "+str(lattice[2][1])+"  "+str(lattice[2][2])+"\n")
    f.write(species_line+"\n")
    f.write(number_line+"\n")
    f.write("Direct"+"\n")
    for site in sites:
        coord = site._coord
        coord_line = str(coord[0])+"  "+str(coord[1])+"  "+str(coord[2])+"\n"
        f.write(coord_line)

    f.close()
    shutil.move("POSCAR", work_dir)
    if os.path.exists("__pycache__") is True:
       os.system("rm -r __pycache__")

def write_kpoints(work_dir, kmesh, qshift=None):
    """
    Generates VASP POSCAR file for VASP calculation.

    **Args:
    workdir (str):
    kmesh (list):
    qmesh (list):
    """
    fl_nm = "KPOINTS"
    f=open(work_dir+"/"+fl_nm, "w+")
    f.write("Automatic mesh \n")
    f.write(str(0)+"\n")
    f.write("Gamma"+"\n")
    f.write(str(kmesh[0])+"  "+str(kmesh[1])+"  "+str(kmesh[2])+"\n")
    if qshift:
       f.write(str(qmesh[0])+"  "+str(qmesh[1])+"  "+str(qmesh[2])+"\n")
    else:
        f.write(str(0)+"  "+str(0)+"  "+str(0)+"\n")
    f.close()
    if os.path.exists("__pycache__") is True:
       os.system("rm -r __pycache__")


def read_incar(incar_path):
    incar = Incar.from_file(incar_path)
    dict = incar.as_dict()
    dict.pop("@class")
    dict.pop("@module")
    start_settings = set(["nwrite", "istart", "iniwav", "icharg", "nelect", "icorelevel", "loptics", "isym", "lelf", "lvhar", "rwigs", "lvtof", "nbands", "lwave"])
    electronic_settings = set(["prec", "algo", "encut", "nelm", "nelmin", "gga", "ediff", "ismear", "sigma", "lasph", "lreal", "addgrid", "maxmix", "bmix"])
    magnetic_settings = set(["magmom", "ispin", "nupdown", "lsorbit", "saxis", "lnoncollinear"])
    hubbard_settings  = set(["ldau", "ldauu", "ldatype", "ldaul", "ldauj", "lmaxmix"])
    ionic_settings = set(["ediffg", "nsw", "ibrion", "isif", "isym", "nblock", "kblock", "iwavpr", "potim"])
    hybrid_settings = set(["lhfcalc", "precfock", "nkred", "time", "hflmax", "hfscreen", "aexx"])


    settings = {}
    start_dict  = {}
    electronic_dict  = {}
    magnetic_dict  = {}
    ionic_dict  = {}
    hubbard_dict  = {}
    hybrid_dict  = {}
    misc_dict = {}

    for key, value in dict.items():
        key = key.lower()
        value = str(value).split("!")
        value = value[0]
        if key in start_settings:
            start_dict[key] = value
        elif key in electronic_settings:
            electronic_dict[key] = value
        elif key in magnetic_settings:
            magnetic_dict[key] = value
        elif key in ionic_settings :
            ionic_dict[key] = value
        elif key in hubbard_settings:
            hubbard_dict[key] = value
        elif key in hybrid_settings :
            hybrid_dict[key] = value
        else:
            misc_dict[key] = value

    settings["start"] = start_dict
    settings["electronic"] = electronic_dict
    settings["magnetic"] = magnetic_dict
    settings["ionic"] = ionic_dict
    settings["hubbard"] = hubbard_dict
    settings["hybrid"] = hybrid_dict
    settings["misc"] = misc_dict
    return settings

#def get_total_energy(file):
#def get_mag_moment(file):
#def get_run_status(file):
#def get_run_time(file):
