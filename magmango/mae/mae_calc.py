import os
from shutil import copy
from magmango.maths.spin_axes import SpinVectors
from magmango.calculation.calculation import Calculation
#from magmango.calculation.incar_default_settings import DefaultMagNCLParameters

class MagneticAnisotropyFlow:
    def __init__(self, work_dir, npoints, angle_range, cl_calculation,cl_dir=None):
        self._work_dir = work_dir
        self._npoints = npoints
        # If calculation class for CL provided
        new_cl_dir = self._work_dir + "/scf_cl"
        if cl_calculation:
            cl_calculation.make_calculation(new_cl_dir)
            self._cl_calculation = cl_calculation
        # If directory for calculation provided
        elif cl_dir:
            new_cl_dir = self._work_dir + "/scf_cl"
            copy(cl_dir, new_cl_dir)
            incar_cl = IncarSettings()
            incar_cl.incar_from_file(new_cl_dir+"/INCAR")
            self._incar_cl = incar_cl
            poscar = PoscarSettings()
            poscar.poscar_from_file(cl_dir+"/POSCAR")
            potcar = PotcarSettings()
            potcar.potcar_from_file(cl_dir+"/POTCAR")
            kpts = KPointsSettings()
            kpts.kpoints_from_file(cl_dir+"/KPOINTS")
            runscript = RunscriptSettings()
            runscript.runscript_from_file(cl_dir+"/run.sh")
            self._cl_calculation = Calculation(new_cl_dir,incar_cl,kpoints,poscar,potcar._outfile_path,runscript)
        else:
            pass
             ## raise an error message instead

        incar_cl = IncarSettings()
        incar_cl.incar_from_file(cl_dir+"/INCAR")
        self._incar_cl = incar_cl
        poscar = PoscarSettings()
        poscar.poscar_from_file(cl_dir+"/POSCAR")
        self._poscar = poscar
        kpts = KPointsSettings()
        kpts.kpoints_from_file(cl_dir+"/KPOINTS")
        self._kpoints = kpts
        potcar = PotcarSettings()
        potcar.potcar_from_file(cl_dir+"/POTCAR")
        self._potcar = potcar
        spins = SpinVectors(npoints=npoints, angle_range=angle_range)
        self._spins = spins._spin_axes.T

        def setup_ncl_calc(spins,incar_cl, poscar, potcar, kpoints):
            ncl_calc = []
            incar_ncl = incar_cl
            incar_ncl.pop("MAGMOM")
            for spin in spins:


            #kpoints = cl_calc.



    # def set_ncl_calculations(self):
    #     itr = 0
    #     lnk_line = "ln -s " + self._cl_dir + " ."
    #     ncl_setts = self._incar
    #     ncl_setts.update_settings("magnetic", "MAGMOM", None)
    #     ncl_setts.update_settings("magnetic", "ISPIN", 1)
    #     ncl_setts.update_settings("magnetic", "LSORBIT", ".TRUE.")
    #     for spin in self._spins:
    #          ncl_setts.update_settings("magnetic", "SAXIS", spin)
    #          ncl_dir = os.path.join(workdir ,"scf_ncl" ,"scf_"+str(itr))
    #          ncl_calc = Calculation(ncl_dir, ncl_setts, self._kpoints, self._poscar, self._potcar, self._runscript)
    #          ncl_calc._runscript.update_settings("links", "link1", lnk_line)
    #          self._noncollinear_calcs.append(ncl_calc)
    #          itr += 1

    def make_calculations(self):
        os.mkdir(self._work_dir)
        os.mkdir(os.path.join(self._work_dir, "scf_cl"))
        os.mkdir(os.path.join(self._work_dir, "scf_ncl"))
        if self._collinear_calc:
            self._collinear_calc.make_calculation()
        else:
            pass
        for calc in self._noncollinear_calcs:
            calc.make_calculation()

    def run_cl_calculations(self):
        self._collinear_calc.run_calculation()

    def run_ncl_calculations(self):
        for calc in self._noncollinear_calcs:
            calc.run_calculation()
