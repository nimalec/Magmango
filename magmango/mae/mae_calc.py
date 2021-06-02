import os
from shutil import copy
from magmango.maths.spin_axes import SpinVectors
from magmango.calculation.calculation import Calculation
from magmango.calculation.runscript import RunscriptSettings
#from magmango.calculation.incar_default_settings import DefaultMagNCLParameters

class MagneticAnisotropyFlow:
    def __init__(self, work_dir, npoints, cl_calculation, nbands, runscript_ncl, angle_range=None, cl_dir=None):
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
        runscript = RunscriptSettings()

        def setup_ncl_calc(work_dir, spins,incar_cl,poscar, potcar,kpoints,runscript,nbands):
            ncl_calcs = []
            incar_ncl = incar_cl
            incar_ncl.remove_settings("MAGMOM")
            incar_ncl.update_settings("ISPIN", 1)
            incar_ncl.update_settings("LSORBIT", ".TRUE.")
            incar_ncl.update_settings("LNONCOLLINEAR", ".TRUE.")
            incar_ncl.update_settings("LPLANE", ".FALSE.")
            incar_ncl.update_settings("NBANDS", str(2*nbands))

            ncl_dir = work_dir + "/scf_ncl"
            itr=0
            for spin in spins:
                itr+=1
                incar_ncl.update_settings("SAXIS",spin)
                calc_ncl =  Calculation(ncl_dir+"ncl_"+str(itr),incar_ncl,kpoints,poscar,potcar._outfile_path,runscript)
                ncl_calcs.append(calc_ncl)
            return ncl_calcs

        self._ncl_calculations = setup_ncl_calc(work_dir, spins,incar_cl,poscar,potcar,kpoints,runscript_ncl,nbands)


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

    # def run_cl_calculations(self):
    #     self._collinear_calc.run_calculation()

    # def run_ncl_calculations(self):
    #     for calc in self._noncollinear_calcs:
    #         calc.run_calculation()
