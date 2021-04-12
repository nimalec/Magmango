import os
from magmango.maths.spin_axes import SpinVectors
from magmango.calculation.calculation import Calculation
from magmango.calculation.incar_default_settings import DefaultMagNCLParameters

class MagneticAnisotropyFlow:
    def __init__(self, work_dir, npoints, angle_range, incar, poscar, potcar, kpoints, run_script, cl_calc_dir=None):
        self._work_dir = work_dir
        self._cl_dir =  cl_calc_dir
        spins = SpinVectors(npoints=npoints, angle_range=angle_range)
        self._spins = spins._spin_axes.T
        self._incar = incar
        self._poscar = poscar
        self._potcar = potcar
        self._kpoints = kpoints
        self._runscript = run_script

        if cl_calc_dir:
           self._collinear_calc = None
        else:
           cl_dir = os.path.join(self._work_dir,"scf_cl")
           self._collinear_calc = Calculation(cl_dir, self._incar, self._kpoints, self._poscar, self._potcar, self._runscript)
           self._cl_dir = cl_dir
        self._noncollinear_calcs = []

    def set_ncl_calculations(self):
        itr = 0
        lnk_line = "ln -s " + self._cl_dir + " ."
        ncl_setts = self._incar
        ncl_setts.update_settings("magnetic", "MAGMOM", None)
        ncl_setts.update_settings("magnetic", "ISPIN", 1)
        ncl_setts.update_settings("magnetic", "LSORBIT", ".TRUE.")
        for spin in self._spins:
             ncl_setts.update_settings("magnetic", "SAXIS", spin)
             ncl_dir = os.path.join(workdir ,"scf_ncl" ,"scf_"+str(itr))
             ncl_calc = Calculation(ncl_dir, ncl_setts, self._kpoints, self._poscar, self._potcar, self._runscript)
             ncl_calc._runscript.update_settings("links","link1", lnk_line)
             self._noncollinear_calcs.append(ncl_calc)
             itr += 1

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
