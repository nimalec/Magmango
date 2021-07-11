import os
from shutil import copy

from pymatgen.symmetry.analyzer import SpacegroupAnalyzer

from magmango.maths.spin_axes import SpinVectors
from magmango.calculation.calculation import Calculation
from magmango.calculation.incar import IncarSettings
from magmango.calculation.poscar import PoscarSettings
from magmango.calculation.potcar import PotcarSettings
#from magmango.calculation.runscript import RunscriptSettings
from magmango.calculation.kpoints import KpointsSettings
#from magmango.calculation.incar_default_settings import DefaultMagNCLParameters

class MagneticAnisotropyFlow:
    def __init__(self, npoints, work_dir,cl_calculation=None,cl_dir=None,symm_reduce=False,angle_range=None):

        #insert excpetion handeling for inputs
        self._work_dir = work_dir
        self._npoints = npoints

        os.mkdir(self._work_dir)
        #new_cl_dir = self._work_dir + "/scf_cl"
        if cl_calculation:
            #cl_calculation.make_calculation(new_cl_dir)
            self._cl_calculation = cl_calculation
        # If directory for calculation provided
        elif cl_dir:
            new_cl_dir = self._work_dir + "/scf_cl"
            copy(cl_dir, new_cl_dir)
            incar_cl = IncarSettings()
            incar_cl.from_file(cl_dir+"/INCAR")
            poscar = PoscarSettings()
            poscar.from_file(cl_dir+"/POSCAR")
            potcar = PotcarSettings()
            potcar.from_file(cl_dir+"/POTCAR")
            kpts = KPointsSettings()
            kpts.from_file(cl_dir+"/KPOINTS")
            #runscript = RunscriptSettings()
            #runscript.from_file(cl_dir+"/run.sh")
            self._cl_calculation = Calculation(incar_cl,kpt,poscar,potcar)
        else:
            pass

        self._cl_calculation.make_calculation(os.path.join(self._work_dir,"scf_cl"))
        struct = self._cl_calculation._poscar._structure
        space_group = SpacegroupAnalyzer(struct).get_space_group_number()

        if angle_range:
            spins = SpinVectors(npoints=npoints, angle_range=angle_range)
        elif symm_reduce is True:
            spins = SpinVectors(npoints=npoints, space_group=space_group)
        else:
            spins = SpinVectors(npoints=npoints)
        self._spins = spins._spin_axes.T

        def setup_ncl_calc(work_dir, spins,incar_cl,poscar,potcar,kpoints):
            ncl_calcs = []
            incar_ncl = incar_cl
            incar_ncl.remove_settings("MAGMOM")
            incar_ncl.update_settings("ISPIN", 1)
            incar_ncl.update_settings("LSORBIT", ".TRUE.")
            incar_ncl.update_settings("LNONCOLLINEAR", ".TRUE.")
            incar_ncl.update_settings("LPLANE", ".FALSE.")
            #incar_ncl.update_settings("NBANDS", str(2*nbands))
            ncl_dir = os.path.join(work_dir, "scf_ncl")
            os.mkdir(ncl_dir)
            itr=0
            for spin in spins:
                itr+=1
                incar_ncl.update_settings("SAXIS",[spin[0], spin[1], spin[2]])
                calc  = Calculation(incar_ncl,kpoints,poscar,potcar)
                calc.make_calculation(os.path.join(ncl_dir, "ncl_"+str(itr)))
        setup_ncl_calc(self._work_dir, self._spins,self._cl_calculation._incar,self._cl_calculation._poscar,self._cl_calculation._potcar,self._cl_calculation._kpoints)


    # def run_cl_calculations(self):
    #     self._collinear_calc.run_calculation()

    # def run_ncl_calculations(self):
    #     for calc in self._noncollinear_calcs:
    #         calc.run_calculation()
