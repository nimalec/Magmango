import pymatgen
import numpy as np
from pymatgen.symmetry.groups import SpaceGroup
class SpinsVectors:
    def __init__(self, npoints, space_group = None, angle_range = None, outfile_path = None):
        """
        Set of unit vector representing the orientation of spin magnetic moments.

        :param  npoints: number of spin vectors [int]
        :param space_group: space group of material system [int]
        :param angle_range: angular range of vectors to compute [list or ndarray] ([[phi_min, phi_max], [theta_min, theta_max]])
        """

        assert type(npoints) is int
        self.npoints = npoints
        self.outfile_path = outfile_path

        if space_group:
           try:
             i = int(space_group)
             sg_obj = SpaceGroup.from_int_number(i)
           except ValueError:
             sg_obj = SpaceGroup(space_group)

           self.crystal_type = sg_obj.crystal_system

           if self.crystal_type is "triclinic":
               self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           elif self.crystal_type is "monoclinic":
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           elif self.crystal_type is "orthorhombic":
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           elif self.crystal_type is "tetragonal":
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           elif self.crystal_type is "trigonal":
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           elif self.crystal_type is "hexagonal":
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
           else:
                self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])
        else:
            self.angle_range_ = np.array([[0, np.pi],[0, 2*np.pi]])

       def generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max):
           u = np.arange(0,1.0,1/int(np.sqrt(npoints)))
           thet = (thet_max - thet_min)*(u + thet_min)
           phi = arccos((cos(phi_max) - cos(phi_min))*u + cos(phi_min))
           thetas, phis = np.meshgrid(thet, phi, sparse=False, indexing='xy')
           x, y, z = cos(thetas) * sin(phis), sin(thetas) * sin(phis), cos(phis)
           return np.array([x.flatten(),y.flatten(),z.flatten()])

       thet_min = self.angle_range_[1][0]
       thet_max = self.angle_range_[1][1]
       phi_min = self.angle_range_[0][0]
       phi_max = self.angle_range_[0][1]
       self.spin_axes_ = generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max)

       def add_spins(spin_vectors):



        ## def make_spher_points_h
        ## def cart_to_spher( )

        ## def spher_to_cart( )

        ## def add_points():
