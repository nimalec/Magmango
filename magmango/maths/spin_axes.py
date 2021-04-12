import numpy
import pymatgen
from numpy import arange, array, meshgrid, ones, sqrt, arccos, sin, cos, pi
from pymatgen.symmetry.groups import SpaceGroup
class SpinVectors:
    def __init__(self, input_spins = None, npoints=None, space_group = None, angle_range = None):
        """
        Set of unit vector representing the orientation of spin magnetic moments.

        :param  npoints: number of spin vectors [int]
        :param space_group: space group of material system [int]
        :param angle_range: angular range of vectors to compute [list or ndarray] ([[phi_min, phi_max], [theta_min, theta_max]])
        """
        self._angle_range = angle_range
        def generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max):
            u = arange(0,1.0,1/int(sqrt(npoints)))
            thet = (thet_max - thet_min)*(u + thet_min)
            phi = arccos((cos(phi_max) - cos(phi_min))*u + cos(phi_min))
            thetas, phis = meshgrid(thet, phi, sparse=False, indexing='xy')
            x, y, z = cos(thetas) * sin(phis), sin(thetas) * sin(phis), cos(phis)
            return array([x.flatten(),y.flatten(),z.flatten()]), array([thetas, phis])

        if npoints:
            assert type(npoints) is int ##change to isinstance
            self._npoints = npoints
            # if space_group:
            #    try:
            #      i = int(space_group)
            #      sg_obj = SpaceGroup.from_int_number(i)
            #    except ValueError:
            #      sg_obj = SpaceGroup(space_group)
            #
            #    self._crystal_type = sg_obj.crystal_system
            #
            #    if self._crystal_type is "triclinic":
            #        self._angle_range = array([[0, pi],[0, 2*pi]])
            #    elif self._crystal_type is "monoclinic":
            #         self._angle_range = array([[0, pi],[0, 2*pi]])
            #    elif self._crystal_type is "orthorhombic":
            #         self._angle_range = array([[0, pi],[0, pi]])
            #    elif self._crystal_type is "tetragonal":
            #         self._angle_range = array([[0, pi],[0, pi/2]])
            #    elif self._crystal_type is "trigonal":
            #         self._angle_range = array([[0, pi],[0, 2*pi]])
            #    elif self._crystal_type is "hexagonal":
            #         self._angle_range = array([[0, pi],[0, 2*pi/3]])
            #    else:
            #         self._angle_range = array([[0, pi/2],[0, pi/2]])
            # else:
            #     self._angle_range = array([[0, pi],[0, 2*pi]])

            thet_min = self._angle_range[1][0]
            thet_max = self._angle_range[1][1]
            phi_min = self._angle_range[0][0]
            phi_max = self._angle_range[0][1]
            self._spin_axes, angle_list = generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max)
            self._spher_coords = [angle_list[0], angle_list[1], ones(npoints)]
        elif input_spins:
           ## Add assertions
           self._spin_axes = array(input_spins)
           self._spher_coords = None
           ## Normalize

       # @property
       # def spher_points(self):
       #    return self._spher_coords
       #
       # # @classmethod
       # # def cart_to_spher(self):
       # #     self.spher_coords_ = None
       # #     return self.spher_coords_
       # @classmethod
       # def add_spins(self, spin_vectors):
       #     spin_list = self.spin_axes_.tolist()
       #     spin_vectors.tolist()
       #     spin_list.append(spin_vectors)
       #     self.spin_axes_ = np.array(spin_list)
       #     self.cart_to_spher()
