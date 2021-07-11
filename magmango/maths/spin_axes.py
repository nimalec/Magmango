import pymatgen
import numpy as np
from numpy import arange, array, meshgrid, ones, sqrt, arccos, sin, cos, pi
from pymatgen.symmetry.groups import SpaceGroup
class SpinVectors:
    def __init__(self, npoints, angle_range = None, space_group = None):
        """
        Set of unit vector representing the orientation of spin magnetic moments.

        :param  npoints: number of spin vectors [int]
        :param space_group: space group of material system [int]
        :param angle_range: angular range of vectors to compute [list or ndarray] ([[phi_min, phi_max], [theta_min, theta_max]])
        """
        if angle_range:
            self._angle_range = angle_range

        elif space_group:
            def angle_range_from_sg_h(space_group_number):
                if 1 <= space_group_number <= 15:
                    angle_range =  [[0,2*pi], [0,pi]]
                elif 16 <= space_group_number <= 74:
                    angle_range =  [[0,pi], [0,pi]]
                elif 75 <= space_group_number <= 142:
                    angle_range =  [[0,pi/2], [0,pi]]
                elif 143 <= space_group_number <= 194:
                    angle_range =  [[0,2*pi/3], [0,pi]]
                else:
                    angle_range =  [[0,pi/2], [0,pi/2]]
                return angle_range
            self._angle_range = angle_range_from_sg_h(space_group)

        else:
            self._angle_range =  [[0,2*pi], [0,pi]]

        def generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max):
            u = arange(0,1.0,1/int(sqrt(npoints)))
            thet = (thet_max - thet_min)*(u + thet_min)
            phi = arccos((cos(phi_max) - cos(phi_min))*u + cos(phi_min))
            thetas, phis = meshgrid(thet, phi, sparse=False, indexing='xy')
            x, y, z = cos(thetas) * sin(phis), sin(thetas) * sin(phis), cos(phis)
            return array([x.flatten(),y.flatten(),z.flatten()]), array([thetas, phis])

        assert type(npoints) is int ##change to isinstance
        self._npoints = npoints
        thet_min = self._angle_range[0][0]
        thet_max = self._angle_range[0][1]
        phi_min = self._angle_range[1][0]
        phi_max = self._angle_range[1][1]
        spin_axes, angle_list = generate_semi_spin_axes_h(npoints, thet_min, thet_max, phi_min, phi_max)
        #self._spin_axes = np.uniquspin_axes,axis=0)
        self._spin_axes  = np.unique(spin_axes.T,axis=0)
        self._spher_coords = [angle_list[0], angle_list[1], ones(npoints)]
