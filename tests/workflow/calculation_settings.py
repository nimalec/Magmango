"""
Module to run tests calculation settings
"""
import os
import numpy as np
from magmango.workflow.calculation_settings import IncarSettings

def incar_settings_std():
    elec_settings =  {"prec":"Accurate" , "algo": "Normal", "encut": 800,
     "nelm": None, "nelmin": None, "gga": "PS" ,"ediff": 10E-05, "ismear": 1, "sigma": 0.2, "lasph": ".TRUE.", "lreal": "Auto", "addgrid": ".TRUE.", "maxmix": 100, "bmix": 1.5}
    mag_settings = {"ispin": 2, "magmom": 2, "saxis": None, "lsorbit": None, "lnoncollinear": None, "nupdown": 1}
    ion_settings = {"edif": 1E-17, "nsw": 20, "ibrion": 2,"isif": 2, "isym": -1, "nblock": 1,  "kblock": 20}
    incr_sett = IncarSettings(magnetic_settings=mag_settings, ionic_settings=ion_settings)
    for key, value in incr_sett.get_electronic_settings().items():
        assert  value == elec_settings[key]
    for key, value in incr_sett.get_magnetic_settings().items():
        assert  value == mag_settings[key]
    for key, value in incr_sett.get_ionic_settings().items():
        assert  value == ion_settings[key]

def incar_settings_from_file():
    cwd = os.getcwd()
    start_settings = {"nwrite": 2, "istart": 1, "iniwav":1 , "icorelevel":1 , "nbands":864 , "nelect": 1247, "isym": -1}
    incar_pth = os.path.join(cwd,"data", "INCAR")
    incr_sett = IncarSettings(from_file=True)
    incr_sett.incar_from_file(incar_pth)
    for key, value in incr_sett.get_start_settings().items():
         assert  value == start_settings[key]

# def incar_settings_update_param():
