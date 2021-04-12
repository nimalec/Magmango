from magmango.calculation.incar import IncarSettings

class DefaultSCFParameters(IncarSettings):
         def __init__(self, encut):
             """
             Sets default input parameters for scf ground state energy calculation

             **Args:
               encut (float): planewave energy cutoff for calculation
               name (str): name for scf setting [default="scf_settings"]

             """
             IncarSettings.__init__(self, name=name)
             self.update_settings(setting_type="electronic", "encut", encut)

class DefaultSCFUParameters(IncarSettings):
         def __init__(self, encut, ldaul, Uparam, Jparam):
             """
             Sets default input parameters for scf ground state energy calculation with +U correction

             encut (float): planewave energy cutoff for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for scf+U setting [default="DFTU_settings"]

             """

             dftu_settings = {"ldau": ".TRUE." , "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             IncarSettings.__init__(self, hubbard_settings=dftu_settings)
             self.update_settings(setting_type="electronic", "encut", encut)

class DefaultMagCLParameters(IncarSettings):
         def __init__(self, encut, magmom, ldaul, Uparam, Jparam):
             """
             Sets default input parameters for scf spin collinear calculation

             encut (flt): planewave energy cutoff for calculation
             magmom (list): list of magnetic moments for each species
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTCL_settings"]

             """

             cl_settings =  {"ispin": 2, "magmom": magmom, "saxis": None, "lsorbit": None, "lnoncollinear": None}
             dftu_settings = {"ldau": ".TRUE.", "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             IncarSettings.__init__(self, magnetic_settings=cl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("encut", encut)

class DefaultMagNCLParameters(IncarSettings):
         def __init__(self, spinaxis, encut, ldaul, Uparam, Jparam):
             """
            Sets default input parameters for scf spin non-collinear calculation

             encut (flt): planewave energy cutoff for calculation
             spinaxis (ndarray): spinaxis  for calculation
             ldaul (list): list of  orbital types for each species
             Uparam (list): list of U parameters for each species
             Jparam (list): list of J paramters for each species
             name (str):  name for magnetic noncolinear calculation setting [default="DFTNCL_settings"]
             """
             ncl_settings =  {"ispin": 2, "magmom": None, "saxis": spinaxis, "lsorbit": ".TRUE.", "lnoncollinear": ".TRUE."}
             dftu_settings = {"ldau": ".TRUE.", "ldauu": Uparam, "ldatype": 2, "ldaul": ldaul, "ldauj": Jparam , "lmaxmix": 4}
             IncarSettings.__init__(self, magnetic_settings=ncl_settings, hubbard_settings=dftu_settings)
             self.update_electronic_settings("encut", encut)
