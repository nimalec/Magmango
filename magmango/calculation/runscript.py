class RunscriptSettings:
	def __init__(self, from_file=False, comment = "Gamma", npoints=0, k_pts = [2,2,2], q_shift = [0,0,0]):
		if from_file is False:
			self._settings = {"comment": comment, "npoints": npoints, "kpoints": k_pts, "qpoints": q_shift}
		else:
			pass
