import setuptools

with open("README.md", "r", encoding="utf-8") as fh:
    long_description = fh.read()

setuptools.setup(
    name="magmango-nimalec", # Replace with your own username
    version="0.0.1",
    author="Nima Leclerc",
    author_email="nleclerc@lbl.gov",
    description="Magmango: the magnetic properties package. Compute magnetic properties of materials from first-principles",
    long_description=long_description,
    long_description_content_type="text/markdown",
    url="https://github.com/nimalec/Magmango",
    packages=setuptools.find_packages(),
    classifiers=[
        "Programming Language :: Python :: 3",
        "License :: OSI Approved :: MIT License",
        "Operating System :: OS Independent",
    ],
    python_requires='>=3.6',
)
