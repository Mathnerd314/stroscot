# Configuration file for the Sphinx documentation builder.
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------

project = 'Stroscot'
copyright = '2019-2020 Mathnerd314'
author = 'Mathnerd314'


# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings.
extensions = [
    'sphinx_rtd_theme',
    'sphinx.ext.mathjax',
    'sphinx.ext.graphviz',
    'sphinxcontrib.bibtex'
    # ,'sphinxcontrib.fulltoc'
]

mathjax_path = "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"

# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']


# -- Options for HTML output -------------------------------------------------

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
html_copy_source = False
html_context = {
    'display_github': True,
    'github_user': 'Mathnerd314', # Username
    'github_repo': 'stroscot', # Repo name
    'github_version': 'master', # Version
    'conf_py_path': '/docs/', # Path in the checkout to the docs root
    'theme_vcs_pageview_mode': 'edit'
}
