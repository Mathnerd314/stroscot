# Configuration file for the Sphinx documentation builder.
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information

project = 'Stroscot'
copyright = '2019-2022 Mathnerd314'
author = 'Mathnerd314'

# -- General configuration

# Add any Sphinx extension module names here, as strings.
extensions = [
    'sphinx.ext.mathjax',
    'sphinx.ext.graphviz',
    'sphinxcontrib.bibtex',
    # ,'sphinxcontrib.fulltoc'
    'sphinx_rtd_theme',
]

highlight_language='haskell'

mathjax_path = "https://cdn.jsdelivr.net/npm/mathjax@3.0.1/es5/tex-mml-chtml.js"

graphviz_output_format = 'svg'

# References
bibtex_bibfiles = ['references.bib']


# Add any paths that contain templates here, relative to this directory.
templates_path = ['_templates']

# List of patterns, relative to source directory, to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = ['_build', 'Thumbs.db', '.DS_Store']

# -- Options for HTML output and RTD theme

html_theme = 'sphinx_rtd_theme'
html_static_path = ['_static']
html_css_files = [
    'custom.css',
]
html_favicon = '_static/hexagon_favicon.png'
html_logo = '_static/hexagon_logo.png'
html_copy_source = False
html_context = {
    'display_github': True,
    'github_user': 'Mathnerd314', # Username
    'github_repo': 'stroscot', # Repo name
    'github_version': 'master', # Version
    'conf_py_path': '/docs/', # Path in the checkout to the docs root
    'theme_vcs_pageview_mode': 'edit',
    'theme_logo_only': True,
}
