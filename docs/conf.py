# Configuration file for the Sphinx documentation builder.

import os
import sys

sys.path.insert(
    0,
    os.path.abspath(
        os.path.join(os.path.dirname(__file__), os.path.pardir, "vehicle-python", "src")
    ),
)

# -- Project information

project = "Vehicle"
copyright = "2022"
author = "Matthew Daggitt, Wen Kokke, et al."

release = "0.1"
version = "0.1.0"

# -- General configuration

extensions = [
    # Support for Markdown syntax
    # See: https://www.sphinx-doc.org/en/master/usage/markdown.html
    "myst_parser",
    # ???
    "sphinx.ext.duration",
    # ???
    "sphinx.ext.doctest",
    # ???
    "sphinx.ext.intersphinx",
    # Support for automatically documenting Python code
    "sphinx.ext.autodoc",
    # Support for automatically generating API documentation for Python code
    "sphinx.ext.autosummary",
]

# -- Options for rewriting intersphinx

intersphinx_mapping = {
    "python": ("https://docs.python.org/3/", None),
    "sphinx": ("https://www.sphinx-doc.org/en/master/", None),
}
intersphinx_disabled_domains = ["std"]

# -- Options for rewriting external links

extlinks = {".": ("https://github.com/vehicle-lang/vehicle/tree/dev/%s", "./%s")}

# -- Options for excluding temporary files

exclude_patterns = [".tox"]

# -- Options for autodoc and autosummary

autodoc_typehints = "description"

autodoc_mock_imports = ["vehicle_lang._binding"]

autosummary_generate = True

autosummary_imported_members = True

autosummary_ignore_module_all = False

# -- Options for MyST --------------------------------------------------------

myst_enable_extensions = [
    # Enables colon fence directives
    # https://myst-parser.readthedocs.io/en/latest/syntax/optional.html#syntax-colon-fence
    # "colon_fence",
    # Enables definition lists
    # https://myst-parser.readthedocs.io/en/latest/syntax/optional.html#definition-lists
    "deflist",
]

myst_heading_anchors = 5

# -- Options for HTML output

html_theme = "sphinx_rtd_theme"

# -- Options for EPUB output
epub_show_urls = "footnote"
