# Configuration file for the Sphinx documentation builder.

import pathlib
import sys

sys.path.insert(0, pathlib.Path(__file__).parent / "vehicle-python" / "src")

# -- Project information

project = "Vehicle"
copyright = "2022"
author = "Matthew Daggitt, Wen Kokke, et al."

release = "0.1"
version = "0.1.0"

# -- General configuration

extensions = [
    "myst_parser",
    "sphinx.ext.duration",
    "sphinx.ext.doctest",
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx.ext.intersphinx",
]

intersphinx_mapping = {
    "python": ("https://docs.python.org/3/", None),
    "sphinx": ("https://www.sphinx-doc.org/en/master/", None),
}
intersphinx_disabled_domains = ["std"]

templates_path = ["_templates"]

# -- Options for rewriting external links

extlinks = {".": ("https://github.com/vehicle-lang/vehicle/tree/dev/%s", "./%s")}

# -- Options for excluding temporary files

exclude_patterns = [".tox"]

# -- Options for autosummary

autosummary_mock_imports = ["vehicle_lang._binding"]

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
