# Configuration file for the Sphinx documentation builder.

import os
import sys
from html import escape
from urllib.parse import quote

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
    # Support for automatically documenting Python code
    "sphinx.ext.autodoc",
    # Support for external link rewriting
    "sphinx.ext.intersphinx",
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

exclude_patterns: list[str] = ["_build", ".venv", ".DS_Store", "Thumbs.db"]

# -- Options for autodoc

autodoc_typehints = "both"

autodoc_typehints_description_target = "documented"

autodoc_mock_imports = [
    "vehicle_lang._binding",
    "torch",
    "tensorflow",
    "jaxtyping",
]

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
html_static_path = ["_static"]
html_css_files = ["badge.css"]

# -- Global RST substitutions -----------------------------------------------

_BACKENDS = {
    "all": {"label": "All backends"},
    "loss": {"label": "Training"},
    "verification": {"label": "Verification"},
    "agda": {"label": "Agda"},
    "rocq": {"label": "Rocq"},
    "imandra": {"label": "Imandra"},
    "isabelle": {"label": "Isabelle"},
}

_STATUSES = {
    "full": {"label": "full support", "color": "#2ea043", "alt": "fully supported"},
    "part": {"label": "restricted", "color": "#ff9900", "alt": "partially supported"},
    "easy": {
        "label": "no support",
        "color": "#cc1414",
        "alt": "not supported but easy to add",
    },
}


def _svg_badge_data_uri(
    label: str,
    message: str,
    color: str,
    left_width: int = 96,
    right_width: int = 102,
    height: int = 20,
) -> str:
    """Create a fixed-width two-segment badge as an SVG data URI."""
    total_width = left_width + right_width
    label_x = left_width / 2
    message_x = left_width + (right_width / 2)
    text_y = height / 2
    rx = 4

    svg = (
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{total_width}" height="{height}" '
        f'viewBox="0 0 {total_width} {height}" '
        f'role="img" aria-label="{escape(label)}: {escape(message)}">'
        "<defs>"
        f'<clipPath id="r"><rect width="{total_width}" height="{height}" rx="{rx}"/></clipPath>'
        '<linearGradient id="s" x2="0" y2="1">'
        '<stop offset="0" stop-color="#fff" stop-opacity=".15"/>'
        '<stop offset="1" stop-opacity=".15"/>'
        "</linearGradient>"
        "</defs>"
        '<g clip-path="url(#r)">'
        f'<rect width="{left_width}" height="{height}" fill="#555"/>'
        f'<rect x="{left_width}" width="{right_width}" height="{height}" fill="{color}"/>'
        f'<rect width="{total_width}" height="{height}" fill="url(#s)"/>'
        "</g>"
        f'<g fill="#fff" text-anchor="middle" '
        'font-family="DejaVu Sans,Verdana,Geneva,sans-serif" font-size="12" dominant-baseline="middle">'
        f'<text x="{label_x}" y="{text_y}">{escape(label)}</text>'
        f'<text x="{message_x}" y="{text_y}">{escape(message)}</text>'
        "</g>"
        "</svg>"
    )
    return f"data:image/svg+xml;utf8,{quote(svg)}"


def _image_substitution(name: str, url: str, alt_text: str) -> str:
    return (
        f".. |{name}| image:: {url}\n"
        f"   :alt: {alt_text}\n"
        "   :class: backend-badge\n"
    )


def _backend_status_substitutions() -> str:
    lines: list[str] = []
    for backend_key, backend in _BACKENDS.items():
        for status_key, status in _STATUSES.items():
            sub_name = f"backend{backend_key}_{status_key}"
            url = _svg_badge_data_uri(
                label=backend["label"],
                message=status["label"],
                color=status["color"],
            )
            alt_text = f"{backend['label']} {status['alt']}"
            lines.append(_image_substitution(sub_name, url, alt_text))
    return "\n".join(lines)


def _status_legend_substitutions() -> str:
    lines: list[str] = []
    for status_key, status in _STATUSES.items():
        sub_name = f"status_{status_key}"
        url = _svg_badge_data_uri(
            label="status",
            message=status["label"],
            color=status["color"],
            left_width=62,
            right_width=102,
        )
        lines.append(_image_substitution(sub_name, url, status["alt"].capitalize()))

    # Backwards-compatible alias used in docs/language/index.rst.
    part = _STATUSES["part"]
    lines.append(
        _image_substitution(
            "status_partial",
            _svg_badge_data_uri(
                label="status",
                message=part["label"],
                color=part["color"],
                left_width=62,
                right_width=102,
            ),
            part["alt"].capitalize(),
        )
    )
    return "\n".join(lines)


rst_epilog = "\n\n".join(
    [
        _backend_status_substitutions(),
        _status_legend_substitutions(),
    ]
)

# -- Options for EPUB output
epub_show_urls = "footnote"
