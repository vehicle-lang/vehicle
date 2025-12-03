Unstable Python API
===================

``vehicle_lang.compile``
------------------------

.. automodule:: vehicle_lang.compile
   :members:
   :undoc-members:

``vehicle_lang.pygments``
-------------------------

.. automodule:: vehicle_lang.pygments
   :members:
   :undoc-members:

``vehicle_lang.verify``
-----------------------

.. automodule:: vehicle_lang.verify
   :noindex:
   :members:
   :undoc-members:

``vehicle_lang.error``
----------------------

.. automodule:: vehicle_lang.error
   :noindex:
   :members:
   :undoc-members:

``vehicle_lang.typing``
-----------------------

.. automodule:: vehicle_lang.typing
   :members:
   :undoc-members:

Legacy modules
--------------

Earlier releases exposed ``vehicle_lang.ast`` and ``vehicle_lang.compile.*`` helper
modules. The modern Python bindings now marshal data directly through the Vehicle
compiler, so those legacy interfaces are no longer published. Consult the git
history (v0.20 and earlier) if you need the old AST helpers for research or
migration work.
