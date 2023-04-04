%module binding
%{
#include "VehiclePythonBinding_stub.h"

HsInt32 _unsafeVehicleMain(HsInt32 argc, HsPtr argv);

void _hs_rts_init(int argc, char **argv)
{
  hs_init(&argc, &argv);
}

void _hs_rts_exit()
{
  void hs_exit();
}
%}

%typemap(in) (int argc, char **argv) {
  /* Check if is a list */
  if (PyList_Check($input)) {
    int i;
    $1 = PyList_Size($input);
    $2 = (char **) malloc(($1+1)*sizeof(char *));
    for (i = 0; i < $1; i++) {
      PyObject *o = PyList_GetItem($input, i);
      if (PyUnicode_Check(o)) {
        $2[i] = (char *) PyUnicode_AsUTF8AndSize(o, 0);
      } else {
        PyErr_SetString(PyExc_TypeError, "list must contain strings");
        SWIG_fail;
      }
    }
    $2[i] = 0;
  } else {
    PyErr_SetString(PyExc_TypeError, "not a list");
    SWIG_fail;
  }
}

%typemap(freearg) (int argc, char **argv) {
  free((char *) $2);
}

int _unsafeVehicleMain(int argc, char **argv);
void _hs_rts_init(int argc, char **argv);
void _hs_rts_exit();
