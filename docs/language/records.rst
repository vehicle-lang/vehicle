Records
=======

.. contents::
   :depth: 1
   :local:

Basics
------

Records allow you to group logical sets of parameters to together. A record can be defined
as follows:

.. code-block:: agda

    record <name> where
      { <fieldName> : <fieldType>
      , ...
      , <fieldName> : <fieldType>
      }

For example, a record that defines a simple pair can be defined :

.. code-block:: agda

    record Pair where
      { a : Real
      , b : Real
      }

A new instance of this record can be defined by providing a value for each of the record fields as follows:

.. code-block:: agda

    myPair : Pair
    myPair = { a = 1.0, b = 0.5 }

The fields can then be accessed via standard ``.`` notation, e.g.:

.. code-block: agda

    sum : Pair -> Real
    sum pair = pair.a + pair.b
