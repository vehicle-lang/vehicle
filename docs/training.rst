Training with a specification
=============================
Vehicle compiles each ``@property`` in your specification into a callable loss function. Use one of the backend modules (PyTorch or TensorFlow) to load the compiled declarations into Python:

.. code-block:: python

   import vehicle_lang as vcl
   from vehicle_lang.loss import pytorch as loss_pt

   declarations = loss_pt.load_specification(
       "spec.vcl",
       logic=vcl.DifferentiableLogic.Vehicle,  # optional, defaults to DL2
   )

   constraint_loss_fn = declarations["output_bounded"]

Each callable returned by ``load_specification`` has a signature that mirrors the ``@network`` declarations referenced inside that property. Pass a Python callable for each network so the loss function can evaluate (and differentiate through) your model. A typical PyTorch training step looks like this:

.. code-block:: python

   import torch

   model = torch.nn.Sequential(
       torch.nn.Linear(1, 8),
       torch.nn.ReLU(),
       torch.nn.Linear(8, 1),
   )
   optimizer = torch.optim.Adam(model.parameters(), lr=1e-2)

   def network(x: torch.Tensor) -> torch.Tensor:
       return model(x.reshape(1, 1)).reshape(1)

   alpha = 0.5  # Blend task and constraint losses

   for epoch in range(num_epochs):
       for x_batch, y_batch in train_loader:
           optimizer.zero_grad()

           preds = model(x_batch)
           task_loss = torch.nn.functional.mse_loss(preds, y_batch)

           constraint_loss = constraint_loss_fn(network)
           total_loss = alpha * task_loss + (1.0 - alpha) * constraint_loss

           total_loss.backward()
           optimizer.step()

Here ``train_loader`` can be any ``torch.utils.data.DataLoader`` that yields ``(x_batch, y_batch)`` pairs, and the ``network`` callable must match the type you declared for the Vehicle network so gradient-based samplers can evaluate it.

TensorFlow works the same way—load the declarations through ``vehicle_lang.loss.tensorflow`` and call the compiled property inside a ``tf.GradientTape`` so gradients flow through both the task loss and the constraint loss:

.. code-block:: python

   import vehicle_lang as vcl
   from vehicle_lang.loss import tensorflow as loss_tf

   declarations = loss_tf.load_specification(
       "spec.vcl",
       logic=vcl.DifferentiableLogic.Vehicle,
   )
   constraint_loss_fn = declarations["output_bounded"]

   with tf.GradientTape() as tape:
       preds = model(x_batch)
       task_loss = tf.reduce_mean(tf.square(preds - y_batch))
       constraint_loss = constraint_loss_fn(network)
       total_loss = alpha * task_loss + (1.0 - alpha) * constraint_loss

   grads = tape.gradient(total_loss, model.trainable_variables)
   optimizer.apply_gradients(zip(grads, model.trainable_variables))

By combining your usual task loss with the constraint losses returned by Vehicle, you ensure the optimiser simultaneously keeps the model on task and enforces the specification.
