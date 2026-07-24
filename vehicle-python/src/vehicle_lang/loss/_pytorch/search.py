from dataclasses import dataclass
from typing import Any,  List
import torch

@dataclass
class Sample:
    inputs: dict[str, Any]
    loss: float
    loss_history: List[float]


def pgd(
    quantifier_data: List[Any],
    loss_fn: Any,
    num_steps: int = 5, # number of steps per quantified variable
) -> Sample:

    # Set starting points for all quantified variables
    current_inputs = {}
    for quantifier in quantifier_data:
        upper_bound = quantifier.upper_bound
        lower_bound = quantifier.lower_bound
        range_size = upper_bound - lower_bound

        initial_point = (
            lower_bound + torch.rand((), dtype=lower_bound.dtype) * range_size
        )
        current_inputs[quantifier.name] = initial_point
    
    loss_history = []
    for quantifier in quantifier_data:
        upper_bound = quantifier.upper_bound
        lower_bound = quantifier.lower_bound
        epsilon = (upper_bound - lower_bound) / num_steps

        for _ in range(num_steps):
            current_point = current_inputs[quantifier.name].detach().clone().requires_grad_(True)
            current_inputs[quantifier.name] = current_point
            
            loss = loss_fn(**current_inputs)
            loss_history.append(loss.item())

            if loss.requires_grad:
                gradient = torch.autograd.grad(
                    loss,
                    current_point,
                    create_graph=False,
                    retain_graph=False,
                    only_inputs=True
                )[0]

                # If gradient contains NaN, replace with zeros
                if gradient is not None:
                    gradient = torch.where(
                        torch.isnan(gradient), torch.zeros_like(gradient), gradient
                    )
                else:
                    gradient = torch.zeros_like(current_point)
            else:
                # No gradients available, can't perform FGSM perturbation
                gradient = torch.zeros_like(current_point)
            
            sign_grad = torch.sign(gradient)
            # -epsilon * sign_grad because to search for witnesses, the loss must be minimised
            perturbation = -epsilon * sign_grad

            perturbed_point = torch.clamp(
                current_point + perturbation.detach(), lower_bound, upper_bound
            ).detach()
            current_inputs[quantifier.name] = perturbed_point
        
    final_loss = loss_fn(**current_inputs)
    return Sample(inputs=current_inputs, loss=final_loss.item(), loss_history=loss_history)