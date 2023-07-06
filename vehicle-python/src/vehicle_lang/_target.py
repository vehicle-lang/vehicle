from enum import Enum


class Target(Enum):
    DEFAULT = 1
    LOSS_DL2 = 2
    LOSS_GODEL = 3
    LOSS_LUKASIEWICZ = 4
    LOSS_PRODUCT = 5
    LOSS_YAGER = 6

    @property
    def vehicle_cli_name(self) -> str:
        return {
            Target.DEFAULT: "JSON",
            Target.LOSS_DL2: "DL2Loss",
            Target.LOSS_GODEL: "GodelLoss",
            Target.LOSS_LUKASIEWICZ: "LukasiewiczLoss",
            Target.LOSS_PRODUCT: "ProductLoss",
            Target.LOSS_YAGER: "YagerLoss",
        }[self]
