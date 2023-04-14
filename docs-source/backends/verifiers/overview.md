# Verifier Backends

This page lists supported and possible future verifier backends, and relevant information about them.

## Supported

### [Marabou](https://github.com/NeuralNetworkVerification/Marabou)

![Top Language](https://img.shields.io/github/languages/top/NeuralNetworkVerification/Marabou)

![Verifier Type](https://img.shields.io/badge/verifier%20type-complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/NeuralNetworkVerification/Marabou)

![Latest Tag](https://img.shields.io/github/v/tag/NeuralNetworkVerification/Marabou)

![Latest PyPI Version](https://img.shields.io/pypi/v/Marabou)

Further information:

- The tool is supported by DNNV.
- The tool has continuous integration [on GitHub Actions](https://github.com/NeuralNetworkVerification/Marabou/blob/663835df7b56be42af94b3d2723bf1b69494c0d5/.github/workflows/ci-with-production.yml).
- The tool lists its dependencies in [README.md](https://github.com/NeuralNetworkVerification/Marabou#build-and-dependencies)

---

## Under consideration

### [auto_LiPRA](https://github.com/Verified-Intelligence/auto_LiRPA)

![Top Language](https://img.shields.io/github/languages/top/Verified-Intelligence/auto_LiRPA)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/Verified-Intelligence/auto_LiRPA)

![Latest Tag](https://img.shields.io/github/v/tag/huanzhang12/CROWN-Robustness-Certification)

![Latest PyPI Version](https://img.shields.io/pypi/v/auto-LiRPA)

Further information:

- The tool has no continuous integration.
- The tool lists its dependencies in [setup.py](https://github.com/Verified-Intelligence/auto_LiRPA/blob/d2592c13198e0eb536967186758c264604b59539/setup.py).
- The tool is distributed on PyPI.

### [BaB](https://github.com/oval-group/PLNN-verification)

![Top Language](https://img.shields.io/github/languages/top/oval-group/PLNN-verification)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/oval-group/PLNN-verification)

![Latest Tag](https://img.shields.io/github/v/tag/oval-group/PLNN-verification)

Further information:

- The tool is supported by DNNV.
- The tool lists its dependencies in [setup.py](https://github.com/oval-group/PLNN-verification/blob/9eb91f4a53d30aee432258d3f81bceffdbf49421/setup.py) and [README.md](https://github.com/oval-group/PLNN-verification#dependencies), but does not list version bounds.

### [DNNV](https://github.com/dlshriver/dnnv)

![Top Language](https://img.shields.io/github/languages/top/dlshriver/dnnv)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/dlshriver/dnnv)

![Latest Release](https://img.shields.io/github/v/release/dlshriver/dnnv)

![Latest Tag](https://img.shields.io/github/v/tag/dlshriver/dnnv)

![Latest PyPI Version](https://img.shields.io/pypi/v/dnnv)

Further information:

- The tool has continuous integration [on GitHub Actions](https://github.com/dlshriver/dnnv/blob/d4f59a01810cf4dac99f8f5e5b9d7a350cbfa8d7/.github/workflows/main.yml).
- The tool lists its dependencies in [pyproject.toml](https://github.com/dlshriver/dnnv/blob/d4f59a01810cf4dac99f8f5e5b9d7a350cbfa8d7/pyproject.toml).
- The tool is distributed on PyPI.

### [ERAN](https://github.com/eth-sri/eran)

![Top Language](https://img.shields.io/github/languages/top/eth-sri/eran)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/eth-sri/eran)

![Latest Tag](https://img.shields.io/github/v/tag/eth-sri/eran)

Further information:

- The tool is supported by DNNV.
- The tool has no continuous integration.
- The tool lists its dependencies in [requirements.txt](https://github.com/eth-sri/eran/blob/8771d3158b2c64a360d5bdfd4433490863257dd6/requirements.txt).
- The tool requires proprietary software (Gurobi).

### [MIPVerify.jl](https://github.com/vtjeng/MIPVerify.jl)

![Top Language](https://img.shields.io/github/languages/top/vtjeng/MIPVerify.jl)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/vtjeng/MIPVerify.jl)

![Latest Tag](https://img.shields.io/github/v/tag/vtjeng/MIPVerify.jl)

Further information:

- The tool is supported by DNNV.
- The tool has continuous integration [on GitHub Actions](https://github.com/vtjeng/MIPVerify.jl/blob/4b6050e819cfa4d9d6964f5704527487c59bee0d/.github/workflows/CI.yml).
- The tool lists its dependencies in [Project.toml](https://github.com/vtjeng/MIPVerify.jl/blob/master/Project.toml).

### [nnenum](https://github.com/stanleybak/nnenum)

![Top Language](https://img.shields.io/github/languages/top/stanleybak/nnenum)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/stanleybak/nnenum)

![Latest Tag](https://img.shields.io/github/v/tag/stanleybak/nnenum)

Further information:

- The tool is supported by DNNV.
- The tool has continuous integration [on Travis.CI](https://github.com/stanleybak/nnenum/blob/cf7c0e72c13543011a7ac3fbe0f5c59c3aafa77e/.travis.yml).
- The tool lists its dependencies in [requirements.txt](https://github.com/stanleybak/nnenum/blob/cf7c0e72c13543011a7ac3fbe0f5c59c3aafa77e/requirements.txt).
- The tool has a [Dockerfile](https://github.com/stanleybak/nnenum/blob/cf7c0e72c13543011a7ac3fbe0f5c59c3aafa77e/Dockerfile).

### [NNV](https://github.com/verivital/nnv)

![Top Language](https://img.shields.io/github/languages/top/verivital/nnv)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/verivital/nnv)

![Latest Tag](https://img.shields.io/github/v/tag/verivital/nnv)

Further information:

- The tool has no continuous integration.
- The tool requires proprietary software (MatLab).
- The tool lists its requirements in [README.md](https://github.com/verivital/nnv#installation).

### [Neurify](https://github.com/tcwangshiqi-columbia/Neurify)

![Top Language](https://img.shields.io/github/languages/top/tcwangshiqi-columbia/Neurify)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20incomplete-orange)

![Last Commit](https://img.shields.io/github/last-commit/tcwangshiqi-columbia/Neurify)

![Latest Tag](https://img.shields.io/github/v/tag/tcwangshiqi-columbia/Neurify)

Further information:

- The tool is supported by DNNV.
- The tool has no continuous integration.
- The tool lists is requirements in [README.md](https://github.com/tcwangshiqi-columbia/Neurify#prerequisite).

### [OVAL](https://github.com/oval-group/oval-bab)

![Top Language](https://img.shields.io/github/languages/top/oval-group/oval-bab)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/oval-group/oval-bab)

![Latest Tag](https://img.shields.io/github/v/tag/oval-group/oval-bab)

Further information:

- The tool has no continuous integration.
- The tool lists its requirements in [setup.py](https://github.com/oval-group/oval-bab/blob/5de311325b2750c4bca9134146cca9e148995550/setup.py), but does not list version bounds.

### [Planet](https://github.com/progirep/planet)

![Top Language](https://img.shields.io/github/languages/top/progirep/planet)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/progirep/planet)

![Latest Tag](https://img.shields.io/github/v/tag/progirep/planet)

Further information:

- The tool is supported by DNNV.
- The tool lists its requirements in [README.md](https://github.com/progirep/planet#preparation), but only as packages for Ubuntu 17.04.

### [QNNVerifier](https://github.com/HymnOfLight/QNNVerifier)

![Top Language](https://img.shields.io/github/languages/top/HymnOfLight/QNNVerifier)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/HymnOfLight/QNNVerifier)

![Latest Tag](https://img.shields.io/github/v/tag/HymnOfLight/QNNVerifier)

Further information:

- The tool has no continuous integration.
- The tool lists its requirements in [README.md](https://github.com/HymnOfLight/QNNVerifier#installing), but does not list version bounds.

### [VeriNet](https://github.com/vas-group-imperial/VeriNet)

![Top Language](https://img.shields.io/github/languages/top/vas-group-imperial/VeriNet)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/vas-group-imperial/VeriNet)

![Latest Tag](https://img.shields.io/github/v/tag/vas-group-imperial/VeriNet)

Further information:

- The tool is supported by DNNV.
- The tool has no continuous integration.
- The tool lists its requirements in [Pipfile](https://github.com/vas-group-imperial/VeriNet/blob/885e51983f9078bce5a251b922bed6bf329539e3/Pipfile), but does not list version bounds.
- The tool lists exact versions for its requirements in [Pipfile.lock](https://github.com/vas-group-imperial/VeriNet/blob/885e51983f9078bce5a251b922bed6bf329539e3/Pipfile.lock)

---

## Rejected

### [α,β-CROWN](https://github.com/Verified-Intelligence/alpha-beta-CROWN)

![Top Language](https://img.shields.io/github/languages/top/Verified-Intelligence/alpha-beta-CROWN)

![Verifier Type](https://img.shields.io/badge/verifier%20type-sound%20and%20complete-brightgreen)

![Last Commit](https://img.shields.io/github/last-commit/Verified-Intelligence/alpha-beta-CROWN)

![Latest Tag](https://img.shields.io/github/v/tag/Verified-Intelligence/alpha-beta-CROWN)

Further information:

- The tool has no continuous integration.
- The tool lists its requirements in [requirements.txt](https://github.com/Verified-Intelligence/alpha-beta-CROWN/blob/7b3d507caf40a5f42d99f6894373526c6fd615b7/vnncomp_scripts/requirements.txt)
- [The tool is only compatible with Ubuntu 22.04.](https://github.com/Verified-Intelligence/alpha-beta-CROWN/blob/7b3d507caf40a5f42d99f6894373526c6fd615b7/vnncomp_scripts/install_tool.sh#L3)
- The tool is not distributed on PyPI and has no setup.py script.

Reasons for Rejection on 2023-04-14:

- Limited compatibility, no versioning or distribution.

### [CROWN](https://github.com/IBM/CROWN-Robustness-Certification)

![Top Language](https://img.shields.io/github/languages/top/huanzhang12/CROWN-Robustness-Certification)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/Verified-Intelligence/auto_LiRPA)

![Latest Tag](https://img.shields.io/github/v/tag/huanzhang12/CROWN-Robustness-Certification)

Further information:

- The repository is mirrored at [huanzhang12/CROWN-Robustness-Certification](https://github.com/huanzhang12/CROWN-Robustness-Certification).
- The tool has no continuous integration.
- The tool does not list its dependencies.

Reasons for Rejection on 2023-04-14:

- Unmaintained and deprecated in favour of auto_LiPRA.

### [Reluplex](https://github.com/guykatzz/ReluplexCav2017)

![Top Language](https://img.shields.io/github/languages/top/stanleybak/nnenum)

![Verifier Type](https://img.shields.io/badge/verifier%20type-unknown-lightgrey)

![Last Commit](https://img.shields.io/github/last-commit/stanleybak/nnenum)

![Latest Tag](https://img.shields.io/github/v/tag/stanleybak/nnenum)

Further information:

- The tool is supported by DNNV.
- The tool has no continuous integration.
- The tool lists its dependencies in [README.txt](https://github.com/guykatzz/ReluplexCav2017#readme).

Reasons for Rejection on 2023-04-14:

- Unmaintained and deprecated in favour of Marabou.
