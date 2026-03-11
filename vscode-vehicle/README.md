# Vehicle VS Code Extension

To build the extension, run:

```sh
npm install
npm run compile
```

To start the extension:

1.  Build `vehicle-lsp`

    ```sh
    # from anywhere in the project
    cabal build vehicle-lsp
    ```

2.  Build `vscode-vehicle`

    ```sh
    # from the vscode-vehicle directory
    npm run compile
    ```

3.  Run the VS Code extension.
    Any of the following should work:

    - Press F5
    - In the menu, select "Run" followed by "Start Debugging".
    - In the "Run and Debug" tab, press "Run Extension".

