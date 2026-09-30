import * as vscode from "vscode";
import { Logger, LogLevel } from "./Logger";

export const VSCodeWindowLogger: Logger = {
  log(level: LogLevel, message: string): void {
    switch (level) {
      case LogLevel.Information:
        vscode.window.showInformationMessage(message);
        break;
      case LogLevel.Warning:
        vscode.window.showWarningMessage(message);
        break;
      case LogLevel.Error:
        vscode.window.showErrorMessage(message);
        break;
    }
  },
};
