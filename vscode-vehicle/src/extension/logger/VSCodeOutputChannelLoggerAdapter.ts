import * as vscode from "vscode";
import { type Logger, type LogLevel, logFormat } from "./Logger";

export class VSCodeOutputChannelLoggerAdapter implements Logger {
  readonly outputChannel: vscode.OutputChannel;

  constructor(outputChannel: vscode.OutputChannel) {
    this.outputChannel = outputChannel;
  }

  log(level: LogLevel, message: string): void {
    this.outputChannel.appendLine(logFormat(level, message));
  }
}
