import * as vscode from "vscode";
import * as lsp from "vscode-languageclient/node";
import { findExecutable } from "./extension/findExecutable";
import { ArrayLogger } from "./extension/logger/ArrayLogger";
import { VSCodeOutputChannelLoggerAdapter } from "./extension/logger/VSCodeOutputChannelLoggerAdapter";
import { VSCodeWindowLogger } from "./extension/logger/VSCodeWindowLogger";

export const extensionId = "wenkokke.vscode-vehicle";

export async function extensionAPI(): Promise<ExtensionAPI> {
  const ext = vscode.extensions.getExtension(extensionId);
  return ext.isActive ? ext.exports : await ext.activate();
}

export type ExtensionAPI = {
  client: lsp.LanguageClient;
};

let client: lsp.LanguageClient;

export async function activate(
  context: vscode.ExtensionContext,
): Promise<ExtensionAPI> {
  // Create output channels.
  const debugOutputChannel = vscode.window.createOutputChannel(
    "Vehicle LSP Client",
    "vehicle",
  );
  const outputChannel = vscode.window.createOutputChannel(
    "Vehicle LSP Server",
    "vehicle",
  );

  // Create logger.
  const clientLogger = new ArrayLogger([
    new VSCodeOutputChannelLoggerAdapter(debugOutputChannel),
    VSCodeWindowLogger,
  ]);

  // Create filesystem watcher for the LSP Client.
  const clientFileSystemWatcher =
    vscode.workspace.createFileSystemWatcher("*.vcl");

  // Create options for the LSP Client.
  const clientOptions: lsp.LanguageClientOptions = {
    // Register the server for Vehicle documents.
    documentSelector: [{ scheme: "file", language: "vehicle" }],
    // Notify the server about changes to Vehicle files in the workspace.
    synchronize: { fileEvents: clientFileSystemWatcher },
    // Add an output channel for server output.
    traceOutputChannel: debugOutputChannel,
    // Add an output channel for client output.
    outputChannel: outputChannel,
  };

  // Find the vehicle executable:
  const vehicleExecutable = findExecutable(clientLogger, context);

  // Create options for running the LSP Server.
  const serverOptions: lsp.ServerOptions = {
    command: vehicleExecutable,
    args: ["lsp"]
  };

  // Create a Language Server Client.
  client = new lsp.LanguageClient("Vehicle", serverOptions, clientOptions);

  // client.trace = lsp.Trace.Verbose;

  // Start the Language Server Client.
  client.start();

  // Return the extension API.
  return { client };
}

export function deactivate(): Promise<void> | undefined {
  if (!client) {
    return undefined;
  }
  return client.stop();
}
