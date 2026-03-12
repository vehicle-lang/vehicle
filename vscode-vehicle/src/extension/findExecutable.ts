import { execSync } from "child_process";
import { globSync } from "glob";
import * as path from "path";
import * as fs from "fs";
import type * as vscode from "vscode";
import * as which from "which";
import { LogLevel, type Logger } from "./logger/Logger";

/** Find executable for `vehicle-lsp` when running in Development Mode.
 *
 * This fails if there are multiple versions of `vehicle-lsp` under the
 * `dist-newstyle` directory, which may happen when `vehicle-lsp` has been
 * compiled by different versions of GHC or for different target systems.
 */
export function findExecutable(
  logger: Logger,
  context?: vscode.ExtensionContext,
): string | null {
  if (isProduction(context)) {
    logger.log(LogLevel.Debug, `Mode is production.`);
    return findExecutableOnPath(logger);
  }
  if (isDevelopment(context)) {
    logger.log(LogLevel.Debug, `Mode is development.`);
    return findExecutableInDevelopmentEnvironment(logger, context);
  }
  if (isTest(context)) {
    logger.log(LogLevel.Debug, `Mode is test.`);
    if (typeof process.env.VEHICLE_LSP_PATH === "string") {
      return process.env.VEHICLE_LSP_PATH;
    } else {
      return findExecutableInDevelopmentEnvironment(logger, context);
    }
  }
  return null;
}

/** Find the executable on the PATH. */
function findExecutableOnPath(logger: Logger): string | null {
  // Try and find `vehicle-lsp` on the PATH.
  const executableName =
    process.platform === "win32" ? "vehicle-lsp.exe" : "vehicle-lsp";
  const executable = which.sync(executableName, { nothrow: true });
  if (executable !== null) {
    logger.log(
      LogLevel.Debug,
      [
        // Log message
        `Found vehicle-lsp executable on path:`,
        executable,
      ].join("\n"),
    );
    return executable;
  } else {
    // Raise an error:
    logger.log(
      LogLevel.Error,
      [
        // Log and user message
        `Could not find vehicle-lsp on the PATH`,
      ].join("\n"),
    );
  }
  return null;
}

/** Find the executable in Cabal's dist-newstyle. */
// NOTE(directory-structure):
// This function depends on the name and location of the `vscode-vehicle` and
// `vehicle-lsp` packages as well as the structure of Cabal's dist-newstyle
// directory and must be updated if those change.
// TODO:
// This function could be refined to search only those directories in the
// `dist-newstyle` directory that match the current platform---which would
// require mapping the names for platforms and archirectures from JavaScript
// (`process.platform` and `process.arch`) to Haskell (`System.Info.os` and
// `System.Info.arch`)---and could choose to prefer the `vehicle-lsp` binary
// compiled by the GHC version that is currently on the path, if any.
function findExecutableInDevelopmentEnvironment(
  logger: Logger,
  context?: vscode.ExtensionContext,
): string | null {
  const projectRoot = findProjectRoot(context);
  logger.log(LogLevel.Debug, `Project root is ${projectRoot}.`);
  const executableName =
    process.platform === "win32" ? "vehicle-lsp.exe" : "vehicle-lsp";
  if (isDevelopmentEnvironment(projectRoot)) {
    // Find the executable for `vehicle-lsp` using `cabal list-bin`.
    const cabal = which.sync("cabal", { nothrow: true });
    if (cabal !== null) {
      logger.log(LogLevel.Debug, `Found cabal at ${cabal}.`);
      const vehicleLspBin = execSync(`${cabal} list-bin -v0 vehicle-lsp`, {
        cwd: projectRoot,
        encoding: "utf-8",
      }).trim();
      if (fs.existsSync(vehicleLspBin)) {
        return vehicleLspBin;
      }
    }
    // Find the executable for `vehicle-lsp` using a glob pattern.
    const pattern = path.join(
      "dist-newstyle",
      "build",
      "*-*",
      "ghc-*",
      "vehicle-lsp-*",
      "x",
      "vehicle-lsp",
      "build",
      "vehicle-lsp",
      executableName,
    );
    const candidates = globSync(pattern, {
      cwd: projectRoot,
      windowsPathsNoEscape: true,
    });
    // Found exactly one executable for `vehicle-lsp`.
    if (candidates.length == 1) {
      const executable = candidates[0];
      const executableRelative = path.dirname(
        path.relative(projectRoot, executable),
      );
      logger.log(LogLevel.Debug, `Found vehicle-lsp in '${executableRelative}'`);
      return executable;
    }
    // Found multiple executables for `vehicle-lsp`.
    if (candidates.length >= 2) {
      const candidatesRelative = candidates.map((candidate) =>
        path.dirname(path.relative(projectRoot, candidate)),
      );
      logger.log(
        LogLevel.Warning,
        [
          // Log message
          `Found multiple candidates for vehicle-lsp:`,
          ...candidatesRelative,
        ].join("\n"),
      );
    }
    // Could not find `vehicle-lsp`.
    if (candidates.length == 0) {
      logger.log(
        LogLevel.Warning,
        [
          // User message
          "Could not find vehicle-lsp in dist-newstyle.",
          "Did you forget to run `cabal build vehicle-lsp`?",
        ].join("\n"),
      );
      logger.log(LogLevel.Debug, `Searched with ${pattern}`);
    }
  }
  return null;
}

/** Find the project root. */
// NOTE(directory-structure):
// This function depends on the name and location of the `vscode-vehicle`
// package as well as this file and must be updated if those change.
function findProjectRoot(context?: vscode.ExtensionContext): string {
  if (context) {
    return path.resolve(
      context.extensionPath,
      "..", // vscode-vehicle
    );
  } else {
    return path.resolve(
      __dirname,
      "..", // extension
      "..", // out
      "..", // vscode-vehicle
    );
  }
}

/** Check if the extension is being executed in test mode.
 */
function isProduction(context?: vscode.ExtensionContext): boolean {
  // vscode.ExtensionMode.Production = 1
  // vscode.ExtensionMode.Development = 2
  // vscode.ExtensionMode.Test = 3
  return context && context.extensionMode == 1;
}

/** Check if the extension is being executed in development mode.
 */
function isDevelopment(context?: vscode.ExtensionContext): boolean {
  // vscode.ExtensionMode.Production = 1
  // vscode.ExtensionMode.Development = 2
  // vscode.ExtensionMode.Test = 3
  return context && context.extensionMode == 2;
}

/** Check if the extension is being executed in test mode.
 */
function isTest(context?: vscode.ExtensionContext): boolean {
  // vscode.ExtensionMode.Production = 1
  // vscode.ExtensionMode.Development = 2
  // vscode.ExtensionMode.Test = 3
  return !context || context.extensionMode == 3;
}

/** Check if the extension is being executed from a development environment.
 *
 *  This checks for `cabal` on the path and for the following files:
 *
 *  - `/cabal.project`
 *  - `/packages/vehicle/vehicle.cabal`
 */
// NOTE(directory-structure):
// This function depends on the name and location of the `vscode-vehicle` and
// `vehicle-lsp` packages and must be updated if those change.
function isDevelopmentEnvironment(projectRoot: string): boolean {
  // Check for `cabal` on the path:
  const cabal = which.sync("cabal", { nothrow: true });
  if (cabal === null) {
    return false;
  }
  // Check for basic configuration files:
  const cabalProject = path.join(projectRoot, "cabal.project");
  if (!fs.existsSync(cabalProject)) {
    return false;
  }
  const vehicleLspCabal = path.join(
    projectRoot,
    "vehicle",
    "vehicle.cabal",
  );
  if (!fs.existsSync(vehicleLspCabal)) {
    return false;
  }
  return true;
}
