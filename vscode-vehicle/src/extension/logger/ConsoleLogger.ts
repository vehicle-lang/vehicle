import { type Logger, type LogLevel, logFormat } from "./Logger";

export const ConsoleLogger: Logger = {
  log: (level: LogLevel, message: string): void => {
    console.log(logFormat(level, message));
  },
};
