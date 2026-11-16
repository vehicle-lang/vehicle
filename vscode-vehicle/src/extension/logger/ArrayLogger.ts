import { type Logger, type LogLevel } from "./Logger";

export class ArrayLogger implements Logger {
  readonly loggers: Logger[];

  constructor(loggers: Logger[]) {
    this.loggers = loggers;
  }

  log(level: LogLevel, message: string): void {
    this.loggers.forEach((logger) => logger.log(level, message));
  }
}
