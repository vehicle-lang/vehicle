export enum LogLevel {
  Error = 0,
  Warning,
  Information,
  Debug,
  Trace,
}

export interface Logger {
  log(level: LogLevel, message: string): void;
}

export function logFormat(level: LogLevel, message: string): string {
  const currentTime = logGetCurrentTime();
  switch (level) {
    case LogLevel.Information:
      return `[Info  - ${currentTime}] ${message}`;
    case LogLevel.Warning:
      return `[Warn  - ${currentTime}] ${message}`;
    case LogLevel.Error:
      return `[Error - ${currentTime}] ${message}`;
    case LogLevel.Debug:
      return `[Debug - ${currentTime}] ${message}`;
    case LogLevel.Trace:
      return `[Trace - ${currentTime}] ${message}`;
  }
}

function logGetCurrentTime(): string {
  const now = new Date();
  const nowHours = now.getHours();
  const nowH = (nowHours > 12 ? nowHours - 12 : nowHours).toString();
  const nowMM = now.getMinutes().toString().padStart(2, "0");
  const nowss = now.getSeconds().toString().padStart(2, "0");
  const nowXM = nowHours > 12 ? "PM" : "AM";
  return `${nowH}:${nowMM}:${nowss} ${nowXM}`;
}
