export interface SessionOptions {
  host?: string;
  port?: number;
  rowMode?: "object" | "array";
}

export interface ExecuteOptions {
  rowMode?: "object" | "array";
}

export interface FieldInfo {
  name: string;
  dataType: string;
}

export interface ExecuteResult {
  command: string;
  [key: string]: any;
}

export class Session {
  private baseUrl: string;
  private defaultRowMode: string;
  private sessionId: string | null = null;

  constructor(options: SessionOptions = {}) {
    const host = options.host ?? "localhost";
    const port = options.port ?? 5432;
    this.baseUrl = `http://${host}:${port}`;
    this.defaultRowMode = options.rowMode ?? "object";
  }

  async execute(
    sql: string,
    options?: ExecuteOptions,
  ): Promise<ExecuteResult[]> {
    const rowMode = options?.rowMode ?? this.defaultRowMode;
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };
    if (this.sessionId) {
      headers["X-Session-Id"] = this.sessionId;
    }

    const res = await fetch(`${this.baseUrl}/sql`, {
      method: "POST",
      headers,
      body: JSON.stringify({ sql, rowMode }),
    });

    if (!res.ok) {
      const body = await res.text();
      throw new Error(body || `HTTP ${res.status}`);
    }

    return (await res.json()) as ExecuteResult[];
  }

  async connect(): Promise<void> {
    const res = await fetch(`${this.baseUrl}/session`, {
      method: "POST",
    });

    if (!res.ok) {
      const body = await res.text();
      throw new Error(body || `HTTP ${res.status}`);
    }

    const data = (await res.json()) as { sessionId: string };
    this.sessionId = data.sessionId;
  }

  async close(): Promise<void> {
    if (!this.sessionId) return;

    const res = await fetch(`${this.baseUrl}/session/${this.sessionId}`, {
      method: "DELETE",
    });

    if (!res.ok) {
      const body = await res.text();
      throw new Error(body || `HTTP ${res.status}`);
    }

    this.sessionId = null;
  }
}
