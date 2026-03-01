import { useState, useRef, useCallback } from 'react'
import { Session } from '@petradb/engine'
import { TextTable } from '@edadma/table'
import { Navbar, Button, Space, Table, Alert, Flex, Badge, Splitter } from '@aster-ui/prefixed'
import { CodeEditor } from '@aster-ui/prefixed/codeeditor'
import { Terminal, type TerminalRef } from '@aster-ui/prefixed/terminal'
import { EditorView, type Extension } from '@codemirror/view'

const editorTheme = EditorView.theme({
  '.cm-gutters': {
    backgroundColor: 'oklch(var(--b3))',
    borderRight: '2px solid oklch(var(--bc) / 0.2)',
    paddingRight: '4px',
  },
  '.cm-lineNumbers .cm-gutterElement': {
    fontSize: '0.75em',
    opacity: '0.4',
    padding: '0 12px 0 8px',
  },
  '.cm-content': {
    outline: 'none',
  },
  '&.cm-editor.cm-focused': {
    outline: 'none',
  },
})

const EDITOR_EXTENSIONS: Extension[] = [editorTheme]

interface ResultEntry {
  type: 'table' | 'info' | 'error'
  content: any
}

const SAMPLE_SQL = `CREATE TABLE employees (
  id SERIAL,
  name TEXT NOT NULL,
  department TEXT,
  salary INT,
  PRIMARY KEY (id)
  );

INSERT INTO employees (name, department, salary) VALUES
  ('Alice', 'Engineering', 95000),
  ('Bob', 'Sales', 72000),
  ('Charlie', 'Engineering', 88000),
  ('Diana', 'Marketing', 67000),
  ('Eve', 'Engineering', 102000);

SELECT department, COUNT(*) AS count, AVG(salary) AS avg_salary
  FROM employees
  GROUP BY department
  ORDER BY avg_salary DESC;
`

function formatValue(v: any): string {
  if (v === null || v === undefined) return 'NULL'
  if (v instanceof Date) return v.toISOString()
  if (typeof v === 'object') return JSON.stringify(v)
  return String(v)
}

function formatTableText(fields: any[], rows: any[]): string {
  const colNames = fields.map((f: any) => f.name)
  const t = new TextTable()
  t.header(colNames)
  for (const row of rows) {
    t.row(colNames.map(n => formatValue(row[n])))
  }
  return t.render().trimEnd()
}

function App() {
  const [results, setResults] = useState<ResultEntry[]>([])
  const [execTime, setExecTime] = useState<number | null>(null)
  const dbRef = useRef<any>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const terminalRef = useRef<TerminalRef>(null)

  const handleEditorReady = useCallback((view: EditorView) => {
    editorViewRef.current = view
  }, [])

  const getDb = useCallback(() => {
    if (!dbRef.current) dbRef.current = new Session()
    return dbRef.current
  }, [])

  const resetDb = useCallback(() => {
    dbRef.current = new Session()
    setResults([])
    setExecTime(null)
    terminalRef.current?.writeln('\x1b[33mDatabase reset.\x1b[0m')
  }, [])

  const resultLabel = useCallback((r: any): string => {
    switch (r.command) {
      case 'insert': return `INSERT — ${JSON.stringify(r.result)}`
      case 'update': return `UPDATE — ${r.rowCount} row(s)`
      case 'delete': return `DELETE — ${r.rowCount} row(s)`
      case 'create table': return `CREATE TABLE ${r.table}`
      case 'drop table': return `DROP TABLE ${r.table}`
      case 'truncate table': return `TRUNCATE TABLE ${r.table}`
      case 'create index': return `CREATE INDEX ${r.index}`
      case 'alter table': return 'ALTER TABLE'
      default: return r.command.toUpperCase()
    }
  }, [])

  const runSql = useCallback(async () => {
    const view = editorViewRef.current
    if (!view) return
    const db = getDb()
    const entries: ResultEntry[] = []
    const start = performance.now()

    try {
      const rawResults = await db.execute(view.state.doc.toString())
      for (const r of rawResults) {
        if (r.command === 'select') {
          entries.push({ type: 'table', content: r })
        } else {
          entries.push({ type: 'info', content: resultLabel(r) })
        }
      }
    } catch (e: any) {
      entries.push({ type: 'error', content: e.message || String(e) })
    }

    setExecTime(performance.now() - start)
    setResults(entries)
  }, [getDb, resultLabel])

  const handleTerminalLine = useCallback((line: string) => {
    const trimmed = line.trim()
    if (!trimmed) return
    const term = terminalRef.current
    if (!term) return

    if (trimmed.toLowerCase() === 'clear') { term.clear(); return }
    if (trimmed.toLowerCase() === 'reset') { resetDb(); return }

    const db = getDb()
    const start = performance.now()

    return db.execute(trimmed).then((rawResults: any[]) => {
      for (const r of rawResults) {
        if (r.command === 'select') {
          for (const line of formatTableText(r.fields, r.rows).split('\n')) {
            term.writeln(line)
          }
          term.writeln(`\x1b[90m(${r.rows.length} row(s))\x1b[0m`)
        } else {
          term.writeln(`\x1b[32m${resultLabel(r)}\x1b[0m`)
        }
      }
      term.writeln(`\x1b[90m(${(performance.now() - start).toFixed(1)}ms)\x1b[0m`)
    }).catch((e: any) => {
      term.writeln(`\x1b[31mERROR: ${e.message || String(e)}\x1b[0m`)
    })
  }, [getDb, resetDb, resultLabel])

  return (
    <Flex direction="column" className="h-screen" data-theme="dark">
      <Navbar
        color="neutral"
        start={<span className="font-bold text-lg">PetraDB Playground</span>}
      />

      <div className="flex-1 min-h-0">
        <Splitter direction="horizontal" defaultSizes={[50, 50]} gutterSize={6}>
          {/* Left: Editor + Terminal */}
          <Splitter.Panel>
            <Splitter direction="vertical" defaultSizes={[70, 30]} gutterSize={6}>
              {/* Editor */}
              <Splitter.Panel>
                <Flex direction="column" className="h-full">
                  <Flex justify="between" align="center" className="px-3 py-0.5 bg-base-200 text-xs uppercase tracking-wider text-base-content/50 shrink-0">
                    <span>SQL Editor</span>
                    <Space size="xs">
                      <Button variant="outline" size="xs" onClick={resetDb}>Reset DB</Button>
                      <Button color="primary" size="xs" onClick={runSql}>Run</Button>
                    </Space>
                  </Flex>
                  <div className="flex-1 min-h-0 overflow-hidden">
                    <CodeEditor
                      language="sql"
                      value={SAMPLE_SQL}
                      autoFocus
                      bordered={false}
                      lineNumbers
                      foldGutter={false}
                      className="h-full"
                      extensions={EDITOR_EXTENSIONS}
                      onEditorReady={handleEditorReady}
                    />
                  </div>
                </Flex>
              </Splitter.Panel>

              {/* Terminal */}
              <Splitter.Panel>
                <Flex direction="column" className="h-full">
                  <Flex justify="between" align="center" className="px-3 py-1.5 bg-base-200 text-xs uppercase tracking-wider text-base-content/50 shrink-0">
                    <span>Terminal</span>
                    <span className="normal-case tracking-normal">type SQL directly, or "clear" / "reset"</span>
                  </Flex>
                  <div className="flex-1 min-h-0">
                    <Terminal
                      ref={terminalRef}
                      readline
                      prompt="petra> "
                      onLine={handleTerminalLine}
                      onReady={(term) => {
                        term.attachCustomKeyEventHandler((e: KeyboardEvent) => {
                          if ((e.ctrlKey || e.metaKey) && e.key === 'v') return false
                          return true
                        })
                        term.writeln('PetraDB interactive terminal. Type SQL to execute.')
                        term.writeln('Commands: \x1b[36mclear\x1b[0m, \x1b[36mreset\x1b[0m')
                        term.writeln('')
                      }}
                      className="h-full"
                    />
                  </div>
                </Flex>
              </Splitter.Panel>
            </Splitter>
          </Splitter.Panel>

          {/* Right: Results */}
          <Splitter.Panel>
            <Flex direction="column" className="h-full">
              <Flex justify="between" align="center" className="px-3 py-1.5 bg-base-200 text-xs uppercase tracking-wider text-base-content/50 shrink-0">
                <span>Results</span>
                {execTime !== null && (
                  <Badge size="sm" variant="ghost">{execTime.toFixed(1)}ms</Badge>
                )}
              </Flex>
              <div className="flex-1 overflow-auto p-3">
                {results.length === 0 && (
                  <span className="text-base-content/30">Run a query to see results</span>
                )}
                <Flex direction="column" gap="sm">
                  {results.map((r, i) => {
                    if (r.type === 'error') {
                      return <Alert key={i} type="error">{r.content}</Alert>
                    }
                    if (r.type === 'info') {
                      return <Alert key={i} type="success" soft>{r.content}</Alert>
                    }
                    const columns = r.content.fields.map((f: any) => ({
                      key: f.name,
                      title: f.name,
                      dataIndex: f.name,
                      render: (v: any) => formatValue(v),
                    }))
                    const dataSource = r.content.rows.map((row: any, ri: number) => ({
                      ...row,
                      _key: ri,
                    }))
                    return (
                      <div key={i}>
                        <Table
                          columns={columns}
                          dataSource={dataSource}
                          rowKey="_key"
                          size="sm"
                          bordered
                          hoverable
                          striped
                          pagination={false}
                        />
                        <span className="text-xs text-base-content/40 mt-1 inline-block">
                          {r.content.rows.length} row(s)
                        </span>
                      </div>
                    )
                  })}
                </Flex>
              </div>
            </Flex>
          </Splitter.Panel>
        </Splitter>
      </div>

      <Flex justify="between" align="center" className="px-3 py-1 bg-base-200 text-xs text-base-content/40 shrink-0">
        <span>@petradb/engine — in-memory SQL database</span>
        <span>All queries run locally in your browser</span>
      </Flex>
    </Flex>
  )
}

export default App
