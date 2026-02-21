import { useState, useRef, useCallback } from 'react'
import { ConnectSQL } from '@edadma/rdb'
import { Navbar, Button, Space, Table, Alert, Kbd, Flex, Badge, Splitter } from '@aster-ui/prefixed'
import { CodeEditor } from '@aster-ui/prefixed/codeeditor'
import { Terminal, type TerminalRef } from '@aster-ui/prefixed/terminal'
import type { EditorView } from '@codemirror/view'

interface ResultEntry {
  type: 'table' | 'info' | 'error'
  content: any
}

const SAMPLE_SQL = `-- Welcome to the RDB Playground!
-- Press Ctrl+Enter or click Run to execute.

CREATE TABLE employees (
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
  const data = rows.map((row: any) => colNames.map(n => formatValue(row[n])))
  const widths = colNames.map((name, ci) =>
    Math.max(name.length, ...data.map(r => r[ci].length))
  )
  const pad = (s: string, w: number) => s + ' '.repeat(w - s.length)
  const header = colNames.map((n, i) => pad(n, widths[i])).join(' | ')
  const sep = widths.map(w => '-'.repeat(w)).join('-+-')
  const body = data.map(r => r.map((c, i) => pad(c, widths[i])).join(' | ')).join('\r\n')
  return `${header}\r\n${sep}\r\n${body}`
}

function App() {
  const [results, setResults] = useState<ResultEntry[]>([])
  const [execTime, setExecTime] = useState<number | null>(null)
  const dbRef = useRef<any>(null)
  const editorViewRef = useRef<EditorView | null>(null)
  const terminalRef = useRef<TerminalRef>(null)

  const getDb = useCallback(() => {
    if (!dbRef.current) dbRef.current = new ConnectSQL()
    return dbRef.current
  }, [])

  const resetDb = useCallback(() => {
    dbRef.current = new ConnectSQL()
    setResults([])
    setExecTime(null)
    terminalRef.current?.writeln('\x1b[33mDatabase reset.\x1b[0m')
  }, [])

  const executeAndDisplay = useCallback((query: string) => {
    const db = getDb()
    const entries: ResultEntry[] = []
    const term = terminalRef.current
    const start = performance.now()

    try {
      const rawResults = db.execute(query)
      for (const r of rawResults) {
        switch (r.command) {
          case 'select':
            entries.push({ type: 'table', content: r })
            if (term) {
              term.writeln(`\x1b[36m${formatTableText(r.fields, r.rows)}\x1b[0m`)
              term.writeln(`\x1b[90m(${r.rows.length} row(s))\x1b[0m`)
            }
            break
          case 'insert':
            entries.push({ type: 'info', content: `INSERT — ${JSON.stringify(r.result)}` })
            term?.writeln(`\x1b[32mINSERT — ${JSON.stringify(r.result)}\x1b[0m`)
            break
          case 'update':
            entries.push({ type: 'info', content: `UPDATE — ${r.rows} row(s)` })
            term?.writeln(`\x1b[32mUPDATE — ${r.rows} row(s)\x1b[0m`)
            break
          case 'delete':
            entries.push({ type: 'info', content: `DELETE — ${r.rows} row(s)` })
            term?.writeln(`\x1b[32mDELETE — ${r.rows} row(s)\x1b[0m`)
            break
          case 'create table':
            entries.push({ type: 'info', content: `CREATE TABLE ${r.table}` })
            term?.writeln(`\x1b[32mCREATE TABLE ${r.table}\x1b[0m`)
            break
          case 'drop table':
            entries.push({ type: 'info', content: `DROP TABLE ${r.table}` })
            term?.writeln(`\x1b[32mDROP TABLE ${r.table}\x1b[0m`)
            break
          case 'truncate table':
            entries.push({ type: 'info', content: `TRUNCATE TABLE ${r.table}` })
            term?.writeln(`\x1b[32mTRUNCATE TABLE ${r.table}\x1b[0m`)
            break
          case 'create index':
            entries.push({ type: 'info', content: `CREATE INDEX ${r.index}` })
            term?.writeln(`\x1b[32mCREATE INDEX ${r.index}\x1b[0m`)
            break
          case 'alter table':
            entries.push({ type: 'info', content: 'ALTER TABLE' })
            term?.writeln('\x1b[32mALTER TABLE\x1b[0m')
            break
          default:
            entries.push({ type: 'info', content: r.command.toUpperCase() })
            term?.writeln(`\x1b[32m${r.command.toUpperCase()}\x1b[0m`)
        }
      }
    } catch (e: any) {
      const msg = e.message || String(e)
      entries.push({ type: 'error', content: msg })
      term?.writeln(`\x1b[31mERROR: ${msg}\x1b[0m`)
    }

    const elapsed = performance.now() - start
    setExecTime(elapsed)
    setResults(entries)
    term?.writeln(`\x1b[90m(${elapsed.toFixed(1)}ms)\x1b[0m`)
  }, [getDb])

  const runSql = useCallback(() => {
    const view = editorViewRef.current
    if (!view) return
    executeAndDisplay(view.state.doc.toString())
  }, [executeAndDisplay])

  const handleTerminalLine = useCallback((line: string) => {
    const trimmed = line.trim()
    if (!trimmed) return
    if (trimmed.toLowerCase() === 'clear') {
      terminalRef.current?.clear()
      return
    }
    if (trimmed.toLowerCase() === 'reset') {
      resetDb()
      return
    }
    executeAndDisplay(trimmed)
  }, [executeAndDisplay, resetDb])

  return (
    <Flex direction="column" className="h-screen" data-theme="dark">
      <Navbar
        color="neutral"
        start={<span className="font-bold text-lg">rdb playground</span>}
        end={
          <Space>
            <Button variant="outline" size="sm" onClick={resetDb}>Reset DB</Button>
            <Button color="primary" size="sm" onClick={runSql}>Run</Button>
          </Space>
        }
      />

      <div className="flex-1 min-h-0">
        <Splitter direction="horizontal" defaultSizes={[50, 50]} gutterSize={6}>
          {/* Left: Editor + Terminal */}
          <Splitter.Panel>
            <Splitter direction="vertical" defaultSizes={[70, 30]} gutterSize={6}>
              {/* Editor */}
              <Splitter.Panel>
                <Flex direction="column" className="h-full">
                  <Flex justify="between" align="center" className="px-3 py-1.5 bg-base-200 text-xs uppercase tracking-wider text-base-content/50 shrink-0">
                    <span>SQL Editor</span>
                    <Space size="xs">
                      <Kbd size="xs">Ctrl</Kbd>
                      <span>+</span>
                      <Kbd size="xs">Enter</Kbd>
                      <span>to run</span>
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
                      minHeight="100%"
                      onEditorReady={(view) => { editorViewRef.current = view }}
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
                      prompt="sql> "
                      onLine={handleTerminalLine}
                      onReady={(term) => {
                        term.writeln('RDB interactive terminal. Type SQL to execute.')
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
        <span>@edadma/rdb — in-memory SQL database</span>
        <span>All queries run locally in your browser</span>
      </Flex>
    </Flex>
  )
}

export default App
