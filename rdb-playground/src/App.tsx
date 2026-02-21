import { useState, useRef, useCallback, useEffect } from 'react'
import { ConnectSQL } from '@edadma/rdb'
import './App.css'

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

function App() {
  const [sql, setSql] = useState(SAMPLE_SQL)
  const [results, setResults] = useState<ResultEntry[]>([])
  const [execTime, setExecTime] = useState<number | null>(null)
  const dbRef = useRef<any>(null)
  const textareaRef = useRef<HTMLTextAreaElement>(null)

  const getDb = useCallback(() => {
    if (!dbRef.current) dbRef.current = new ConnectSQL()
    return dbRef.current
  }, [])

  const resetDb = useCallback(() => {
    dbRef.current = new ConnectSQL()
    setResults([])
    setExecTime(null)
  }, [])

  const runSql = useCallback(() => {
    const db = getDb()
    const entries: ResultEntry[] = []
    const start = performance.now()

    try {
      const rawResults = db.execute(sql)
      for (const r of rawResults) {
        switch (r.command) {
          case 'select':
            entries.push({ type: 'table', content: r })
            break
          case 'insert':
            entries.push({ type: 'info', content: `INSERT — ${JSON.stringify(r.result)}` })
            break
          case 'update':
            entries.push({ type: 'info', content: `UPDATE — ${r.rows} row(s)` })
            break
          case 'delete':
            entries.push({ type: 'info', content: `DELETE — ${r.rows} row(s)` })
            break
          case 'create table':
            entries.push({ type: 'info', content: `CREATE TABLE ${r.table}` })
            break
          case 'drop table':
            entries.push({ type: 'info', content: `DROP TABLE ${r.table}` })
            break
          case 'truncate table':
            entries.push({ type: 'info', content: `TRUNCATE TABLE ${r.table}` })
            break
          case 'create index':
            entries.push({ type: 'info', content: `CREATE INDEX ${r.index}` })
            break
          case 'alter table':
            entries.push({ type: 'info', content: 'ALTER TABLE' })
            break
          default:
            entries.push({ type: 'info', content: r.command.toUpperCase() })
        }
      }
    } catch (e: any) {
      entries.push({ type: 'error', content: e.message || String(e) })
    }

    setExecTime(performance.now() - start)
    setResults(entries)
  }, [sql, getDb])

  useEffect(() => {
    const handler = (e: KeyboardEvent) => {
      if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') {
        e.preventDefault()
        runSql()
      }
    }
    window.addEventListener('keydown', handler)
    return () => window.removeEventListener('keydown', handler)
  }, [runSql])

  return (
    <div className="app">
      <div className="header">
        <h1>rdb playground</h1>
        <div className="header-actions">
          <button className="btn" onClick={resetDb}>Reset DB</button>
          <button className="btn btn-primary" onClick={runSql}>Run</button>
        </div>
      </div>

      <div className="main">
        <div className="panel">
          <div className="panel-header">
            <span>SQL</span>
            <span><span className="kbd">Ctrl</span> + <span className="kbd">Enter</span> to run</span>
          </div>
          <div className="editor-area">
            <textarea
              ref={textareaRef}
              value={sql}
              onChange={e => setSql(e.target.value)}
              placeholder="Enter SQL here..."
              spellCheck={false}
              autoFocus
            />
          </div>
        </div>

        <div className="divider" />

        <div className="panel">
          <div className="panel-header">
            <span>Results</span>
            {execTime !== null && <span>{execTime.toFixed(1)}ms</span>}
          </div>
          <div className="results-area">
            {results.length === 0 && (
              <span style={{ color: 'var(--text-muted)' }}>Run a query to see results</span>
            )}
            {results.map((r, i) => (
              <div key={i} className="result-block">
                {r.type === 'error' && <div className="result-error">{r.content}</div>}
                {r.type === 'info' && <div className="result-info">{r.content}</div>}
                {r.type === 'table' && (
                  <>
                    <table className="result-table">
                      <thead>
                        <tr>
                          {r.content.fields.map((f: any) => (
                            <th key={f.name}>{f.name}</th>
                          ))}
                        </tr>
                      </thead>
                      <tbody>
                        {r.content.rows.map((row: any, ri: number) => (
                          <tr key={ri}>
                            {r.content.fields.map((f: any) => (
                              <td key={f.name}>{formatValue(row[f.name])}</td>
                            ))}
                          </tr>
                        ))}
                      </tbody>
                    </table>
                    <div className="result-count">
                      {r.content.rows.length} row(s)
                    </div>
                  </>
                )}
              </div>
            ))}
          </div>
        </div>
      </div>

      <div className="status-bar">
        <span>@edadma/rdb — in-memory SQL database</span>
        <span>All queries run locally in your browser</span>
      </div>
    </div>
  )
}

export default App
