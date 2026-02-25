package io.github.edadma.petradb.cli

import io.github.edadma.petradb.Session
import scala.scalajs.js
import scala.scalajs.js.annotation.*

@js.native
@JSImport("readline", JSImport.Namespace)
private object NodeReadlineModule extends js.Object:
  def createInterface(options: js.Dynamic): NodeReadlineInterface = js.native

@js.native
private trait NodeReadlineInterface extends js.Object:
  def question(query: String, callback: js.Function1[String, Unit]): Unit = js.native
  def close(): Unit = js.native

class PlatformRepl(session: Session) extends Repl(session):
  private lazy val rl = NodeReadlineModule.createInterface(
    js.Dynamic.literal(
      input = js.Dynamic.global.process.stdin,
      output = js.Dynamic.global.process.stdout,
    )
  )

  private var buffer = new StringBuilder
  private var collecting = false

  def run(): Unit =
    askLine(prompt)

  private def askLine(p: String): Unit =
    rl.question(p, { (answer: String) =>
      val line = answer.asInstanceOf[String]
      val trimmed = line.trim

      if collecting then
        buffer.append("\n").append(line)
        if trimmed.endsWith(";") then
          collecting = false
          executeSql(buffer.toString)
          buffer.clear()
          askLine(prompt)
        else
          askLine(contPrompt)
      else if trimmed.isEmpty then
        askLine(prompt)
      else if trimmed.startsWith("\\") then
        if handleMeta(trimmed) then askLine(prompt)
        else rl.close()
      else if trimmed.endsWith(";") then
        executeSql(trimmed)
        askLine(prompt)
      else
        collecting = true
        buffer.clear()
        buffer.append(trimmed)
        askLine(contPrompt)
    }: js.Function1[String, Unit])
