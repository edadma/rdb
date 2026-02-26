package io.github.edadma.petradb.server

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

@js.native
trait IncomingMessage extends js.Object:
  val method: String  = js.native
  val url: String     = js.native
  val headers: js.Dictionary[String] = js.native
  def on(event: String, listener: js.Function1[js.Any, Unit]): Unit = js.native

@js.native
trait ServerResponse extends js.Object:
  def writeHead(statusCode: Int, headers: js.Dictionary[String]): Unit = js.native
  def end(data: Uint8Array): Unit   = js.native
  def end(data: String): Unit       = js.native
  def end(): Unit                    = js.native

@js.native
trait NodeHttpServer extends js.Object:
  def listen(port: Int, host: String, callback: js.Function0[Unit]): Unit = js.native
  def close(callback: js.Function0[Unit]): Unit = js.native
  def address(): js.Dynamic = js.native

@js.native
@JSImport("http", JSImport.Namespace)
object NodeHttp extends js.Object:
  def createServer(handler: js.Function2[IncomingMessage, ServerResponse, Unit]): NodeHttpServer = js.native

@js.native
@JSImport("buffer", "Buffer")
object NodeBuffer extends js.Object:
  def from(data: js.Array[Int]): Uint8Array = js.native
