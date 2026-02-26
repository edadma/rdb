package io.github.edadma.petradb.server

import scala.scalajs.js
import scala.scalajs.js.annotation.*
import scala.scalajs.js.typedarray.Uint8Array

@js.native
@JSImport("crypto", JSImport.Namespace)
private object NodeCrypto extends js.Object:
  def pbkdf2Sync(password: String, salt: Uint8Array, iterations: Int, keylen: Int, digest: String): NodeJSBuffer =
    js.native
  def randomBytes(size: Int): NodeJSBuffer = js.native

@js.native
@JSImport("buffer", "Buffer")
private object NodeJSBufferObj extends js.Object:
  def from(data: js.Array[Int]): NodeJSBuffer       = js.native
  def from(data: String, encoding: String): NodeJSBuffer = js.native

@js.native
private trait NodeJSBuffer extends Uint8Array:
  def toString(encoding: String): String = js.native

object PlatformCrypto:
  private def toByteArray(buf: Uint8Array): Array[Byte] =
    val len = buf.length
    val arr = new Array[Byte](len)
    var i = 0
    while i < len do
      arr(i) = buf(i).toByte
      i += 1
    arr

  private def toJSBuffer(bytes: Array[Byte]): NodeJSBuffer =
    NodeJSBufferObj.from(js.Array(bytes.map(_.toInt & 0xff)*))

  def pbkdf2(password: String, salt: Array[Byte], iterations: Int, keyLength: Int): Array[Byte] =
    val saltBuf = toJSBuffer(salt)
    val result = NodeCrypto.pbkdf2Sync(password, saltBuf, iterations, keyLength, "sha256")
    toByteArray(result)

  def randomBytes(length: Int): Array[Byte] =
    toByteArray(NodeCrypto.randomBytes(length))

  def base64Encode(bytes: Array[Byte]): String =
    toJSBuffer(bytes).toString("base64")

  def base64Decode(s: String): Array[Byte] =
    toByteArray(NodeJSBufferObj.from(s, "base64"))
