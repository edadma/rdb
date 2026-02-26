package io.github.edadma.petradb.server

object PasswordHash:
  private val Iterations = 100000
  private val SaltLength = 16
  private val KeyLength  = 32

  def hashPassword(password: String): String =
    val salt = PlatformCrypto.randomBytes(SaltLength)
    val hash = PlatformCrypto.pbkdf2(password, salt, Iterations, KeyLength)
    s"pbkdf2$$${Iterations}$$${PlatformCrypto.base64Encode(salt)}$$${PlatformCrypto.base64Encode(hash)}"

  def verifyPassword(password: String, stored: String): Boolean =
    stored.split("\\$", 4) match
      case Array("pbkdf2", iterStr, saltB64, hashB64) =>
        val iterations = iterStr.toInt
        val salt = PlatformCrypto.base64Decode(saltB64)
        val expectedHash = PlatformCrypto.base64Decode(hashB64)
        val computedHash = PlatformCrypto.pbkdf2(password, salt, iterations, expectedHash.length)
        constantTimeEquals(expectedHash, computedHash)
      case _ => false

  private def constantTimeEquals(a: Array[Byte], b: Array[Byte]): Boolean =
    if a.length != b.length then return false
    var result = 0
    var i = 0
    while i < a.length do
      result |= (a(i) ^ b(i))
      i += 1
    result == 0
